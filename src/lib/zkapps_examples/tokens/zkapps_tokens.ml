open Core_kernel
open Snark_params.Tick.Run
open Signature_lib
open Mina_base
open Pickles_types

(** Circuit requests, to get values and run code outside of the snark. *)
type _ Snarky_backendless.Request.t +=
  | Public_key : Public_key.Compressed.t Snarky_backendless.Request.t
  | Token_id : Token_id.t Snarky_backendless.Request.t
  | Amount_to_mint : Currency.Amount.t Snarky_backendless.Request.t
  | Mint_to_public_key : Public_key.Compressed.t Snarky_backendless.Request.t

module Rules = struct
  (** Rule to initialize the zkApp.

      Asserts that the state was not last updated by a proof (ie. the zkApp is
      freshly deployed, or that the state was modified -- tampered with --
      without using a proof).
      The app state is set to the initial state.
  *)
  module Initialize_state = struct
    let initial_state = lazy (List.init 8 ~f:(fun _ -> Field.Constant.zero))

    let handler (public_key : Public_key.Compressed.t) (token_id : Token_id.t)
        (Snarky_backendless.Request.With { request; respond }) =
      match request with
      | Public_key ->
          respond (Provide public_key)
      | Token_id ->
          respond (Provide token_id)
      | _ ->
          respond Unhandled

    let main input =
      let public_key =
        exists Public_key.Compressed.typ ~request:(fun () -> Public_key)
      in
      let token_id = exists Token_id.typ ~request:(fun () -> Token_id) in
      Zkapps_examples.wrap_main ~public_key ~token_id
        (fun account_update ->
          let initial_state =
            List.map ~f:Field.constant (Lazy.force initial_state)
          in
          account_update#assert_state_unproved ;
          account_update#set_full_state initial_state )
        input

    let rule : _ Pickles.Inductive_rule.t =
      { identifier = "Initialize zkapp"; prevs = []; main; uses_lookup = false }
  end

  (** Rule to mint tokens. *)
  module Mint = struct
    let handler ~(owner_public_key : Public_key.Compressed.t)
        ~(owner_token_id : Token_id.t) ~(amount : Currency.Amount.t)
        ~(mint_to_public_key : Public_key.Compressed.t)
        (Snarky_backendless.Request.With { request; respond }) =
      match request with
      | Public_key ->
          respond (Provide owner_public_key)
      | Token_id ->
          respond (Provide owner_token_id)
      | Amount_to_mint ->
          respond (Provide amount)
      | Mint_to_public_key ->
          respond (Provide mint_to_public_key)
      | _ ->
          respond Unhandled

    let main input =
      let public_key =
        exists Public_key.Compressed.typ ~request:(fun () -> Public_key)
      in
      let token_id = exists Token_id.typ ~request:(fun () -> Token_id) in
      Zkapps_examples.wrap_main ~public_key ~token_id
        (fun account_update ->
          let amount_to_mint =
            exists Currency.Amount.typ ~request:(fun () -> Amount_to_mint)
          in
          let destination_pk =
            exists Public_key.Compressed.typ ~request:(fun () ->
                Mint_to_public_key )
          in
          let self_token =
            Account_id.Checked.derive_token_id
              ~owner:(Account_id.Checked.create public_key token_id)
          in
          let destination_account_update, destination_account_calls =
            let account_update =
              new Zkapps_examples.account_update
                ~public_key:destination_pk ~token_id:self_token
                ~caller:self_token
            in
            account_update#set_balance_change
              Currency.Amount.Signed.Checked.(of_unsigned amount_to_mint) ;
            account_update#set_authorization_kind
              { is_proved = Boolean.false_; is_signed = Boolean.false_ } ;
            let final_update, calls =
              account_update#account_update_under_construction
              |> Zkapps_examples.Account_update_under_construction.In_circuit
                 .to_account_update_and_calls
            in
            let digest =
              Zkapp_command.Digest.Account_update.Checked.create final_update
            in
            ( { Zkapp_call_forest.Checked.account_update =
                  { data = final_update; hash = digest }
              ; control = Prover_value.create (fun () -> Control.None_given)
              }
            , calls )
          in
          account_update#register_call destination_account_update
            destination_account_calls ;
          account_update#add_sequence_events
            [ [| Currency.Amount.Checked.to_field amount_to_mint |] ] ;
          account_update#assert_state_proved )
        input

    let rule : _ Pickles.Inductive_rule.t =
      { identifier = "Mint token"; prevs = []; main; uses_lookup = false }
  end

  (** Rule to transfer tokens. *)
  module Transfer = struct
    let handler (public_key : Public_key.Compressed.t) (token_id : Token_id.t)
        (Snarky_backendless.Request.With { request; respond }) =
      match request with
      | Public_key ->
          respond (Provide public_key)
      | Token_id ->
          respond (Provide token_id)
      | _ ->
          respond Unhandled

    let dummy_account_update : Zkapp_call_forest.Checked.account_update Lazy.t =
      lazy
        (let dummy_body = Account_update.Body.dummy in
         { account_update =
             { data = constant (Account_update.Body.typ ()) dummy_body
             ; hash =
                 constant Zkapp_command.Digest.Account_update.typ
                   (Zkapp_command.Digest.Account_update.create_body dummy_body)
             }
         ; control = Prover_value.create (fun () -> Control.None_given)
         } )

    let next_account_update () = ((), Lazy.force dummy_account_update)

    let skip_subtree_if _skip () = ()

    let check_children ~self_token ~running_total ~state n =
      let state = ref state in
      let running_total = ref running_total in
      let consume_account_update () =
        let next_state, account_update = next_account_update !state in
        state := next_state ;
        let can_access_this_token =
          Token_id.Checked.equal self_token
            account_update.account_update.data.caller
        in
        let using_this_token =
          Token_id.Checked.equal self_token
            account_update.account_update.data.token_id
        in
        let amount =
          if_ using_this_token ~typ:Field.typ
            ~then_:
              ( account_update.account_update.data.balance_change
              |> Currency.Amount.Signed.Checked.to_field_var |> run_checked )
            ~else_:Field.zero
        in
        running_total := Field.( + ) !running_total amount ;
        state := skip_subtree_if (Boolean.not can_access_this_token) !state
      in
      for _i = 0 to n do
        consume_account_update ()
      done ;
      (!state, !running_total)

    let state_is_empty () = Boolean.true_

    let dummy_proof =
      lazy (Pickles.Proof.dummy Nat.N2.n Nat.N2.n Nat.N2.n ~domain_log2:15)

    module Recursive = struct
      module Statement = struct
        module Value = struct
          type t =
            { state : unit
            ; self_token : Token_id.t
            ; running_total : Field.Constant.t
            }
          [@@deriving hlist]
        end

        module Circuit = struct
          type t =
            { state : unit
            ; self_token : Token_id.Checked.t
            ; running_total : Field.t
            }
          [@@deriving hlist]
        end

        let typ =
          Typ.of_hlistable
            [ Typ.unit; Token_id.typ; Field.typ ]
            ~var_to_hlist:Circuit.to_hlist ~var_of_hlist:Circuit.of_hlist
            ~value_to_hlist:Value.to_hlist ~value_of_hlist:Value.of_hlist
      end

      let main
          { Pickles.Inductive_rule.public_input =
              ({ state; self_token; running_total } : Statement.Circuit.t)
          } =
        let state, running_total =
          check_children ~self_token ~running_total ~state 3
        in
        let recursive_input =
          { Statement.Circuit.state; self_token; running_total }
        in
        let proof =
          exists (Typ.Internal.ref ()) ~compute:(fun () ->
              Lazy.force dummy_proof )
        in
        { Pickles.Inductive_rule.previous_proof_statements =
            [ { public_input = recursive_input
              ; proof
              ; proof_must_verify = Boolean.not (state_is_empty state)
              }
            ; (* dummy to avoid pickles bug *)
              { public_input = recursive_input
              ; proof =
                  exists (Typ.Internal.ref ()) ~compute:(fun () ->
                      Lazy.force dummy_proof )
              ; proof_must_verify = Boolean.false_
              }
            ]
        ; public_output = ()
        ; auxiliary_output = ()
        }

      let rule self : _ Pickles.Inductive_rule.t =
        { identifier = "Transfer tokens"
        ; prevs = [ self; self ]
        ; main
        ; uses_lookup = false
        }
    end

    let main input =
      let public_key =
        exists Public_key.Compressed.typ ~request:(fun () -> Public_key)
      in
      let token_id = exists Token_id.typ ~request:(fun () -> Token_id) in
      let { Pickles.Inductive_rule.previous_proof_statements = _
          ; public_output = account_update
          ; auxiliary_output = recursive_input
          } =
        Zkapps_examples.wrap_main ~public_key ~token_id
          (fun account_update ->
            let self_token =
              Account_id.Checked.derive_token_id
                ~owner:(Account_id.Checked.create public_key token_id)
            in
            (* Accumulators *)
            let state, running_total =
              check_children ~self_token ~running_total:Field.zero ~state:() 3
            in
            account_update#assert_state_proved ;
            { Recursive.Statement.Circuit.state; self_token; running_total } )
          input
      in
      let proof =
        exists (Typ.Internal.ref ()) ~compute:(fun () ->
            Lazy.force dummy_proof )
      in
      { Pickles.Inductive_rule.previous_proof_statements =
          [ { public_input = recursive_input
            ; proof
            ; proof_must_verify =
                Boolean.not (state_is_empty recursive_input.state)
            }
          ; (* dummy to avoid pickles bug *)
            { public_input = recursive_input
            ; proof =
                exists (Typ.Internal.ref ()) ~compute:(fun () ->
                    Lazy.force dummy_proof )
            ; proof_must_verify = Boolean.false_
            }
          ]
      ; public_output = account_update
      ; auxiliary_output = ()
      }

    let rule prev : _ Pickles.Inductive_rule.t =
      { identifier = "Transfer tokens"
      ; prevs = [ prev; prev ]
      ; main
      ; uses_lookup = false
      }
  end
end

module Transfer_recursive = struct
  let lazy_compiled =
    lazy
      (Pickles.compile () ~cache:Cache_dir.cache
         ~public_input:(Input Rules.Transfer.Recursive.Statement.typ)
         ~auxiliary_typ:Impl.Typ.unit
         ~branches:(module Nat.N1)
         ~max_proofs_verified:(module Nat.N2)
         ~name:"transfer recurse"
         ~constraint_constants:
           Genesis_constants.Constraint_constants.(
             to_snark_keys_header compiled)
         ~choices:(fun ~self -> [ Rules.Transfer.Recursive.rule self ]) )

  let tag = Lazy.map lazy_compiled ~f:(fun (tag, _, _, _) -> tag)
end

let lazy_compiled =
  lazy
    (Zkapps_examples.compile () ~cache:Cache_dir.cache
       ~auxiliary_typ:Impl.Typ.unit
       ~branches:(module Nat.N3)
       ~max_proofs_verified:(module Nat.N2)
       ~name:"tokens"
       ~constraint_constants:
         Genesis_constants.Constraint_constants.(to_snark_keys_header compiled)
       ~choices:(fun ~self:_ ->
         [ Rules.Initialize_state.rule
         ; Rules.Mint.rule
         ; Rules.Transfer.rule (Lazy.force Transfer_recursive.tag)
         ] ) )

let compile () = ignore (Lazy.force lazy_compiled : _)

let tag = Lazy.map lazy_compiled ~f:(fun (tag, _, _, _) -> tag)

let vk = Lazy.map ~f:Pickles.Side_loaded.Verification_key.of_compiled tag

let p_module = Lazy.map lazy_compiled ~f:(fun (_, _, p_module, _) -> p_module)

module P = struct
  type statement = Zkapp_statement.t

  type t = (Nat.N2.n, Nat.N2.n) Pickles.Proof.t

  module type Proof_intf =
    Pickles.Proof_intf with type statement = statement and type t = t

  let verification_key =
    Lazy.bind p_module ~f:(fun (module P : Proof_intf) -> P.verification_key)

  let id = Lazy.bind p_module ~f:(fun (module P : Proof_intf) -> P.id)

  let verify statements =
    let module P : Proof_intf = (val Lazy.force p_module) in
    P.verify statements

  let verify_promise statements =
    let module P : Proof_intf = (val Lazy.force p_module) in
    P.verify_promise statements
end

let initialize_prover =
  Lazy.map lazy_compiled
    ~f:(fun (_, _, _, Pickles.Provers.[ initialize_prover; _; _ ]) ->
      initialize_prover )

let initialize public_key token_id =
  let initialize_prover = Lazy.force initialize_prover in
  initialize_prover
    ~handler:(Rules.Initialize_state.handler public_key token_id)

let mint_prover =
  Lazy.map lazy_compiled
    ~f:(fun (_, _, _, Pickles.Provers.[ _; mint_prover; _ ]) -> mint_prover)

let mint ~owner_public_key ~owner_token_id ~amount ~mint_to_public_key =
  let mint_prover = Lazy.force mint_prover in
  mint_prover
    ~handler:
      (Rules.Mint.handler ~owner_public_key ~owner_token_id ~amount
         ~mint_to_public_key )

let child_forest_prover =
  Lazy.map lazy_compiled
    ~f:(fun (_, _, _, Pickles.Provers.[ _; _; child_forest_prover ]) ->
      child_forest_prover )

let child_forest public_key token_id =
  let child_forest_prover = Lazy.force child_forest_prover in
  child_forest_prover ~handler:(Rules.Transfer.handler public_key token_id)
