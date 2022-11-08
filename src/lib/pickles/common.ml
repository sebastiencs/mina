open Core_kernel
open Pickles_types
open Import
open Backend

module Max_degree = struct
  let step_log2 = Nat.to_int Backend.Tick.Rounds.n

  let step = 1 lsl step_log2

  let wrap_log2 = Nat.to_int Backend.Tock.Rounds.n

  let wrap = 1 lsl wrap_log2
end

let tick_shifts, tock_shifts =
  let mk g =
    let f =
      Memo.general ~cache_size_bound:20 ~hashable:Int.hashable (fun log2_size ->
          g log2_size )
    in
    fun ~log2_size -> f log2_size
  in
  ( mk Kimchi_bindings.Protocol.VerifierIndex.Fp.shifts
  , mk Kimchi_bindings.Protocol.VerifierIndex.Fq.shifts )

let wrap_domains ~proofs_verified =
  let h =
    match proofs_verified with 0 -> 13 | 1 -> 14 | 2 -> 15 | _ -> assert false
  in
  { Domains.h = Pow_2_roots_of_unity h }

let old_challenges_to_string (vec : (Pasta_bindings.Fp.t, 'a) Vector.vec) =
  String.concat ~sep:"\n"
    (List.cons
       ("length=" ^ string_of_int (Nat.to_int (Vector.length vec)))
       (Vector.to_list
          (Vector.map vec ~f:(fun a -> Pasta_bindings.Fp.to_string a)) ) )

let curve_to_string (a, b) =
  Pasta_bindings.Fp.to_string a ^ "," ^ Pasta_bindings.Fp.to_string b

let curve_vec_to_string
    (curve_vec : (Pasta_bindings.Fp.t * Pasta_bindings.Fp.t, 'a) Vector.vec) =
  String.concat ~sep:"\n"
    (List.cons
       ("length=" ^ string_of_int (Nat.to_int (Vector.length curve_vec)))
       (Vector.to_list (Vector.map curve_vec ~f:curve_to_string)) )

(* { sigma_comm : 'comm Plonk_types.Permuts_vec.Stable.V1.t *)
(* ; coefficients_comm : 'comm Plonk_types.Columns_vec.Stable.V1.t *)
(* ; generic_comm : 'comm *)
(* ; psm_comm : 'comm *)
(* ; complete_add_comm : 'comm *)
(* ; mul_comm : 'comm *)
(* ; emul_comm : 'comm *)
(* ; endomul_scalar_comm : 'comm *)
(* } *)

(* app_state: Mina_state.Protocol_state.Value.Stable.Latest.t *)
(* Plonk_verification_key_evals.Stable.Latest.t *)
module Concrete = struct
  type 'app_state t =
    ( Tock.Curve.Affine.t
    , 'app_state
    , Tock.Curve.Affine.t Vector.Vector_2.Stable.Latest.t
    , Tick.Field.t Vector.Vector_16.Stable.Latest.t
      Vector.Vector_2.Stable.Latest.t )
    Types.Wrap.Messages_for_next_step_proof.t
  [@@deriving bin_io_unversioned, sexp]
end

external rust_get_random_message : unit -> bytes = "rust_get_random_message"

external rust_hash_message_for_next_step_proof : bytes -> string list -> unit
  = "rust_hash_message_for_next_step_proof"

let compare_hash_messages_for_next_step_proof ?get_binprot_helpers_app_state
    ~app_state =
  let g (x, y) = [ x; y ] in
  let ( bin_size_app_state
      , bin_writer_app_state
      , bin_reader_app_state
      , sexp_of_app_state ) =
    (Option.value_exn get_binprot_helpers_app_state) ()
  in
  let rand_t_b = rust_get_random_message () in
  let rand_t =
    Bin_prot.Reader.of_bytes
      (Concrete.bin_reader_t bin_reader_app_state)
      rand_t_b
  in
  let open Backend in
  let res =
    Tick_field_sponge.digest Tick_field_sponge.params
      (Types.Step.Proof_state.Messages_for_next_step_proof.to_field_elements
         rand_t ~g
         ~comm:(fun (x : Tock.Curve.Affine.t) -> Array.of_list (g x))
         ~app_state )
  in
  rust_hash_message_for_next_step_proof rand_t_b
    (Vector.to_list
       (Vector.map res ~f:(fun n -> Core_kernel.Int64.to_string n)) ) ;
  res

let hash_messages_for_next_step_proof ?get_binprot_helpers_app_state ~app_state
    (t : _ Types.Step.Proof_state.Messages_for_next_step_proof.t) =
  let concrete = (Obj.magic (Obj.repr t) : 'a Concrete.t) in
  let g (x, y) = [ x; y ] in
  Printf.eprintf "START hash_messages_for_next_step_proof\n%!" ;
  let ( bin_size_app_state
      , bin_writer_app_state
      , bin_reader_app_state
      , sexp_of_app_state ) =
    (Option.value_exn get_binprot_helpers_app_state) ()
  in
  let size = Concrete.bin_size_t bin_size_app_state concrete in
  (* Printf.eprintf "SIZE=%d\n%!" size ; *)
  let serialized =
    Bin_prot.Writer.to_string
      (Concrete.bin_writer_t bin_writer_app_state)
      concrete
  in

  let t =
    Bin_prot.Reader.of_string
      (Concrete.bin_reader_t bin_reader_app_state)
      serialized
  in

  (* Printf.eprintf
   *   "Serialized+Base64-encoded Messages_for_next_step_proof:\n%s\n%!"
   *   (Stdlib.Result.get_ok @@ Base64.encode serialized) ;
   * let sexp =
   *   Sexp.to_string_hum (Concrete.sexp_of_t sexp_of_app_state concrete)
   * in
   * Printf.eprintf "Sexp:\n%s\n%!" sexp ; *)
  let open Backend in
  let res =
    Tick_field_sponge.digest Tick_field_sponge.params
      (Types.Step.Proof_state.Messages_for_next_step_proof.to_field_elements t
         ~g
         ~comm:(fun (x : Tock.Curve.Affine.t) -> Array.of_list (g x))
         ~app_state )
  in
  (* rust_hash_message_for_next_step_proof (Stdlib.Result.get_ok @@ Base64.encode serialized) (Vector.to_list
   *         (Vector.map res ~f:(fun n -> Core_kernel.Int64.to_string n)) ) ; *)
  (* Printf.eprintf "dlog_plonk_index.comm_sigma=\n%s\n\n%!"
   *   (t.dlog_plonk_index.sigma_comm |> curve_vec_to_string) ;
   * Printf.eprintf "dlog_plonk_index.coefficients_comm=\n%s\n\n%!"
   *   (t.dlog_plonk_index.coefficients_comm |> curve_vec_to_string) ;
   * Printf.eprintf "dlog_plonk_index.generic_comm=\n%s\n\n%!"
   *   (t.dlog_plonk_index.generic_comm |> curve_to_string) ;
   * Printf.eprintf "dlog_plonk_index.psm_comm=\n%s\n\n%!"
   *   (t.dlog_plonk_index.psm_comm |> curve_to_string) ;
   * Printf.eprintf "dlog_plonk_index.complete_add_comm=\n%s\n\n%!"
   *   (t.dlog_plonk_index.complete_add_comm |> curve_to_string) ;
   * Printf.eprintf "dlog_plonk_index.mul_comm=\n%s\n\n%!"
   *   (t.dlog_plonk_index.mul_comm |> curve_to_string) ;
   * Printf.eprintf "dlog_plonk_index.emul_comm=\n%s\n\n%!"
   *   (t.dlog_plonk_index.emul_comm |> curve_to_string) ;
   * Printf.eprintf "dlog_plonk_index.endomul_scalar_comm=\n%s\n\n%!"
   *   (t.dlog_plonk_index.endomul_scalar_comm |> curve_to_string) ;
   *
   * Printf.eprintf "Calling (app_state t.app_state)\n%!" ;
   * let len = Array.length (app_state t.app_state) in
   * Printf.eprintf "Called (app_state t.app_state)=%d\n%!" len ;
   *
   * Printf.eprintf "app_state.length=%d value=\n%s\n\n%!"
   *   (Array.length (app_state t.app_state))
   *   (String.concat ~sep:"\n"
   *      (Array.to_list
   *         (Array.map (app_state t.app_state) ~f:(fun a ->
   *              Pasta_bindings.Fp.to_string a ) ) ) ) ;
   * Printf.eprintf "challenge_polynomial_commitments.length=%d inner=\n%s\n\n%!"
   *   (Nat.to_int (Vector.length t.challenge_polynomial_commitments))
   *   (String.concat ~sep:"\n"
   *      (Vector.to_list
   *         (Vector.map t.challenge_polynomial_commitments ~f:(fun (a, b) ->
   *              Pasta_bindings.Fp.to_string a
   *              ^ ","
   *              ^ Pasta_bindings.Fp.to_string b ) ) ) ) ;
   * Printf.eprintf "old_bulletproof_challenges.length=%d inner=\n%s\n\n%!"
   *   (Nat.to_int (Vector.length t.old_bulletproof_challenges))
   *   (String.concat ~sep:"\n"
   *      (Vector.to_list
   *         (Vector.map t.old_bulletproof_challenges ~f:old_challenges_to_string) ) ) ; *)
  Printf.eprintf "END hash_messages_for_next_step_proof length=%d %s\n%!"
    (Nat.to_int (Vector.length res))
    (String.concat ~sep:"_"
       (Vector.to_list
          (Vector.map res ~f:(fun n -> Core_kernel.Int64.to_string n)) ) ) ;
  res

let dlog_pcs_batch (type proofs_verified total)
    ((without_degree_bound, _pi) :
      total Nat.t * (proofs_verified, Nat.N26.n, total) Nat.Adds.t ) =
  Pcs_batch.create ~without_degree_bound ~with_degree_bound:[]

let when_profiling profiling default =
  match Option.map (Sys.getenv_opt "PICKLES_PROFILING") ~f:String.lowercase with
  | None | Some ("0" | "false") ->
      default
  | Some _ ->
      profiling

let time lab f =
  when_profiling
    (fun () ->
      let start = Time.now () in
      let x = f () in
      let stop = Time.now () in
      printf "%s: %s\n%!" lab (Time.Span.to_string_hum (Time.diff stop start)) ;
      x )
    f ()

let bits_to_bytes bits =
  let byte_of_bits bs =
    List.foldi bs ~init:0 ~f:(fun i acc b ->
        if b then acc lor (1 lsl i) else acc )
    |> Char.of_int_exn
  in
  List.map (List.groupi bits ~break:(fun i _ _ -> i mod 8 = 0)) ~f:byte_of_bits
  |> String.of_char_list

let group_map m ~a ~b =
  let params = Group_map.Params.create m { a; b } in
  stage (fun x -> Group_map.to_group m ~params x)

module Shifts = struct
  let tock1 : Tock.Field.t Shifted_value.Type1.Shift.t =
    Shifted_value.Type1.Shift.create (module Tock.Field)

  let tock2 : Tock.Field.t Shifted_value.Type2.Shift.t =
    Shifted_value.Type2.Shift.create (module Tock.Field)

  let tick1 : Tick.Field.t Shifted_value.Type1.Shift.t =
    Shifted_value.Type1.Shift.create (module Tick.Field)

  let tick2 : Tick.Field.t Shifted_value.Type2.Shift.t =
    Shifted_value.Type2.Shift.create (module Tick.Field)
end

module Lookup_parameters = struct
  let tick_zero : _ Composition_types.Zero_values.t =
    { value =
        { challenge = Challenge.Constant.zero
        ; scalar =
            Shifted_value.Type2.Shifted_value Impls.Wrap.Field.Constant.zero
        }
    ; var =
        { challenge = Impls.Step.Field.zero
        ; scalar =
            Shifted_value.Type2.Shifted_value
              (Impls.Step.Field.zero, Impls.Step.Boolean.false_)
        }
    }

  let tock_zero : _ Composition_types.Zero_values.t =
    { value =
        { challenge = Challenge.Constant.zero
        ; scalar =
            Shifted_value.Type2.Shifted_value Impls.Wrap.Field.Constant.zero
        }
    ; var =
        { challenge = Impls.Wrap.Field.zero
        ; scalar = Shifted_value.Type2.Shifted_value Impls.Wrap.Field.zero
        }
    }

  let tick ~lookup:flag : _ Composition_types.Wrap.Lookup_parameters.t =
    { use = No; zero = tick_zero }
end

let finite_exn : 'a Kimchi_types.or_infinity -> 'a * 'a = function
  | Finite (x, y) ->
      (x, y)
  | Infinity ->
      failwith "finite_exn"

let or_infinite_conv : ('a * 'a) Or_infinity.t -> 'a Kimchi_types.or_infinity =
  function
  | Finite (x, y) ->
      Finite (x, y)
  | Infinity ->
      Infinity

module Ipa = struct
  open Backend

  (* TODO: Make all this completely generic over backend *)

  let compute_challenge (type f) ~endo_to_field
      (module Field : Kimchi_backend.Field.S with type t = f) c =
    endo_to_field c

  let compute_challenges ~endo_to_field field chals =
    Vector.map chals ~f:(fun { Bulletproof_challenge.prechallenge } ->
        compute_challenge field ~endo_to_field prechallenge )

  module Wrap = struct
    let field =
      (module Tock.Field : Kimchi_backend.Field.S with type t = Tock.Field.t)

    let endo_to_field = Endo.Step_inner_curve.to_field

    let compute_challenge c = compute_challenge field ~endo_to_field c

    let compute_challenges cs = compute_challenges field ~endo_to_field cs

    let compute_sg chals =
      let comm =
        Kimchi_bindings.Protocol.SRS.Fq.b_poly_commitment
          (Backend.Tock.Keypair.load_urs ())
          (Pickles_types.Vector.to_array (compute_challenges chals))
      in
      comm.unshifted.(0) |> finite_exn
  end

  module Step = struct
    let field =
      (module Tick.Field : Kimchi_backend.Field.S with type t = Tick.Field.t)

    let endo_to_field = Endo.Wrap_inner_curve.to_field

    let compute_challenge c = compute_challenge field ~endo_to_field c

    let compute_challenges cs = compute_challenges field ~endo_to_field cs

    let compute_sg chals =
      let comm =
        Kimchi_bindings.Protocol.SRS.Fp.b_poly_commitment
          (Backend.Tick.Keypair.load_urs ())
          (Pickles_types.Vector.to_array (compute_challenges chals))
      in
      comm.unshifted.(0) |> finite_exn

    let accumulator_check comm_chals =
      let chals =
        Array.concat
        @@ List.map comm_chals ~f:(fun (_, chals) -> Vector.to_array chals)
      in
      let comms =
        Array.of_list_map comm_chals ~f:(fun (comm, _) ->
            Or_infinity.Finite comm )
      in
      let urs = Backend.Tick.Keypair.load_urs () in
      Kimchi_bindings.Protocol.SRS.Fp.batch_accumulator_check urs
        (Array.map comms ~f:or_infinite_conv)
        chals
  end
end

let tock_unpadded_public_input_of_statement prev_statement =
  let input =
    let (T (typ, _conv, _conv_inv)) = Impls.Wrap.input () in
    Impls.Wrap.generate_public_input [ typ ] prev_statement
  in
  List.init
    (Backend.Tock.Field.Vector.length input)
    ~f:(Backend.Tock.Field.Vector.get input)

let tock_public_input_of_statement s = tock_unpadded_public_input_of_statement s

let tick_public_input_of_statement ~max_proofs_verified ~uses_lookup
    (prev_statement : _ Types.Step.Statement.t) =
  let input =
    let (T (input, _conv, _conv_inv)) =
      Impls.Step.input ~proofs_verified:max_proofs_verified
        ~wrap_rounds:Tock.Rounds.n ~uses_lookup
    in
    Impls.Step.generate_public_input [ input ] prev_statement
  in
  List.init
    (Backend.Tick.Field.Vector.length input)
    ~f:(Backend.Tick.Field.Vector.get input)

let max_log2_degree = Pickles_base.Side_loaded_verification_key.max_log2_degree

let max_quot_size ~of_int ~mul:( * ) ~sub:( - ) domain_size =
  of_int 5 * (domain_size - of_int 1)

let max_quot_size_int = max_quot_size ~of_int:Fn.id ~mul:( * ) ~sub:( - )

let ft_comm ~add:( + ) ~scale ~endoscale ~negate
    ~verification_key:(m : _ Plonk_verification_key_evals.t) ~alpha
    ~(plonk : _ Types.Wrap.Proof_state.Deferred_values.Plonk.In_circuit.t)
    ~t_comm =
  let ( * ) x g = scale g x in
  let _, [ sigma_comm_last ] =
    Vector.split m.sigma_comm (snd (Plonk_types.Permuts_minus_1.add Nat.N1.n))
  in
  let f_comm =
    (* The poseidon and generic gates are special cases,
       as they use coefficient commitments from the verifier index.
       Note that for all gates, powers of alpha start at a^0 = 1.
    *)
    let poseidon =
      let (pn :: ps) = Vector.rev m.coefficients_comm in
      let res =
        Vector.fold ~init:pn ps ~f:(fun acc c -> c + endoscale acc alpha)
      in
      scale res plonk.poseidon_selector |> negate
    in

    (*
    Remember, the layout of the generic gate:
    | 0  |  1 |  2 |  3 |  4 |  5 |  6 |  7 |  8 |  9 |
    | l1 | r1 | o1 | m1 | c1 | l2 | r2 | o2 | m2 | c2 |
    *)
    let generic =
      let coeffs = Vector.to_array m.coefficients_comm in
      let (generic_selector :: l1 :: r1 :: o1 :: m1 :: l2 :: r2 :: o2 :: m2 :: _)
          =
        plonk.generic
      in
      (* Second gate first, to multiply with a power of alpha. *)
      let snd_gate = l2 * coeffs.(5) in
      let snd_gate = snd_gate + (r2 * coeffs.(6)) in
      let snd_gate = snd_gate + (o2 * coeffs.(7)) in
      let snd_gate = snd_gate + (m2 * coeffs.(8)) in
      let snd_gate = snd_gate + coeffs.(9) in
      let snd_gate = endoscale snd_gate alpha in
      (* And then the first gate. *)
      let generic_gate = snd_gate + (l1 * coeffs.(0)) in
      let generic_gate = generic_gate + (r1 * coeffs.(1)) in
      let generic_gate = generic_gate + (o1 * coeffs.(2)) in
      let generic_gate = generic_gate + (m1 * coeffs.(3)) in
      let generic_gate = generic_gate + coeffs.(4) in
      (* generic_selector * (fst_gate + snd_gate * alpha) *)
      generic_selector * generic_gate
    in

    List.reduce_exn ~f:( + )
      [ plonk.perm * sigma_comm_last
      ; generic
      ; poseidon
      ; plonk.vbmul * m.mul_comm
      ; plonk.complete_add * m.complete_add_comm
      ; plonk.endomul * m.emul_comm
      ; plonk.endomul_scalar * m.endomul_scalar_comm
      ]
  in
  let chunked_t_comm =
    let n = Array.length t_comm in
    let res = ref t_comm.(n - 1) in
    for i = n - 2 downto 0 do
      res := t_comm.(i) + scale !res plonk.zeta_to_srs_length
    done ;
    !res
  in
  f_comm + chunked_t_comm
  + negate (scale chunked_t_comm plonk.zeta_to_domain_size)

let combined_evaluation (type f)
    (module Impl : Snarky_backendless.Snark_intf.Run with type field = f)
    ~(xi : Impl.Field.t) (without_degree_bound : _ list) =
  let open Impl in
  let open Field in
  let mul_and_add ~(acc : Field.t) ~(xi : Field.t)
      (fx : (Field.t, Boolean.var) Plonk_types.Opt.t) : Field.t =
    match fx with
    | None ->
        acc
    | Some fx ->
        fx + (xi * acc)
    | Maybe (b, fx) ->
        Field.if_ b ~then_:(fx + (xi * acc)) ~else_:acc
  in
  with_label __LOC__ (fun () ->
      Pcs_batch.combine_split_evaluations ~mul_and_add
        ~init:(function
          | Some x ->
              x
          | None ->
              Field.zero
          | Maybe (b, x) ->
              (b :> Field.t) * x )
        ~xi without_degree_bound )
