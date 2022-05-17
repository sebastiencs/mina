open Core_kernel
open Mina_base

type t = Block.t State_hash.With_state_hashes.t * State_hash.t Non_empty_list.t
[@@deriving sexp, equal]

let to_yojson (block_with_hashes, _) =
  State_hash.With_state_hashes.to_yojson Block.to_yojson block_with_hashes

let lift (b, v) =
  match v with
  | _, _, _, (`Delta_block_chain, Truth.True delta_block_chain_proof), _, _, _
    ->
      (b, delta_block_chain_proof)

let forget (b, _) = b

let remember (b, delta_block_chain_proof) =
  ( b
  , ( (`Time_received, Truth.True ())
    , (`Genesis_state, Truth.True ())
    , (`Proof, Truth.True ())
    , (`Delta_block_chain, Truth.True delta_block_chain_proof)
    , (`Frontier_dependencies, Truth.True ())
    , (`Staged_ledger_diff, Truth.True ())
    , (`Protocol_versions, Truth.True ()) ) )

let delta_block_chain_proof (_, d) = d

let valid_commands (block, _) =
  block |> With_hash.data |> Block.body |> Body.staged_ledger_diff
  |> Staged_ledger_diff.commands
  |> List.map ~f:(fun cmd ->
         (* This is safe because at this point the stage ledger diff has been
              applied successfully. *)
         let (`If_this_is_used_it_should_have_a_comment_justifying_it data) =
           User_command.to_valid_unsafe cmd.data
         in
         { cmd with data } )

let unsafe_of_trusted_block ~delta_block_chain_proof
    (`This_block_is_trusted_to_be_safe b) =
  (b, delta_block_chain_proof)

let state_hash (b, _) = State_hash.With_state_hashes.state_hash b

let state_body_hash (t, _) =
  State_hash.With_state_hashes.state_body_hash t
    ~compute_hashes:
      (Fn.compose Mina_state.Protocol_state.hashes
         (Fn.compose Header.protocol_state Block.header) )

let header t = t |> forget |> With_hash.data |> Block.header

let body t = t |> forget |> With_hash.data |> Block.body
