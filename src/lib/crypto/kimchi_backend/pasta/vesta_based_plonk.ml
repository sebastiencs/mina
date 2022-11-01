open Core_kernel
open Kimchi_backend_common
open Basic
module Field = Fp
module Curve = Vesta

module Bigint = struct
  module R = struct
    include Field.Bigint

    let of_data _ = failwith __LOC__

    let to_field = Field.of_bigint

    let of_field = Field.to_bigint
  end
end

let field_size : Bigint.R.t = Field.size

module Verification_key = struct
  type t =
    ( Pasta_bindings.Fp.t
    , Kimchi_bindings.Protocol.SRS.Fp.t
    , Pasta_bindings.Fq.t Kimchi_types.or_infinity Kimchi_types.poly_comm )
    Kimchi_types.VerifierIndex.verifier_index

  let to_string _ = failwith __LOC__

  let of_string _ = failwith __LOC__

  let shifts (t : t) = t.shifts
end

module R1CS_constraint_system =
  Plonk_constraint_system.Make
    (Field)
    (Kimchi_bindings.Protocol.Gates.Vector.Fp)
    (struct
      let params =
        Sponge.Params.(
          map pasta_p_kimchi ~f:(fun x ->
              Field.of_bigint (Bigint256.of_decimal_string x) ))
    end)

let lagrange : int -> _ Kimchi_types.poly_comm array =
  Memo.general ~hashable:Int.hashable (fun domain_log2 ->
      Array.map
        Precomputed.Lagrange_precomputations.(
          vesta.(index_of_domain_log2 domain_log2))
        ~f:(fun unshifted ->
          { Kimchi_types.unshifted =
              Array.map unshifted ~f:(fun (x, y) -> Kimchi_types.Finite (x, y))
          ; shifted = None
          } ) )

let with_lagrange f (vk : Verification_key.t) =
  f (lagrange vk.domain.log_size_of_group) vk

let with_lagranges f (vks : Verification_key.t array) =
  let lgrs =
    Array.map vks ~f:(fun vk -> lagrange vk.domain.log_size_of_group)
  in
  f lgrs vks

module Rounds_vector = Rounds.Step_vector
module Rounds = Rounds.Step

module Keypair = Dlog_plonk_based_keypair.Make (struct
  let name = "vesta"

  module Rounds = Rounds
  module Urs = Kimchi_bindings.Protocol.SRS.Fp
  module Index = Kimchi_bindings.Protocol.Index.Fp
  module Curve = Curve
  module Poly_comm = Fp_poly_comm
  module Scalar_field = Field
  module Verifier_index = Kimchi_bindings.Protocol.VerifierIndex.Fp
  module Gate_vector = Kimchi_bindings.Protocol.Gates.Vector.Fp
  module Constraint_system = R1CS_constraint_system
end)

module Proof = Plonk_dlog_proof.Make (struct
  let id = "pasta_vesta"

  module Scalar_field = Field
  module Base_field = Fq

  module Backend = struct
    type t =
      ( Pasta_bindings.Fq.t Kimchi_types.or_infinity
      , Pasta_bindings.Fp.t )
      Kimchi_types.prover_proof

    include Kimchi_bindings.Protocol.Proof.Fp

    let batch_verify vks ts =
      Promise.run_in_thread (fun () -> batch_verify vks ts)

    let create_aux ~f:create (pk : Keypair.t) primary auxiliary prev_chals
        prev_comms =
      (* external values contains [1, primary..., auxiliary ] *)
      let external_values i =
        let open Field.Vector in
        if i = 0 then Field.one
        else if i - 1 < length primary then get primary (i - 1)
        else get auxiliary (i - 1 - length primary)
      in

      (* compute witness *)
      let computed_witness =
        R1CS_constraint_system.compute_witness pk.cs external_values
      in
      let num_rows = Array.length computed_witness.(0) in

      (* convert to Rust vector *)
      let witness_cols =
        Array.init Kimchi_backend_common.Constants.columns ~f:(fun col ->
            let witness = Field.Vector.create () in
            for row = 0 to num_rows - 1 do
              Field.Vector.emplace_back witness computed_witness.(col).(row)
            done ;
            witness )
      in
      create pk.index witness_cols prev_chals prev_comms

    let create_async (pk : Keypair.t) primary auxiliary prev_chals prev_comms =
      create_aux pk primary auxiliary prev_chals prev_comms
        ~f:(fun pk auxiliary_input prev_challenges prev_sgs ->
          Promise.run_in_thread (fun () ->
              create pk auxiliary_input prev_challenges prev_sgs ) )

    let create (pk : Keypair.t) primary auxiliary prev_chals prev_comms =
      create_aux pk primary auxiliary prev_chals prev_comms ~f:create
  end

  module Verifier_index = Kimchi_bindings.Protocol.VerifierIndex.Fp
  module Index = Keypair

  module Evaluations_backend = struct
    type t = Scalar_field.t Kimchi_types.proof_evaluations
  end

  module Opening_proof_backend = struct
    type t = (Curve.Affine.Backend.t, Scalar_field.t) Kimchi_types.opening_proof
  end

  module Poly_comm = Fp_poly_comm
  module Curve = Curve
end)

module Proving_key = struct
  type t = Keypair.t

  include
    Core_kernel.Binable.Of_binable
      (Core_kernel.Unit)
      (struct
        type nonrec t = t

        let to_binable _ = ()

        let of_binable () = failwith "TODO"
      end)

  let is_initialized _ = `Yes

  let set_constraint_system _ _ = ()

  let to_string _ = failwith "TODO"

  let of_string _ = failwith "TODO"
end

module Oracles = Plonk_dlog_oracles.Make (struct
  module Verifier_index = Verification_key
  module Field = Field
  module Proof = Proof

  module Backend = struct
    include Kimchi_bindings.Protocol.Oracles.Fp

    let create = with_lagrange create
  end
end)
