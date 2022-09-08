type bits := bool list

(** [bits_random_oracle ~length s] *)
val bits_random_oracle : length:int -> String.t -> bits

(** [ro] *)
val ro : string -> int -> (bits -> 'a) -> unit -> 'a

(** [tock] *)
val tock : unit -> Backend.Tock.Field.t

(** [tick] *)
val tick : unit -> Backend.Tick.Field.t

(** [scalar_chal] *)
val scalar_chal :
     unit
  -> (Core_kernel.Int64.t, Pickles_types.Nat.N2.n) Pickles_types.Vector.t
     Pickles__.Import.Scalar_challenge.t

(** [chal] *)
val chal :
  unit -> (Core_kernel.Int64.t, Pickles_types.Nat.N2.n) Pickles_types.Vector.t
