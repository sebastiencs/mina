(** Implementing structure with pre-defined length *)
(* TODO: Check if that's adequate *)

(** {2 Type definitions} *)

type ('a, 'b) t

type ('a, 'n) at_most = ('a, 'n) t

(** {2 Module Signatures} *)

module type S = sig
  type 'a t

  include Sigs.HASH_FOLDABLE with type 'a t := 'a t

  include Sigs.COMPARABLE with type 'a t := 'a t

  include Vector.Yojson_intf1 with type 'a t := 'a t

  include Core_kernel.Sexpable.S1 with type 'a t := 'a t
end

module type VERSIONED = sig
  type 'a ty

  module Stable : sig
    module V1 : sig
      type 'a t = 'a ty

      (* TODO:  bin_* functions are from Core_kernel,Binable *)
      val version : int

      val bin_shape_t : Bin_prot.Shape.t -> Bin_prot.Shape.t

      val bin_size_t : ('a, 'a t) Bin_prot.Size.sizer1

      val bin_write_t : ('a, 'a t) Bin_prot.Write.writer1

      val bin_read_t : ('a, 'a t) Bin_prot.Read.reader1

      val __bin_read_t__ : ('a, int -> 'a t) Bin_prot.Read.reader1

      val bin_writer_t : ('a, 'a t) Bin_prot.Type_class.S1.writer

      val bin_reader_t : ('a, 'a t) Bin_prot.Type_class.S1.reader

      val bin_t : ('a, 'a t) Bin_prot.Type_class.S1.t

      val __versioned__ : unit

      include S with type 'a t := 'a t
    end
  end

  type 'a t = 'a Stable.V1.t

  include S with type 'a t := 'a t
end

(** {2 Modules}*)

(** [At_most_2] *)
module At_most_2 : VERSIONED with type 'a ty = ('a, Nat.N2.n) at_most

module At_most_8 : VERSIONED with type 'a ty = ('a, Nat.N8.n) at_most

(** [With_length] *)
module With_length (N : Nat.Intf) : S with type 'a t = ('a, N.n) at_most

(** [of_vector] *)
val of_vector :
  'a 'n 'm. ('a, 'n) Vector.vec -> ('n, 'm) Nat.Lte.t -> ('a, 'm) t

(** [to_vector m] transforms [m] into a vector *)
val to_vector : 'a 'n. ('a, 'n) t -> 'a Vector.e
