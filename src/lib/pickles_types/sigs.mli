(** Type definiation for library Pickles_types *)

type json = Yojson.Safe.t

module type JSONABLE = sig
  type t

  val to_yojson : t -> json

  val of_yojson : json -> t
end

module type HASH_FOLDABLE = sig
  type 'a t

  val hash_fold_t :
       (Ppx_hash_lib.Std.Hash.state -> 'a -> Ppx_hash_lib.Std.Hash.state)
    -> Ppx_hash_lib.Std.Hash.state
    -> 'a t
    -> Ppx_hash_lib.Std.Hash.state
end

module type COMPARABLE = sig
  type 'a t

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end
