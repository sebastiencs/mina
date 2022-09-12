(* Implementing triplets *)

module Label : sig
  type t = A | B | C

  val equal : t -> t -> bool

  val all : t list
end
