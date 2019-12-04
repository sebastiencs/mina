type z = Z

type _ s = S

type _ nat = Z : z nat | S : 'n nat -> 'n s nat

let nat_to_int : type n. n nat -> int =
  let rec go : type n. int -> n nat -> int =
    fun acc n ->
    match n with
    | Z -> acc
    | S n -> go (acc + 1) n
  in
  fun x -> go 0 x

type ('a, _) t = [] : ('a, z) t | ( :: ) : 'a * ('a, 'n) t -> ('a, 'n s) t

let rec map2 : type a b c n. (a, n) t -> (b, n) t -> f:(a -> b -> c) -> (c, n) t =
 fun t1 t2 ~f ->
  match (t1, t2) with [], [] -> [] | x :: xs, y :: ys -> f x y :: map2 xs ys ~f

let zip xs ys = map2 xs ys ~f:(fun x y -> (x, y))

let rec to_list : type a n. (a, n) t -> a list =
 fun t -> match t with [] -> [] | x :: xs -> x :: to_list xs

let rec length : type a n. (a, n) t -> n nat = function
  | [] ->
      Z
  | _ :: xs ->
      S (length xs)

let rec init : type a n. int -> n nat -> f:(int -> a) -> (a, n) t =
  fun i n ~f ->
  match n with
  | Z -> []
  | S n -> f i :: init (i + 1) n ~f

let init n ~f = init 0 n ~f

let rec fold_map : type acc a b n.
    (a, n) t -> f:(acc -> a -> acc * b) -> init:acc -> acc * (b, n) t =
 fun t ~f ~init ->
  match t with
  | [] ->
      (init, [])
  | x :: xs ->
      let acc, y = f init x in
      let res, ys = fold_map xs ~f ~init:acc in
      (res, y :: ys)

let rec map : type a b n. (a, n) t -> f:(a -> b) -> (b, n) t =
 fun t ~f -> match t with [] -> [] | x :: xs -> f x :: map xs ~f

type _ e =
  | T : ('a, 'n) t -> 'a e


let rec of_list : type a. a list -> a e = function
  | [] -> T []
  | x :: xs ->
    let T xs = of_list xs in
    T (x :: xs)

let rec fold : type acc a n. (a, n) t -> f:(acc -> a -> acc) -> init:acc -> acc
    =
 fun t ~f ~init ->
  match t with
  | [] ->
      init
  | x :: xs ->
      let acc = f init x in
      fold xs ~f ~init:acc

open Core

module Cata
    (F : sig
       type _ t
       val pair : 'a t -> 'b t -> ('a * 'b) t
       val cnv : ('a -> 'b) -> ('b -> 'a) -> 'b t -> 'a t
       val unit : unit t
     end)
= struct
  let rec f
    : type n a. n nat
      -> a F.t
      -> (a, n) t F.t
    =
    fun n tc ->
      match n with
      | Z ->
        F.cnv
          (function [] -> ())
          (fun () -> [])
          F.unit
      | S n ->
        let tl = f n tc in
        F.cnv
          (function x :: xs -> (x, xs))
          (fun (x, xs) -> x :: xs)
          (F.pair tc tl)
end

module type Nat_intf = sig type n val n : n nat end

module Binable  (N : Nat_intf)
  : Binable.S1 with type 'a t := ('a, N.n) t
= struct
  open Bin_prot
  module Tc = Cata( struct
    type 'a t = 'a Type_class.t
    let pair = Type_class.bin_pair
    let cnv t = Type_class.cnv Fn.id t
    let unit = Type_class.bin_unit
  end )

  module Shape = Cata(struct
      type _ t = Shape.t
      let pair = Shape.bin_shape_pair
      let cnv _ _ = Fn.id
      let unit = Shape.bin_shape_unit
    end)

  module Size = Cata(struct
      type 'a t = 'a Size.sizer
      let pair = Size.bin_size_pair
      let cnv a_to_b _b_to_a b_sizer =
        fun a -> b_sizer (a_to_b a)
      let unit = Size.bin_size_unit
    end)

  module Write = Cata(struct
      type 'a t = 'a Write.writer
      let pair = Write.bin_write_pair

      let cnv a_to_b _b_to_a b_writer =
        fun buf ~pos a -> b_writer buf ~pos (a_to_b a)

      let unit = Write.bin_write_unit
    end)

  module Writer = Cata(struct
      type 'a t = 'a Type_class.writer
      let pair = Type_class.bin_writer_pair

      let cnv a_to_b _b_to_a b_writer =
        Type_class.cnv_writer a_to_b b_writer

      let unit = Type_class.bin_writer_unit
    end)

  module Reader = Cata(struct
      type 'a t = 'a Type_class.reader
      let pair = Type_class.bin_reader_pair

      let cnv _a_to_b b_to_a b_reader =
        Type_class.cnv_reader b_to_a b_reader

      let unit = Type_class.bin_reader_unit
    end)

  module Read = Cata(struct
      type 'a t = 'a Read.reader
      let pair = Read.bin_read_pair

      let cnv _a_to_b b_to_a b_reader =
        fun buf ~pos_ref ->
          b_to_a (b_reader buf ~pos_ref)

      let unit = Read.bin_read_unit
    end)

  let bin_shape_t sh = Shape.f N.n sh

  let bin_size_t sz = Size.f N.n sz

  let bin_write_t wr = Write.f N.n wr

  let bin_writer_t wr = Writer.f N.n wr

  let bin_t tc = Tc.f N.n tc

  let bin_reader_t re = Reader.f N.n re

  let bin_read_t re = Read.f N.n re
  let __bin_read_t__ _f _buf ~pos_ref _vint =
    Common.raise_variant_wrong_type "vector" !pos_ref
end

let rec typ
  : type f var value n.
    (var, value, f) Snarky.Typ.t
    -> n nat
    -> ((var, n) t, (value, n) t, f) Snarky.Typ.t
  =
  let open Snarky.Typ in
  fun elt n ->
  match n with
  | S n ->
    let tl = typ elt n in
    let there = function x :: xs -> (x, xs) in
    let back (x, xs) = x :: xs in
    transport (elt * tl)
      ~there ~back
  |> transport_var ~there ~back
  | Z ->
    let there [] = () in
    let back () = [] in
    transport (unit ()) ~there ~back
    |> transport_var ~there ~back
