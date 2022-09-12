open Core_kernel

[%%versioned
module Stable = struct
  module V1 = struct
    type 'a t = { a : 'a; b : 'a; c : 'a }
    [@@deriving sexp, equal, compare, hash, yojson, hlist, fields]
  end
end]

module Label = struct
  type t = A | B | C [@@deriving equal]

  let all = [ A; B; C ]
end
