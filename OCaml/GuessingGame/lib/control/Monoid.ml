module type Monoid = sig
  type t

  val mempty : t
  val mappend : t -> t -> t
end

module Sum : Monoid with type t = int = struct
  type t = int

  let mempty = 0
  let mappend = ( + )
end

module Product : Monoid with type t = int = struct
  type t = int

  let mempty = 1
  let mappend = ( * )
end

module All : Monoid with type t = bool = struct
  type t = bool

  let mempty = true
  let mappend = ( && )
end

module Any : Monoid with type t = bool = struct
  type t = bool

  let mempty = false
  let mappend = ( || )
end

module Monoid_Utils (M : Monoid) = struct
  open M

  let ( <+> ) x y = mappend x y
  let concat xs = List.fold_left ( <+> ) mempty xs
end

module MonoidString : Monoid with type t = string = struct
  type t = string

  let mempty = ""
  let mappend s1 s2 = s1 ^ s2
end
