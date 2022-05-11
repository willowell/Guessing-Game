open Applicative

module type Monad = sig
  type 'a t

  include Applicative with type 'a t := 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module Monad_Utils (M : Monad) = struct
  open M

  let ( >>= ) = bind
end
