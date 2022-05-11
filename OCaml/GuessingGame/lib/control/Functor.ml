open Combinators

module type Functor = sig
  type 'a t

  val fmap : ('a -> 'b) -> 'a t -> 'b t
  val void : 'a t -> unit t
end

module Functor_Utils (F : Functor) = struct
  open F

  let ( <$> ) f x = fmap f x
  let ( <$ ) r x = fmap (const r) x
  let ( $> ) r x = flip ( <$ ) r x
end

module FunctorList : Functor with type 'a t = 'a list = struct
  type 'a t = 'a list

  let fmap f = List.map f
  let void xs = List.map (fun _ -> ()) xs
end
