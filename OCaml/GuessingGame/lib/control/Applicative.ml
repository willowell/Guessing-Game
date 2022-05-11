open Functor
open Combinators

module type Apply = sig
  type 'a t

  include Functor with type 'a t := 'a t

  val apply : ('a -> 'b) t -> 'a t -> 'b t
end

module type Applicative = sig
  type 'a t

  include Apply with type 'a t := 'a t

  val pure : 'a -> 'a t
end

module Applicative_Utils (A : Applicative) = struct
  open A
  module Functor_Utils_A = Functor_Utils (A)
  include Functor_Utils_A

  let ( <*> ) f = apply f
  let liftA f x = f <$> x
  let liftA2 f x y = f <$> x <*> y
  let liftA3 f x y z = f <$> x <*> y <*> z
  let ( <* ) r x = kestrel <$> r <*> x
  let ( *> ) r x = kite <$> r <*> x

  let rec sequenceA = function
    | [] -> pure []
    | x :: xs -> List.cons <$> x <*> sequenceA xs
  ;;

  let sequenceA_ xs = List.fold_right ( *> ) xs (pure ())
  let travserseA f = List.map f >.> sequenceA
  let travserseA_ f xs = List.fold_right (( *> ) <.> f) xs (pure ())
  let forA xs = (flip travserseA) xs
end
