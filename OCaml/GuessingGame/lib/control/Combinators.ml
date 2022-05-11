let id x = x
let const x _ = x
let kestrel = const
let kite _ x = x
let flip f x y = f y x
let compose f g x = f (g x)
let ( <.> ) f g x = f (g x)
let ( >.> ) g f x = f (g x)
