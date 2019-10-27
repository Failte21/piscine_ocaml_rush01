type name =
  | Eat
  | Thunder
  | Bath
  | Kill
  | Decay

type t = ( name * int * int * int * int )

val toString: t -> string

val decay: t

val all: t list