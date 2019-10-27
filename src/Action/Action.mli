type name =
  | Eat
  | Thunder
  | Bath
  | Kill

type t = ( name * int * int * int * int )

val toString: t -> string

val all: t list