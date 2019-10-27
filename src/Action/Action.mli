type name =
  | Eat
  | Thunder
  | Bath
  | Kill

type t

val toString: t -> string

val all: t list