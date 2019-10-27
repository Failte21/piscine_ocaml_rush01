type image = string
type t

val create: unit -> t
val next_state: t -> image
val set_to_dead: unit -> t
