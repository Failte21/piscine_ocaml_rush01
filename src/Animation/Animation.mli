type image = string
type t

val create: unit -> t
val next_state: t -> image
val continue: t -> t
val set_to_base: unit -> t
val set_to_eat: unit -> t
val set_to_dead: unit -> t
