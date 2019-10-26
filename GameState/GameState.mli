type t

val applyAction: Action.t -> t -> t

val display: t -> unit

val startGame: unit -> t

val isOver: t -> bool 

val endGame: unit -> unit