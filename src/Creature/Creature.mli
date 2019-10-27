type t = {
  hygiene: int;  
  energy: int;  
  health: int;  
  happiness: int;  
}

type state =
  | HYGIENE
  | ENERGY
  | HEALTH
  | HAPPINESS

val get_hygiene: t -> int

val allStates: state list

val stateToString: state -> string

val create: unit -> t

val serialize: t -> string
val deserialize: string -> t option

val isDead: t -> bool
val applyAction: Action.t -> t -> t

val getState: state -> t -> int