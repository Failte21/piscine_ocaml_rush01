type t = {
  creature: Creature.t;
  time: float;
}

val applyAction: Action.t -> t -> t

val create: unit -> t

val serialize: t -> string
val deserialize: string -> t option

val updateTime: t -> float -> t
(* val applyAction: Action.t -> t -> t *)

val isOver: t -> bool

(* val save: t -> t *)
