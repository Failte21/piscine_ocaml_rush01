type t = {
  creature: Creature.t
}

val applyAction: Action.t -> t -> t

val create: unit -> t

val serialize : t -> string
val deserialize : string -> t option

(* val applyAction: Action.t -> t -> t *)

val isOver: t -> bool

(* val save: t -> t *)
