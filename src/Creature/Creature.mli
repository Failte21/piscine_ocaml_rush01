type t

val create: unit -> t

val serialize: t -> string
val deserialize: string -> t option

(* val applyAction: Action.t -> t -> t *)

(* val isDead: t -> bool *)
