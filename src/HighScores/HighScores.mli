type t

type score = { name: string; time: float }

val serialize: t -> string
val deserialize: string -> t option

val load: string -> (t, string) result
val save: t -> string -> (unit, string) result

val create: unit -> t
val add_score: t -> score -> t
val scores: t -> score list

val is_high_score: t -> float -> bool

val print: t -> unit
