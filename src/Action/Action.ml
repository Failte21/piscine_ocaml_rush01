(* EAT : Health + 25, Energy - 10, Hygiene - 20, Happiness + 5
• THUNDER : Health - 20, Energy + 25, Happiness - 20
• BATH : Health - 20, Energy - 10, Hygiene + 25, Happiness + 5
• KILL : Health - 20, Energy - 10, Happiness + 20 *)

type name =
  | Eat
  | Thunder
  | Bath
  | Kill

(* health energy hygiene happiness *)
type t = ( name * int * int * int * int )

let eat = (Eat, 25, -10, -20, 5)
let thunder = (Thunder, -20, 25, 0, -20)
let bath = (Bath, -20, -10, 25, 5)
let kill = (Kill, -20, -10, 0, 20)

let all = [eat; thunder; bath; kill]

let fromString = function
  | "Eat" -> eat
  | "Thunder" -> thunder
  | "Bath" -> bath
  | "kill" -> kill
  | _ -> kill

let toString = function
  | (Eat, _, _, _, _) -> "Eat"
  | (Thunder, _, _, _, _) -> "Thunder"
  | (Bath, _, _, _, _) -> "Bath"
  | (Kill, _, _, _, _) -> "Kill"