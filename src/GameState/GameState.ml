type t = {
  creature: Creature.t
}

let create () = {
  creature = Creature.create ()
}

let serialize { creature } =
  Creature.serialize creature

let deserialize s =
  match Creature.deserialize s with
  | None -> None
  | Some creature -> Some { creature = creature }
