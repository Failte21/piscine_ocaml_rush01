type t = {
  creature: Creature.t;
  time: float;
}

let create () = {
  creature = Creature.create ();
  time = 0.;
}

let serialize { creature } =
  Creature.serialize creature

let deserialize s =
  match Creature.deserialize s with
  | None -> None
  | Some creature -> Some { creature = creature }

let applyAction action state = {
  creature = Creature.applyAction action state.creature
}

let updateTime gameState time = {
  creature = gameState.creature;
  time = time;
}
