type t = {
  creature: Creature.t;
  time: float;
}

let create () = {
  creature = Creature.create ();
  time = 0.;
}

let serialize { creature; time } =
  Creature.serialize creature ^ " " ^ Float.to_string time

let deserialize s =
  match String.split_on_char ' ' s with
  | creature_s::time_s::[] ->
    (match Creature.deserialize creature_s, Float.of_string_opt time_s with
     | Some creature, Some time -> Some { creature = creature; time = time }
     | _, _ -> None)
  | _ -> None

let applyAction action { creature; time } = {
  creature = Creature.applyAction action creature;
  time = time
}

let updateTime gameState time = {
  creature = gameState.creature;
  time = time;
}
