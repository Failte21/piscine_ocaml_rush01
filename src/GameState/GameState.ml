type t = {
  is_over: bool;
  creature: Creature.t
}

let create () = {
  is_over = false;
  creature = Creature.create ()
}

let serialize { is_over; creature } =
  string_of_bool is_over ^ " " ^
  Creature.serialize creature

let deserialize s =
  match String.split_on_char ' ' s with
  | s_is_over::s_creature::[] ->
    (match Stdlib.bool_of_string_opt s_is_over,
           Creature.deserialize s_creature with
    | None, _ | _, None -> None
    | Some is_over, Some creature ->
      Some { is_over = is_over;
             creature = creature })
  | _ -> None
