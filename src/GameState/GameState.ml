type t = {
  is_over: bool;
  creature: Creature.t
}

let create () = {
  is_over = false;
  creature = Creature.create ()
}