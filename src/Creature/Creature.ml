type t = {
  hygiene: int;
  energy: int;
  health: int;
  happiness: int;
}

let create () = {
  hygiene = 100;
  energy = 100;
  health = 100;
  happiness = 100;
}

let serialize { hygiene; energy; health; happiness } =
  string_of_int hygiene ^ ";" ^
  string_of_int energy ^ ";" ^
  string_of_int health ^ ";" ^
  string_of_int happiness

let deserialize s =
  match String.split_on_char ';' s with
  | hy::en::he::ha::[] ->
    (match Stdlib.int_of_string_opt hy,
           Stdlib.int_of_string_opt en,
           Stdlib.int_of_string_opt he,
           Stdlib.int_of_string_opt ha
     with
     | None, _, _, _ | _, None, _, _ | _, _, None, _ | _, _, _, None -> None
     | Some ihy, Some ien, Some ihe, Some iha ->
       Some
         {
           hygiene = ihy;
           energy = ien;
           health = ihe;
           happiness = iha;
         })
  | _ -> None

type state =
  | HYGIENE
  | ENERGY
  | HEALTH
  | HAPPINESS

let allStates = [HYGIENE; ENERGY; HEALTH; HAPPINESS]

let get_hygiene creature = creature.hygiene

let stateToString = function
  | HYGIENE -> "Hygiene"
  | ENERGY -> "Energy"
  | HEALTH -> "Health"
  | HAPPINESS -> "Happiness"

let getState state creature =
  match state with
    | HYGIENE -> creature.hygiene
    | ENERGY -> creature.energy
    | HEALTH -> creature.health
    | HAPPINESS -> creature.happiness

let limit value = if value < 0 then 0 else if value > 100 then 100 else value

let applyAction action creature = 
  match action with
    | (_, health_points, energy_points, hygiene_points, happiness_points) -> {
      hygiene = limit (creature.hygiene + hygiene_points);
      energy = limit (creature.energy + energy_points);
      health = limit (creature.health + health_points);
      happiness = limit (creature.happiness + happiness_points)
    }
