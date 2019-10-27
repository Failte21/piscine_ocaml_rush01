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
