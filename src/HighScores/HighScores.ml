type score = { name: string; time: float }

type t = { count: int; scores: score list }

let max_highscores = 5

let create () = { count = 0; scores = [] }

let add_score { count; scores } s =
  let rec sorted_insert l =
    match l with
    | [] -> [s]
    | hd::tl when s.time <= hd.time -> s::hd::tl
    | hd::tl -> hd::sorted_insert tl
  in
  let new_hsl = sorted_insert scores in
  if count = max_highscores
  then { count = count; scores = List.tl new_hsl }
  else { count = count + 1; scores = new_hsl }

let scores { scores } = scores

let is_high_score { scores } user_time =
  match scores with
  | [] -> true
  | { name; time }::_ -> user_time > time

let deserialize_score s =
  match String.split_on_char ',' s with
  | name::time_string::[] ->
    (match Float.of_string_opt time_string with
     | Some time -> Some { name = name; time = time }
     | _ -> None)
  | _ -> None

let serialize_score { name; time } = name ^ "," ^ Float.to_string time

let serialize { scores } =
  match scores with
  | [] -> ""
  | hd::tl ->
    serialize_score hd ^ List.fold_left
      (fun acc s -> acc ^ ";" ^ serialize_score s)
      ""
      tl

let deserialize s =
  let score_options = List.map deserialize_score (String.split_on_char ';' s) in
  if List.exists (function None -> true | _ -> false) score_options
  then None
  else Some { count = List.length score_options;
              scores = List.map Option.get score_options }

let load filename =
  try
    let ic = open_in filename in
    let hs_result = (match deserialize (input_line ic) with
        | None -> Error "File format is wrong"
        | Some hs -> Ok hs) in
    close_in ic;
    hs_result
  with e -> Error (Printexc.to_string e)

let save hs filename =
  try
    let oc = open_out filename in
    output_string oc (serialize hs);
    close_out oc;
    Ok ()
  with e -> Error (Printexc.to_string e)

let print { scores } =
  List.iter print_endline
    (List.rev_map
      (fun { name; time } -> name ^ " : " ^ Int.to_string (Float.to_int time) ^ "s")
      scores)

(* Tests *)

let score_to_string = (fun {name; time} -> Float.to_string time)

let list_to_string to_string l =
  List.fold_left (fun acc x -> acc ^ to_string x ^ "; ") "[" l ^ "]"

let test_add_score () =
  let hs = create () in
  print_endline (list_to_string score_to_string (scores hs));
  let hs = add_score hs { name = "Jean"; time = 4. } in
  print_endline (list_to_string score_to_string (scores hs));
  let hs = add_score hs { name = "Jean"; time = 2. } in
  print_endline (list_to_string score_to_string (scores hs));
  let hs = add_score hs { name = "Jean"; time = 5. } in
  print_endline (list_to_string score_to_string (scores hs));
  let hs = add_score hs { name = "Jean"; time = 9. } in
  print_endline (list_to_string score_to_string (scores hs));
  let hs = add_score hs { name = "Jean"; time = 8. } in
  print_endline (list_to_string score_to_string (scores hs))

let test_load () =
  match load "high.scores" with
  | Ok hs -> print_endline (list_to_string score_to_string (scores hs))
  | Error s -> prerr_endline ("An error happened : " ^ s)

let test_serialize () =
  match load "high.scores" with
  | Ok hs -> print_endline (serialize hs)
  | Error s -> prerr_endline ("An error happened : " ^ s)

let test_highscores () =
  test_add_score ();
  test_load ();
  test_serialize ()
