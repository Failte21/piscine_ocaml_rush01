let backup_file = "save.itama"

type user_action = LoadFile | NewGame | DeleteFile

let rec get_user_action () =
  print_string
    "A backup file was found. Do you want to load it ? (Y/n/d) ";
  match read_line () with
  | "y" | "Y" | "yes" | "" -> LoadFile
  | "n" | "N" | "no" -> NewGame
  | "d" | "D" -> DeleteFile
  | _ -> get_user_action ()

let delete_backup_file () =
  try Sys.remove backup_file
  with e ->
    prerr_endline
      ("An error happened while removing the backup file: " ^
       Printexc.to_string e)

let get_game_state () : GameState.t =
  try (* Sys.file_exists can throw *)
    if Sys.file_exists backup_file then
      match get_user_action () with
      | LoadFile ->
        (match Backup.load backup_file with
         | Ok gs -> gs
         | Error s ->
           prerr_endline ("An error happened while loading file: " ^ s);
           GameState.create ())
      | NewGame -> GameState.create ()
      | DeleteFile -> delete_backup_file (); GameState.create ()
    else GameState.create ()
  with e ->
    prerr_endline
      ("An error happened while checking for the backup file: " ^
       (Printexc.to_string e));
    GameState.create ()

let update_high_scores (gameState:GameState.t) =
  let high_scores_file = "high.scores" in
  let new_high_score hs =
    print_endline ("Congratulations! You made a new high score: " ^
      Int.to_string (Float.to_int gameState.time) ^ " !");
    let rec get_name () =
      print_string "Enter your name: ";
      match read_line () with
      | "" -> get_name ()
      | name -> name
      in
    let hs = HighScores.add_score hs
        { name = get_name ();
          time = gameState.time } in
    HighScores.print hs;
    match HighScores.save hs high_scores_file with
    | Ok () -> ()
    | Error s ->
      prerr_endline ("An error happened while writing High Scores: " ^ s)
  in
  let hs_op =
    (if Sys.file_exists high_scores_file
     then
       (match HighScores.load high_scores_file with
        | Ok hs -> Some hs
        | Error s ->
          prerr_endline ("An error happened while reading High Scores: " ^ s);
          None)
     else Some (HighScores.create ()))
  in
  Option.iter
    (fun hs ->
       if HighScores.is_high_score hs gameState.time
       then new_high_score hs)
    hs_op

let main () =
  try
    let gameState = ref (get_game_state ()) in
    Lwt_main.run (Gui.gui gameState ());
    update_high_scores !gameState
  with e -> prerr_endline ("An error happened: " ^ Printexc.to_string e)

let () = main ()
