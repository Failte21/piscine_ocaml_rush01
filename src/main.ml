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

let main () =
  let gameState = get_game_state () in
  Lwt_main.run (Gui.gui gameState ())

let () = main ()
