let backup_file = "save.itama"

let get_game_state () : GameState.t =
  if Sys.file_exists backup_file then
    (match Backup.load backup_file with
     | Ok gs -> gs
     | Error s ->
       prerr_endline ("An error happened while loading file: " ^ s);
       GameState.create ())
  else GameState.create ()

let main () =
  let gameState = get_game_state () in
  Lwt_main.run (Gui.gui gameState ());
  try
    Sys.remove backup_file
  with e ->
    prerr_endline
      ("An error happened while removing the backup file: " ^
       Printexc.to_string e)

let () = main ()
