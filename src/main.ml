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
  match Backup.save (GameState.create ()) backup_file with
  | Error s ->
    prerr_endline ("An error happened while saving backup file: "  ^ s)
  | _ -> ()

let () = main ()
