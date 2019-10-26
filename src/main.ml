let main () =
  let gameState = GameState.create () in
  Lwt_main.run (Gui.gui gameState ())

let () = main ()
