open Lwt
open LTerm_widget

let main () =
  (* let gameState = GameState.startGame () in *)
  Lwt_main.run (Gui.gui ())

let () = main ()
  (* let gameState = GameState.startGame () in *)
  (* game_loop gameState *)
