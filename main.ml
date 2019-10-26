let green = "\x1b[32m"
let reset = "\x1b[0m"

let pikachu = "`;-.          ___,
`.`\\_...._/`.-\"`
  \\        /      ,
  /()   () \\    .' `-._
 |)  .    ()\\  /   _.'
 \\  -'-     ,; '. <
  ;.__     ,;|   > \
 / ,    / ,  |.-'.-'
(_/    (_/ ,;|.<`
  \\    ,     ;-`
   >   \\    /
  (_,-'`> .'
         (_,'"

let rec gameLoop window =
  let _ = Curses.waddstr window pikachu in
  let _ = Curses.refresh () in
  Curses.erase ();
  gameLoop window
(* let rec gameLoop gameState =
  if GameState.isOver gameState then GameState.endGame () else
  GameState.display gameState;
  let action = Action.pickAction "FEED" in
  let new_gameState = GameState.applyAction action;
  gameLoop new_gameState *)

(* let main () = *)

let () = 
  let window = Curses.initscr () in
  gameLoop window
  (* let gameState = GameState.startGame () in *)
  (* game_loop gameState *)
