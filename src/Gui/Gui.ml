open Lwt
open LTerm_widget

let gui gameState () =
  let waiter, wakener = wait () in
  let vbox = new vbox in
  let button = new button
    ~brackets:("[ ", " ]")
    "exit"
  in
  let label = new label "_" in
  button#on_click (wakeup wakener);
  vbox#add button;
  vbox#add label;

  let stat_box = new hbox in
  let rec create_stats i =
    if i <= 0 then () else
    let label = new label "Health\n|||||||||||||||||||||||||" in
    stat_box#add label;
    create_stats (i - 1) in
  create_stats 4;

  let buttons_box = new hbox in
  let rec display_buttons i =
    if i <= 0 then () else
    let button = new button ("button" ^ string_of_int i) in
    button#on_click (fun () -> print_endline "hello");
    buttons_box#add button;
    display_buttons (i - 1) in
  display_buttons 4;
  vbox#add stat_box;
  vbox#add buttons_box;

  let frame = new frame in
  frame#set vbox;
  frame#set_label ~alignment:LTerm_geom.H_align_center "Tamagotchu";
  Lazy.force LTerm.stdout >>= fun term ->
  LTerm.enable_mouse term >>= fun () ->
  Lwt.finalize 
    (fun () -> run term frame waiter)
    (fun () -> LTerm.disable_mouse term)