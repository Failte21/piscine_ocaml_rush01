let (>>=) = Lwt.(>>=)

let (--) i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc) in
  aux j []

let get_stat_string n =
  List.fold_left (fun acc _ -> acc ^ "■") "" (0--(n / 5))

let display wakener =
  let vbox = new LTerm_widget.vbox in
  let button = new LTerm_widget.button
    ~brackets:("[ ", " ]")
    "exit"
  in
  let label = new LTerm_widget.label "_" in
  button#on_click (Lwt.wakeup wakener);
  vbox#add button;
  vbox#add label;

  let stat_box = new LTerm_widget.hbox in
  let rec create_stats i =
    if i <= 0 then () else
    let label = new LTerm_widget.label ("Health\n" ^ (get_stat_string 100)) in
    stat_box#add label;
    create_stats (i - 1) in
  create_stats 4;

  let buttons_box = new LTerm_widget.hbox in
  let rec display_buttons i =
    if i <= 0 then () else
    let button = new LTerm_widget.button ("button" ^ string_of_int i) in
    button#on_click (fun () -> print_endline "hello");
    buttons_box#add button;
    display_buttons (i - 1) in
  display_buttons 4;
  vbox#add stat_box;
  vbox#add buttons_box;

  let frame = new LTerm_widget.frame in
  frame#set vbox;
  frame#set_label ~alignment:LTerm_geom.H_align_center "Tamagotchu";
  frame

let gui gameState () =
  let waiter, wakener = Lwt.wait () in
  let frame = display wakener in
  Lazy.force LTerm.stdout >>= fun term ->
  LTerm.enable_mouse term >>= fun () ->
  Lwt.finalize 
    (fun () -> LTerm_widget.run term frame waiter)
    (fun () -> LTerm.disable_mouse term)
