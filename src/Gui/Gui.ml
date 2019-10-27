let (>>=) = Lwt.(>>=)

let (--) i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc) in
  aux j []

let get_stat_string n =
  List.fold_left (fun acc _ -> acc ^ "■") "" (0--(n / 5))

type creature = { hygiene: int }

let display wakener (creature: creature ref) =
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

  let labels = List.map (fun _ -> new LTerm_widget.label ("Health\n" ^ (get_stat_string (!creature.hygiene)))) (0--4) in

  let create_stats () = List.iter (fun label -> stat_box#add label) labels in

  let recreate_stats () =
    List.iter (fun label -> label#set_text ("Health\n" ^ (get_stat_string !creature.hygiene))) labels in
  create_stats ();

  let buttons_box = new LTerm_widget.hbox in

  List.iter (fun action -> (
    let label = Action.toString action in
    let button = new LTerm_widget.button (label) in
    button#on_click (fun () -> (
      creature := { hygiene = !creature.hygiene - 10 };
      recreate_stats ()
    ));
    buttons_box#add button;
  )) Action.all;
  vbox#add stat_box;
  vbox#add buttons_box;

  let frame = new LTerm_widget.frame in
  frame#set vbox;
  frame#set_label ~alignment:LTerm_geom.H_align_center "Tamagotchu";
  frame

let gui gameState () =
  Lazy.force LTerm.stdout >>= fun term ->
    let waiter, wakener = Lwt.wait () in
    let creature = ref { hygiene = 100 } in
    let frame = display wakener creature in
    LTerm.enable_mouse term >>= fun () ->
      Lwt.finalize
        (fun () -> LTerm_widget.run term frame waiter)
        (fun () -> LTerm.disable_mouse term)
(* let get_time () =
  let localtime = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%02u:%02u:%02u"
    localtime.Unix.tm_hour
    localtime.Unix.tm_min
    localtime.Unix.tm_sec

let gui gameState () =
  let waiter, wakener = wait () in


  (* let anim = Creature.get_animation (GameState.get_creature gameState) in *)

  let vbox = new vbox in
  let clock = new label (get_time ()) in
  let button = new button ~brackets:("[ ", " ]") "exit" in
  button#on_click (wakeup wakener);
  vbox#add clock;
  vbox#add button;


  let animation = Animation.create () in
  let next_image = Animation.next_state animation in
  let creature = new label next_image in
  vbox#add creature;
  ignore (Lwt_engine.on_timer 1.0 true (fun _ -> creature#set_text next_image));

  (* Update the time every second. *)
  ignore (Lwt_engine.on_timer 1.0 true (fun _ -> clock#set_text (get_time ())));


  let frame = new frame in
  frame#set vbox;
  (* frame#set_label ~alignment:LTerm_geom.H_align_center "Button test按钮测试"; *)
  Lazy.force LTerm.stdout >>= fun term ->
  LTerm.enable_mouse term >>= fun () ->
  Lwt.finalize
    (fun () -> run term frame waiter)
    (fun () -> LTerm.disable_mouse term) *)
