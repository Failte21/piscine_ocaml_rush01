let (>>=) = Lwt.(>>=)

let (--) i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc) in
  aux j []
  
let get_stat_string n =
  let filled = n / 5 in
  let empty = 20 - filled in
  List.fold_left (fun acc _ -> acc ^ "▓") "" (1--filled) ^
  List.fold_left (fun acc _ -> acc ^ "░") "" (1--empty)

let get_time start_timer timer =
  timer := Unix.time ();

  let time = Unix.localtime (!timer -. start_timer) in
  let hour = time.Unix.tm_hour - 1 in
  let minutes = time.Unix.tm_min in
  "Your creature has survived " ^
  (if hour <> 0 then Printf.sprintf "%02uh" hour else "") ^
  (if minutes <> 0 then Printf.sprintf "%02um" minutes else "") ^
  Printf.sprintf "%02us" time.Unix.tm_sec

let display wakener creature =
  let vbox = new LTerm_widget.vbox in
  let button = new LTerm_widget.button
    ~brackets:("[ ", " ]")
    "exit"
  in
  button#on_click (Lwt.wakeup wakener);
  vbox#add button;

  (* Add Timer *)
  let timer = ref (Unix.time ()) in
  let get_timer = get_time !timer  in
  let clock = new LTerm_widget.label (get_timer timer) in
  vbox#add clock;
  ignore (Lwt_engine.on_timer 1.0 true (fun _ -> clock#set_text (get_timer timer)));

  (* Add Creature *)
  let animation = Animation.create () in
  let creature_images = new LTerm_widget.label (Animation.next_state animation) in
  vbox#add creature_images;

  ignore (Lwt_engine.on_timer 1.0 true
    (fun _ -> creature_images#set_text (Animation.next_state animation)));
  
  let labels = List.map (fun s -> new LTerm_widget.label ((Creature.stateToString s) ^ "\n" ^ (get_stat_string (Creature.getState s !creature)))) Creature.allStates in

  let labels_states = List.map2 (fun label state -> (label, state)) labels Creature.allStates in

  (* Add Stats *)
  let stat_box = new LTerm_widget.hbox in
  let create_stats () = List.iter (fun label -> stat_box#add label) labels in
  let recreate_stats () =
    List.iter (fun (label, s) -> label#set_text ((Creature.stateToString s) ^ "\n" ^ (get_stat_string (Creature.getState s !creature)))) labels_states in
  create_stats ();

  let buttons_box = new LTerm_widget.hbox in

  List.iter (fun action -> (
    let label = Action.toString action in
    let button = new LTerm_widget.button (label) in
    button#on_click (fun () -> (
      creature := Creature.applyAction action !creature;
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

let gui creature () =
  Lazy.force LTerm.stdout >>= fun term ->
    let waiter, wakener = Lwt.wait () in
    let creature = ref creature in
    let frame = display wakener creature in
    LTerm.enable_mouse term >>= fun () ->
      Lwt.finalize
        (fun () -> LTerm_widget.run term frame waiter)
        (fun () -> LTerm.disable_mouse term)
