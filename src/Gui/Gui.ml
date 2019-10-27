let (>>=) = Lwt.(>>=)

let (--) i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc) in
  aux j []

let get_stat_string n =
  let filled = n / 4 in
  let empty = 25 - filled in
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


let recreate_stats labels_states creature =
  List.iter (fun (label, s) -> label#set_text ((Creature.stateToString s) ^ "\n" ^ (get_stat_string (Creature.getState s creature)))) labels_states

let udpate_game quit_fn update_stats_fn clear creature =
  if Creature.isDead creature then clear ()
  else update_stats_fn creature

let display wakener (gameState: GameState.t ref) =
  let creature = ref !gameState.creature in
  let vbox = new LTerm_widget.vbox in

  (* Add button Save and exit *)
  let save_exit_box = new LTerm_widget.hbox in
  let exit_button = new LTerm_widget.button ~brackets:("[ ", " ]") "exit" in
  let save_button = new LTerm_widget.button ~brackets:("[ ", " ]") "save" in
  let quit = Lwt.wakeup wakener in
  exit_button#on_click quit;
  save_exit_box#add exit_button;
  save_button#on_click (fun () -> ignore(Backup.save !gameState "save.itama"));
  save_exit_box#add save_button;
  vbox#add save_exit_box;

  (* Add Stats *)
  let labels = List.map (fun s -> new LTerm_widget.label ((Creature.stateToString s) ^ "\n" ^ (get_stat_string (Creature.getState s !creature)))) Creature.allStates in
  let labels_states = List.map2 (fun label state -> (label, state)) labels Creature.allStates in
  let stat_box = new LTerm_widget.hbox in
  List.iter (fun label -> stat_box#add label) labels;
  let update_stats = recreate_stats labels_states in

  (* Add Timer *)
  let timer = ref (Unix.time ()) in
  let get_timer = get_time !timer  in
  let clock = new LTerm_widget.label (get_timer timer) in
  vbox#add clock;

  (* Add Creature *)
  let animation = Animation.create () in
  let creature_images = new LTerm_widget.label (Animation.next_state animation) in
  vbox#add creature_images;
  ignore (Lwt_engine.on_timer 1.0 true
    (fun _ -> creature_images#set_text (Animation.next_state animation)));

  let buttons_box = new LTerm_widget.hbox in

  let frame = new LTerm_widget.frame in
  frame#set vbox;
  frame#set_label ~alignment:LTerm_geom.H_align_center "Tamagotchu";

  let clear () =
    vbox#remove clock;
    vbox#remove creature_images;
    vbox#remove buttons_box;
    vbox#remove stat_box in
  
  let update_d = udpate_game quit update_stats clear in

  ignore (Lwt_engine.on_timer 1.0 true (fun _ ->
    clock#set_text (get_timer timer);
    creature := Creature.applyAction Action.decay !creature;
    update_d !creature
  ));

  List.iter (fun action -> (
    let label = Action.toString action in
    let button = new LTerm_widget.button (label) in
    button#on_click (fun () -> (
      creature := Creature.applyAction action !creature;
      update_d !creature;
    ));
    buttons_box#add button;
  )) Action.all;
  vbox#add stat_box;
  vbox#add buttons_box;
  
  frame

let gui gameState () = 
  Lazy.force LTerm.stdout >>= fun term ->
    let waiter, wakener = Lwt.wait () in
    let gameState = ref gameState in
    let frame = display wakener gameState in
    LTerm.enable_mouse term >>= fun () ->
      Lwt.finalize
        (fun () -> LTerm_widget.run term frame waiter)
        (fun () -> (
          print_endline (string_of_bool (GameState.isOver !gameState)) ;
          LTerm.disable_mouse term
        ))
