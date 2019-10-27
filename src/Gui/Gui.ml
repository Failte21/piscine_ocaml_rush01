let (>>=) = Lwt.(>>=)

let (--) i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc) in
  aux j []

let get_stat_string n =
  List.fold_left (fun acc _ -> acc ^ "■") "" (0--(n / 5))

type creature = { hygiene: int }

let get_time start_timer timer =
  timer := Unix.time ();

  let time = Unix.localtime (!timer -. start_timer) in
  let hour = time.Unix.tm_hour - 1 in
  let minutes = time.Unix.tm_min in
  "Your creature has survived " ^
  (if hour <> 0 then Printf.sprintf "%02uh" hour else "") ^
  (if minutes <> 0 then Printf.sprintf "%02um" minutes else "") ^
  Printf.sprintf "%02us" time.Unix.tm_sec

let display wakener (creature: creature ref) =
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

  (* Add Stats *)
  let stat_box = new LTerm_widget.hbox in
  let labels = List.map (fun _ ->
    new LTerm_widget.label ("Health\n" ^ (get_stat_string (!creature.hygiene)))) (0--4)
  in
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
