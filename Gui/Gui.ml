open Lwt
open LTerm_widget

let gui () =
  let waiter, wakener = wait () in
  let vbox = new vbox in
  let button = new button
    ~brackets:("[ ", " ]")
    "exit退出"
  in
  let label = new label "_" in
  button#on_click (wakeup wakener);
  vbox#add button;
  vbox#add label;
  let frame = new frame in
  frame#set vbox;
  frame#set_label ~alignment:LTerm_geom.H_align_center "Button test按钮测试";
  Lazy.force LTerm.stdout >>= fun term ->
  LTerm.enable_mouse term >>= fun () ->
  Lwt.finalize 
    (fun () -> run term frame waiter)
    (fun () -> LTerm.disable_mouse term)