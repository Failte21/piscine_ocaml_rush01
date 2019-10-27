type image = string
type t = {
  arts: image list;
  mutable current: image;
  mutable index: int;
}

let ascii_base: image =
"
       \\:.             .:/
        \\``._________.''/
         \\             /
 .--.--, / .':.   .':. \\
/__:  /  | '::' . '::' |
   / /   |`.   ._.   .'|
  / /    |.'         '.|
 /___-_-,|.\\  \\   /  /.|
      // |''\\.;   ;,/ '|
      `==|:=         =:|
         `.          .'
           :-._____.-:
          `''       `''"

let ascii_open_eyes: image =
"       \\:.             .:/
        \\``._________.''/
         \\             /
         / .':.   .':. \\
 .--.--, | |  |   |  | |
/__:  /  | '::' . '::' |
   / /   |`.   ._.   .'|
  / /    |.'         '.|
 /___-_-,|.\\  \\   /  /.|
      // |''\\.;   ;,/ '|
      `==|:=         =:|
         `.          .'
           :-._____.-:
          `''       `''"

let create () = {
  arts = [ascii_base; ascii_open_eyes];
  current = ascii_base;
  index = 0;
}

let next_state t =
  t.index <- (t.index + 1) mod (List.length t.arts);
  t.current <-
  (try List.nth t.arts t.index
  with Failure _ | Invalid_argument _ -> List.nth t.arts 0);
  t.current

(* open Lwt

let rec loop ui coord =
  LTerm_ui.wait ui >>= function
    | LTerm_event.Key{ code = Up; _ } ->
        coord := { !coord with row = !coord.row - 1 };
        LTerm_ui.draw ui;
        loop ui coord
    | LTerm_event.Key{ code = Down; _ } ->
        coord := { !coord with row = !coord.row + 1 };
        LTerm_ui.draw ui;
        loop ui coord
    | LTerm_event.Key{ code = Left; _ } ->
        coord := { !coord with col = !coord.col - 1 };
        LTerm_ui.draw ui;
        loop ui coord
    | LTerm_event.Key{ code = Right; _ } ->
        coord := { !coord with col = !coord.col + 1 };
        LTerm_ui.draw ui;
        loop ui coord
    | LTerm_event.Key{ code = Escape; _ } ->
        return ()
    | _ ->
        loop ui coord *)
