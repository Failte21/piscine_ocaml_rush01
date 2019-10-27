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

let ascii_hands_up: image =
"       \\:.             .:/
        \\``._________.''/
         \\             /
         / .':.   .':. \\
 .--.--, | |  |   |  | |
 /__:  /  | '::' . '::' |,;
     / /   |`.   ._.   . /  /
  / /    |.'         /. /
 /___-_-,|.\\  \\       .|
      // |''\\.;       '|
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
  (* arts = [ascii_hands_up]; *)
  arts = [ascii_base; ascii_hands_up; ascii_base; ascii_open_eyes; ascii_base];
  current = ascii_base;
  index = 0;
}

let next_state crea =
  crea.index <- (crea.index + 1) mod (List.length crea.arts);
  crea.current <-
  (try List.nth crea.arts crea.index
  with Failure _ | Invalid_argument _ -> crea.index <- 0;List.nth crea.arts 0);
  crea.current

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
