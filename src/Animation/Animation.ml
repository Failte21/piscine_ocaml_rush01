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

let ascii_dead: image =
"
       \\:.             .:/
        \\``._________.''/
         \\             /
         /  \\/     \\/ \\
         |  /\\  .  /\\  |
         |`.         .'|
          |.'    0    '.|
   __-_-,|.\\  \\   /  /.|
  / / // |''\\.;   ;,/ '|
  | | `==|:=         =:|
  | |    `.          .'
  /  \\     :-._____.-:
 |____|   `''       `''"


let create () = {
  arts = [ascii_base; ascii_hands_up; ascii_base; ascii_open_eyes];
  current = ascii_base;
  index = 0;
}

let set_to_dead () = {
  arts = [ascii_dead];
  current = ascii_dead;
  index = 0;
}

let next_state crea =
  crea.index <- (crea.index + 1) mod (List.length crea.arts);
  crea.current <-
  (try List.nth crea.arts crea.index
  with Failure _ | Invalid_argument _ -> crea.index <- 0;List.nth crea.arts 0);
  crea.current
