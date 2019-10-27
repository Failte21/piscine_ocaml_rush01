let load filename =
  try
    let ic = open_in filename in
    let gs = match GameState.deserialize (input_line ic) with
      | None -> Error "File format is wrong"
      | Some gs -> Ok gs
    in
    close_in ic;
    gs
  with e -> Error (Printexc.to_string e)

let save state filename =
  try
    let oc = open_out filename in
    output_string oc (GameState.serialize state);
    close_out oc;
    Ok ()
  with e -> Error (Printexc.to_string e)
