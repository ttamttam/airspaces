open Air_space__AirSpace

let rec lines ?(accu=[]) ic =
  match input_line ic with
  | line ->
    let l =
      line
      |> String.trim
      |> Lexing.from_string
      |> Air_space__Pars_as_line.line Air_space__Lex_as.general in
    Printf.printf "%s\n%!" (string_of_line l);
    lines ~accu:(l :: accu) ic
  | exception End_of_file -> List.rev accu

let lines_of_file fn =
  let ic = open_in fn in
  let l = lines ic in
  close_in ic;
  l

let () =
  let ll = lines_of_file "/home/matt/projets/ffvv-airspaces/AIRSPACE_France.txt" in
  (* let ll = lines_of_file "/home/matt/projets/ffvv-airspaces/160306__AIRSPACE_FRANCE_TXT_1602b.txt" in *)
  Printf.printf "Nb : %d" (List.length ll)
