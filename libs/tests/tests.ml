open Batteries
open Air_space

let parse_and_print_count_lines io =
  IO.lines_of io
  |> Enum.fold (fun lines_nb l ->
      String.trim l
      |> Lexing.from_string
      |> Pars_as_line.line Lex_as.general
      |> AirSpace.pp_line Format.std_formatter;
      Format.fprintf Format.std_formatter "\n%!";
      succ lines_nb) 0
  |> Format.fprintf Format.std_formatter "Nb : %d\n%!"

let () =
  File.with_file_in
    "/home/matt/projets/ffvv-airspaces/AIRSPACE_France.txt"
    parse_and_print_count_lines
