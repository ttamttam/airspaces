type airclass = A | B | C | CTR | D | E | GP | P | Q | R | W [@@deriving enum, show]

let opt_apply f = function
  | Some v -> Some (f v)
  | None -> None

let airclass_of_string =
  let strs = Array.init
               (max_airclass - min_airclass + 1)
      (fun i -> airclass_of_enum i
                |> opt_apply show_airclass)
             |> Array.to_list in
  fun v ->
    let rec is index = function
      | [] -> None
      | Some hd :: tl when hd = v -> airclass_of_enum index
      | _ :: tl -> is (succ index) tl in
    is 0 strs

type line_style =
    Transparent [@value 5]
  | Solid [@value 0]
  | Dash [@value 1]
[@@deriving enum, show]

type t = {
  filename: string;             (* Nom du fichier (sans le chemin) *)
  comment: string;              (* Commentaire (en début de fichier) *)
  zones: zone list;             (* Liste de zones aériennes *)
}

and zone =
  {
    ac: airclass;
    sp: pen_style;
    sb: brush_style;
    nm: string;                   (* Nom *)
    ah: alt list;                      (* Plafond *)
    al: alt list;                      (* Plancher *)
    gm: geom option;
  }

and pen_style = {
  style: line_style;
  width: int;                     (* Pixels *)
  color: rgb;
}

and brush_style = rgb option    (* None correspond à : transparent *)

and rgb = int * int * int       (* RGB, avec des entiers [0..255] pour Red, Green, Blue*)

and alt =
  | Hauteur of int                      (* Par rapport au sol, en m *)
  | Altitude of int                     (* par rapport à la mer, en m *)
  | FlightLevel of int                  (* Flight Level en 100 pieds *)
  | Unlimited

and geom =
  | Circle of circle
  | Poly of edge list

and circle = { o : coord; r : float; }

and edge = Point of coord | Arc of arc

and coord = Gps.t

and arc =
  {
    center : coord;
    coord1 : coord;
    coord2 : coord;
    rot : sens;             (* Utilité ? *)
  }

and sens =
  | Clockwise
  | Counterclockwise

let brush_style_of_rgb = function
  | -1, -1, -1 -> None
  | r, g, b when r >= 0 && g >= 0 && b >= 0 ->
    Some (r, g, b)
  | _ -> assert false

let default_zone = {
  ac = A;
  sp = {style = Solid; width = 1; color = (0,0,0)};
  sb = None;
  nm = "";
  ah = [Unlimited];
  al = [Hauteur 0];
  gm = None;
}

type line =
| Void
| AirClass of airclass
| AirClassName of string
| Comment of string
| PenStyle of pen_style
| BrushStyle of brush_style
| CircleRadius of float
| AltH of alt list
| AltL of alt list
| EdgePoint of coord
| ArcPoints of coord * coord
| SetCenter of coord
| SetDir of bool
| SetLabel of coord

let pp_ac formatter z =
  Format.fprintf formatter "AC %a\n" pp_airclass z.ac

let pp_an formatter z =
  Format.fprintf formatter "AN %s\n" z.nm

let show_alt = function
  | Hauteur 0 -> "SFC"
  | Hauteur a -> Printf.sprintf "%d M AGL" a
  | Altitude a -> Printf.sprintf "%d M AMSL" a
  | FlightLevel a -> Printf.sprintf "FL%d" (a / 30)
  | Unlimited -> "UNL"

let pp_alt fmt alt =
  Format.fprintf fmt "%s" (show_alt alt)

let pp_al formatter z =
  List.iter (fun alt ->
      Format.fprintf formatter "AL ";
      pp_alt formatter alt;
      Format.fprintf formatter "\n") z.al

let pp_ah formatter z =
  List.iter (fun z ->
      Format.fprintf formatter "AH ";
      pp_alt formatter z;
      Format.fprintf formatter "\n") z.ah

let pp formatter z =
  Format.fprintf formatter "%a%a%a%a"
    pp_ac z pp_an z pp_al z pp_ah z
