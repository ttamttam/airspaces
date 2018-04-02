(** Zones aériennes *)

type airclass =
  | A (** Class A *)
  | B (** Class B ??? *)
  | C (** Class C *)
  | CTR(** CTR (Control Zone, out Control Traffic Region) *)
  | D (** Class D *)
  | E (** Class E ??? *)
  | GP (** glider prohibited *)
  | P (** prohibited *)
  | Q (** danger *)
  | R (** restricted *)
  | W (** Wave Window *)
 [@@deriving enum, show]

val airclass_of_string: string -> airclass option

(** un style de ligne, c’est *)
type line_style =
  | Transparent                 (** Transparent *)
  | Solid                       (** Continu *)
  | Dash                        (** ou Pointillé *)

val line_style_to_enum: line_style -> int
val line_style_of_enum: int -> line_style option
val pp_line_style: Format.formatter -> line_style -> unit
val show_line_style: line_style -> string

(** Un fichier de zones aériennes est constitué *)
type t = {
  filename: string;             (** du nom du fichier (sans le chemin) *)
  comment: string;              (** d’un commentaire (en début de fichier) *)
  zones: zone list;             (** et d’une liste de zones aériennes *)
}

(** une zone aérienne, c’est  *)
and zone =
  {
    ac: airclass;               (** une classe *)
    sp: pen_style;              (** un style de stylo *)
    sb: brush_style;            (** un style de brosse *)
    nm: string;                 (** un nom *)
    ah: alt list;               (** une altitude de plafond *)
    al: alt list;               (** une altitude de plancher *)
    gm: geom option;
  }

(** Un style de stylo, c’est  *)
and pen_style = {
  style: line_style;            (** un style de ligne *)
  width: int;                   (** une largeur en pixels *)
  color: rgb;                   (** une couleur *)
}

(** un style de brosse *)
and brush_style = rgb option     (** None correspond à : transparent *)

(** une couleur *)
and rgb = int * int * int    (** RGB, avec des entiers [0..255] pour Red, Green, Blue*)

(** une altitude *)
and alt =
  | Hauteur of int              (** Above ground level, in m *)
  | Altitude of int             (** Above see level, in m *)
  | FlightLevel of int          (** Flight Level, in 100 f *)
  | Unlimited

(** une forme géométrique *)
and geom =
  | Circle of circle            (** Cercle *)
  | Poly of edge list           (** Polygone, constitué d’une liste de côtés *)

(** Cercle *)
and circle =
  { o : coord;                  (** Centre *)
    r : float; }                (** Rayon *)

(** Côté d’un polygone *)
and edge =
    Point of coord              (** Point *)
  | Arc of arc                  (** Arc *)

(** Coordonnées d’un point *)
and coord = Gps.t               (** (latitude, longitude). > 0 means North or East *)

(** Arc *)
and arc =
  {
    center : coord;             (** Centre *)
    coord1 : coord;             (** Premier point *)
    coord2 : coord;             (** Second point *)
    rot : sens;                 (** Used? *)
  }

(** Sens de parcours d’un arc *)
and sens =
  | Clockwise
  | Counterclockwise

val brush_style_of_rgb: rgb -> brush_style

(* Une zone par défaut, non nommée *)
val default_zone: zone

(** Les différentes lignes qu’on peut rencontrer dans un fichier *)
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

val pp: Format.formatter -> zone -> unit
