(* Pseudo-mercator *)

(* http://wiki.openstreetmap.org/wiki/Mercator *)

let earth_radius = 6378137.
let p4 = atan2 1. 1.
let lat2y lat =   log(tan((lat /. 90. +. 1.) *. p4 )) *. 45. /. p4
let lat2y_m lat = log(tan( lat *. p4 /. 90. +. p4  )) *. earth_radius
let lon2x_m lon = lon *. p4 /. 45. *. earth_radius

let projette (lon, lat) = Gg.P2.v lon (lat2y lat)

(* (float * float) list -> float * float * float * float *)
let bounding_box noeuds =
  ListLabels.fold_left
    ~init:(nan,nan,nan,nan)
    ~f:(fun (l,b,t,r) (lon,lat) -> min l lon, min b lat, max t lat, max r lon)
    noeuds

(* (float * float) list -> Vg.path *)
let path_of_noeuds noeuds =
  let first = noeuds |> List.hd |> projette in
  ListLabels.fold_left
    ~init:( Vg.(P.empty >> P.sub first))
    ~f:(fun pa p -> Vg.(P.line (projette p) pa))
    noeuds

(* (float * float) list -> Vg.image *)
let image noeuds =
  let area = `O { Vg.P.o with Vg.P.width = 0.005 } in
  let gr = Vg.I.const Gg.Color.black in
  Vg.I.cut ~area (path_of_noeuds noeuds) gr

(* string -> Vg.image -> unit *)
let svg_of output_file image =
  let size = Gg.Size2.v 500. 500. in
  let view = Gg.Box2.v Gg.V2.(zero + v (-4.79) 46.8)  (Gg.Size2.v 13. 13.) in
  try
    let oc = open_out output_file in
    let r = Vg.Vgr.create (Vgr_svg.target ()) (`Channel oc) in
    try
      ignore (Vg.Vgr.render r (`Image (size, view, image)));
      ignore (Vg.Vgr.render r `End);
      close_out oc
    with e -> close_out oc; raise e
  with Sys_error e -> prerr_endline e

let main1 () =
  Contour_france.france
  |> image
  |> svg_of "france.svg"

(*
   x = r clo sla
   y = r slo sla
   z = r cla

   la = acos (z / r)
   atan (y / x) = lo



*)

module V : Polyline_simplification.VECTOR with type t = Gg.v3 =
struct
  include Gg.V3
  let cross_norm2 a b = cross a b |> norm2
end

module T2 : sig
  type t = float * float
  val to_v3 : t -> V.t
  val of_v3 : V.t -> t
end = struct
  type t = float * float
  let r = 6378137.0
  let deg = atan2 1. 1. /. 45.
  let to_v3 (la,lo) =
    let lard = la *. deg and lord = lo *. deg in
    let sla = sin lard and cla = cos lard
    and slo = sin lord and clo = cos lord in
    (r *. sla *. clo, r *. sla *. slo, r *. cla)
    |> Gg.V3.of_tuple
  let of_v3 vv =
    let la = acos (Gg.V3.z vv /. r) in
    let lo = atan2 (Gg.V3.y vv) (Gg.V3.x vv) in
    la /. deg, lo /. deg
end

(* module V = struct
 *   type t = Gg.v3
 *   let sub = Gg.V3.sub
 *   let norm2 = Gg.V3.norm2
 *   let unit = Gg.V3.unit
 *   let cross_norm2 a b =
 *     Gg.V3.cross a b |> norm2
 * end *)




let simplification fm frorig epsilon =
  Array.map T2.to_v3 frorig
  |> (fun polyline -> Polyline_simplification.simplified_indexes fm ~polyline ~epsilon)
  |> Batteries.BitSet.enum
  (* Use the indexes to index the array elements we want to keep *)
  |> Batteries.Enum.map (Array.get frorig)
  |> Batteries.List.of_enum
  |> (fun l  -> Printf.printf "France precision %.0f m : %d points\n" epsilon (List.length l); l)
  |> image
  |> svg_of (Printf.sprintf "france_%04dm.svg" (truncate epsilon))

let main2 () =

  let fm = Polyline_simplification.find_middle (module V) in

  (* Original array *)
  let frorig = Array.of_list Contour_france.france in
  Printf.printf "France original = %d points\n" (Array.length frorig);
  simplification fm frorig 1000.;
  simplification fm frorig 500.;
  simplification fm frorig 100.;
  simplification fm frorig 50.;
  simplification fm frorig 10.;
  simplification fm frorig 1.

let () =
  let t0 = Benchmark.make 0L in
  main1 ();
  let t1 = Benchmark.make 0L in
  let dt1 = Benchmark.sub t1 t0 in
  main2 ();
  let t2 = Benchmark.make 0L in
  let dt2 = Benchmark.sub t2 t1 in
  Printf.printf "%s\n%s\n" (Benchmark.to_string dt1) (Benchmark.to_string dt2)
