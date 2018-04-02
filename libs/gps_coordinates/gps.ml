let arrondi p =
  p
  |> abs_float
  |> modf
  |> (fun (frac, ent) ->
    (if p < 0. then (-. 1.) else 1.) *. (ent +. floor (2. *. frac)))

let truncate' p = p |> arrondi |> truncate

let ff = 3_600_000.
let f = truncate ff

type t = int * int
(* Latitude et longitude stockés en deg × f *)

let latitude = fst
let longitude = snd

let deg_of_bin x = float x /. ff
let rad_of_deg deg = deg *. atan2 1. 1. /. 45.

let dec n x =
  let f = 10. ** float n in
  (x *. f |> arrondi) /. f

let dec5 = dec 5

let latitude_deg c = c |> latitude |> deg_of_bin |> dec5
let longitude_deg c = c |> longitude |> deg_of_bin |> dec5
let latitude_rad c = c |> latitude_deg |> rad_of_deg
let longitude_rad c = c |> longitude_deg |> rad_of_deg

let of_deg ~lat ~lon = (lat *. ff |> truncate', lon *. ff |> truncate')

let bin_of_dms (d,m,s) = d * f + m * 60_000 + (s *. 1000. |> truncate')

let dms_of_bin b =
  let d = b / f in
  let r = b - d * f in
  let m = r / 60_000 in
  let r2 = r - m * 60_000 in
  let s = float r2 /. 1000. |> dec5 in
  d,m,s

(* TODO: ajouter signe *)
let of_dms ~lat ~lon = bin_of_dms lat, bin_of_dms lon

let to_dms c = latitude c |> dms_of_bin, longitude c |> dms_of_bin
let to_deg c = latitude_deg c, longitude_deg c
let to_rad c = latitude_rad c, longitude_rad c


(* let distance x1 x2 = *)
(*   let rla1 = latitude_rad x1 *)
(*   and rla2 = latitude_rad x2 in *)
(*   let sdlo = (longitude_rad x2 -. longitude_rad x1) /. 2. |> sin in *)
(*   let sdla = (rla2 -. rla1) /. 2. |> sin in *)
(*   let a = (sdla *. sdla) +. cos rla1 *. cos rla2 *. (sdlo *. sdlo) in *)

(*   let d = 2. *. atan2 (sqrt a) (sqrt (1. -. a)) in *)

(*   (\* multiply by earth radius in m to obtain distance in m *\) *)
(*   6378137. *. d *)

let test () =
  (* let pos_char = 'N' *)
  (* and neg_char = 'S' in *)
  let deg, mins, sec =
    Random.int 180,
    Random.int 60,
    Random.float 60000. |> arrondi |> (fun x -> x /. 1000.) in

  let i = bin_of_dms (deg, mins, sec) in

  let deg', mins', sec' = dms_of_bin i in

  if deg = deg' && mins = mins' && sec = sec'
  then  (* Printf.printf "%03d %02d %.5f -> %d -> OK\n" *)
        (*               deg mins sec i *) ()
  else Printf.printf "%03d %02d %.5f -> %d -> %03d %02d %.5f \n"
                     deg mins sec i deg' mins' sec'


(*
http://geodesie.ign.fr/contenu/fichiers/Distance_longitude_latitude.pdf
http://www.jstott.me.uk/jscoord/
https://en.wikipedia.org/wiki/Vincenty%27s_formulae
-> https://www.ngs.noaa.gov/PUBS_LIB/inverse.pdf
-> http://www.movable-type.co.uk/scripts/latlong-vincenty.html
-> https://github.com/mrJean1/PyGeodesy
 *)


(* Distance de vincenty *)
let distance x1 x2 =
  let a = 6_378_137.0 in         (* demi-axe WGS-84 *)
  let f = 1. /. 298.257223563 in  (* flattening WGS-84 *)
  let b = (1. -. f) *. a in       (* autre demi-axe *)
  let phi1 = latitude_rad x1
  and phi2 = latitude_rad x2 in
  let u1 = (1. -. f) *. tan phi1 |> atan
  and u2 = (1. -. f) *. tan phi2 |> atan in
  let cos_u2 = cos u2
  and cos_u1 = cos u1
  and sin_u2 = sin u2
  and sin_u1 = sin u1 in
  let lr2 = longitude_rad x2
  and lr1 = longitude_rad x1 in
  let l = lr2 -. lr1 in

  if
    lr2 = lr1 && phi1 = phi2
  then
    (* Sinon, ça plante... *)
    0.
  else
    let rec itere lambda =
      let sin_lambda = sin lambda
      and cos_lambda = cos lambda in
      let sin_sigma =
        let t1 = cos_u2 *. sin_lambda
        and t2 = cos_u1 *. sin_u2 -. sin_u1 *. cos_u2 *. cos_lambda in
        sqrt (t1 *. t1 +. t2 *. t2) in

      (* Tester sin_sigma = 0 *)

      let cos_sigma = sin_u1 *. sin_u2 +. cos_u1 *. cos_u2 *. cos_lambda in
      let sigma = atan2 sin_sigma cos_sigma in

      let sin_alpha = cos_u1 *. cos_u2 *. sin_lambda /. sin_sigma in
      let square_cos_alpha = 1. -. sin_alpha *. sin_alpha in
      let double_sigma_cos =
        if
          square_cos_alpha = 0.
        then
          0.
        else
          cos_sigma -. 2. *. sin_u1 *. sin_u2 /. square_cos_alpha in
      let c = f /. 16. *. square_cos_alpha *. (4. +. f *. (4. -. 3. *. square_cos_alpha)) in
      let lambda' =
        l +. (1. -. c) *. f *. sin_alpha *.
             (sigma +. c *. sin_sigma
                       *. (double_sigma_cos +. c *. cos_sigma *. (2. *. double_sigma_cos *. double_sigma_cos -. 1.))) in

      if
        lambda' -. lambda < 1e-10
      then
        (square_cos_alpha, sigma, sin_sigma, cos_sigma, double_sigma_cos)
      else
        itere lambda' in

    let (square_cos_alpha, sigma, sin_sigma, cos_sigma, double_sigma_cos) = itere l in

    let square_u = square_cos_alpha *. (a *. a -. b *. b) /. b /. b in
    let bigA = 1. +. square_u /. 16384. *.(4096. +. square_u *. (square_u *. (320. -. 175. *. square_u) -. 768. )) in
    let bigB = square_u /. 1024. *. (256. +. square_u *. (square_u *. (74. -. 47. *. square_u) -. 128.)) in
    let delta_sigma =
      bigB *. sin_sigma
      *. (double_sigma_cos
          +. bigB /. 4. *. (cos_sigma *. (2. *. double_sigma_cos *. double_sigma_cos -. 1.)
                            -. bigB /. 6. *. double_sigma_cos
                               *. (4. *. sin_sigma *. sin_sigma -. 3.)
                               *. (4. *. double_sigma_cos *. double_sigma_cos -. 3.) )
         ) in
    let s = b *. bigA *. (sigma -. delta_sigma) in
    s

let p1 = of_dms ~lat:(40, 43,  5.228) ~lon:(-73, -59, -44.401)
let p2 = of_dms ~lat:(51, 29, 59.932) ~lon:(  0,  -7, -31.127)
let p3 = of_dms ~lat:(41, 29, 59.932) ~lon:(-70,  -7, -31.127)
let p4 = of_dms ~lat:(40, 29, 59.932) ~lon:(-73,  -7, -31.127)
let p5 = of_dms ~lat:(40, 40,  0.   ) ~lon:(-73, -59,   0.   )
let p6 = of_dms ~lat:(40, 40,  5.228) ~lon:(-73, -59,  44.401)


let opp x = -x


let of_dms_string1 s =
  Scanf.sscanf s "%2u°%2u'%2u\"%c , %3u°%2u'%2u\"%c"
    (fun deg_lat min_lat sec_lat north_south deg_lon min_lon sec_lon east_west ->
       let (deg_lat, min_lat, sec_lat) =
         match north_south with
         | 'N' -> deg_lat, min_lat, sec_lat
         | 'S' -> opp deg_lat, opp min_lat, opp sec_lat
         | _ -> assert false in
       let (deg_lon, min_lon, sec_lon) =
         match east_west with
         | 'E' -> deg_lon, min_lon, sec_lon
         | 'W' -> opp deg_lon, opp min_lon, opp sec_lon
         | _ -> assert false in
       of_dms
         ~lat:(deg_lat, min_lat, float sec_lat)
         ~lon:(deg_lon, min_lon, float sec_lon)
    )

let of_dms_string2 s =
  Scanf.sscanf s "%2u:%2u:%2u %c %3u:%2u:%2u %c"
    (fun deg_lat min_lat sec_lat north_south deg_lon min_lon sec_lon east_west ->
       let (deg_lat, min_lat, sec_lat) =
         match north_south with
         | 'N' -> deg_lat, min_lat, sec_lat
         | 'S' -> opp deg_lat, opp min_lat, opp sec_lat
         | _ -> assert false in
       let (deg_lon, min_lon, sec_lon) =
         match east_west with
         | 'E' -> deg_lon, min_lon, sec_lon
         | 'W' -> opp deg_lon, opp min_lon, opp sec_lon
         | _ -> assert false in
       of_dms
         ~lat:(deg_lat, min_lat, float sec_lat)
         ~lon:(deg_lon, min_lon, float sec_lon)
    )

let to_dms_string1 c =
  let ((lad, lam, las), (lod, lom, los)) = to_dms c in
  Printf.sprintf "%02u°%02u'%02u\"%c, %03u°%02u'%02u\"%c"
                 (abs lad) (abs lam) (abs_float las |> truncate)
                 (if latitude_deg c < 0. then 'S' else 'N')
                 (abs lod) (abs lom) (abs_float los |> truncate)
                 (if longitude_deg c < 0. then 'W' else 'E')


let to_dms_string2 c =
  let ((lad, lam, las), (lod, lom, los)) = to_dms c in
  Printf.sprintf "%02u:%02u:%02u %c %03u:%02u:%02u %c"
                 (abs lad) (abs lam) (abs_float las |> truncate)
                 (if latitude_deg c < 0. then 'S' else 'N')
                 (abs lod) (abs lom) (abs_float los |> truncate)
                 (if longitude_deg c < 0. then 'W' else 'E')
