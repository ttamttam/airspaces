(*
#require"netclient";;
#require "nettls-gnutls";;
#require"re.str";;
#require"lambdasoup";;
#require "webr";;
#require "gps-coordinates";;
*)
let () = Nettls_gnutls.init ()

(**************************************)
(*   ADRESSE HTTP eAIP EN COURS ET    *)
(*      DATE DE PUBLICATION           *)
(**************************************)

let href_without_file_part x =
  x
  |> String.split_on_char '/'
  |> List.rev |> List.tl |> List.rev
  |> String.concat "/"

let eaip_addr, date_publication =
  let open Soup in
  let lien_eaip =
    with_stop
      (fun stop ->
         "https://www.sia.aviation-civile.gouv.fr"
         |> Nethttp_client.Convenience.http_get
         |> parse
         |> select "a"
         |> iter (fun elt ->
             if leaf_text elt = Some "eAIP FRANCE"
             then stop.throw (R.attribute "href" elt)
           );
         "" ) in

  (* Recherche de la partie du lien qui pointe vers vers eAIP *)
  Re_str.search_forward
    (Re_str.regexp {|https://www.sia.aviation-civile.gouv.fr/documents/htmlshow\?.*=\(.*\)|})
    lien_eaip
    0
  |> ignore;

  let eaip_listings_lien = Printf.sprintf
      {|https://www.sia.aviation-civile.gouv.fr/%s|}
      (Re_str.matched_group 1 lien_eaip) in

  eaip_listings_lien
  |> Nethttp_client.Convenience.http_get
  |> parse
  |> R.select_one "table > tbody > tr > td.green > a"
  |> (fun link ->
      (
        (href_without_file_part eaip_listings_lien
         ^ "/"
         ^ (R.attribute "href" link
            |> href_without_file_part)),
        (R.leaf_text link)
      )
    )

(********************)
(*   SECTION eAIP   *)
(********************)
module W = Webr

(* Récupération de la section x de l’eAIP *)
let get_section eaip_addr x =
  Printf.sprintf "%s/eAIP/FR-%s-fr-FR.html" eaip_addr x
  |> Nethttp_client.Convenience.http_get
  |> W.tree_of_xmlstring

(********************)
(*   option monad   *)
(********************)

let (||>) x f = match x with
  | None -> None
  | Some x -> f x

let (|.>) x f = match x with
  | None -> []
  | Some x -> f x

(********************)
(*   TABLES         *)
(********************)

let line_of_tr tr = W.get_childs_elt ~condition:(W.is_tag "td") tr

let simple_table sec1 sec2 =
  get_section eaip_addr sec2
  |> W.get_first_elt ~condition:(W.is_tag "body")
  ||> W.get_first_elt ~condition:(W.is_attribute ~attrib:"id" sec1)
  ||> W.get_first_elt ~condition:(W.is_attribute ~attrib:"id" sec2)
  ||> W.get_first_elt ~condition:(W.is_tag "table")
  ||> W.get_first_elt ~condition:(W.is_tag "tbody")
  |.> W.get_childs_elt ~condition:(W.is_tag "tr")
  |> List.rev_map line_of_tr
  |> List.rev


let table_tma = simple_table "ENR-2" "ENR-2.3"
let table_cta = simple_table "ENR-2" "ENR-2.4"
let table_parcs = simple_table  "ENR-5"  "ENR-5.6"

let table_prohibited_and_restricted =
  get_section eaip_addr "ENR-5.1"
  |> W.get_first_elt ~condition:(W.is_tag "body")
  ||> W.get_first_elt ~condition:(W.is_attribute ~attrib:"id" "ENR-5")
  ||> W.get_first_elt ~condition:(W.is_attribute ~attrib:"id" "ENR-5.1")
  |.> W.get_childs_elt ~condition:(W.is_tag "table")
  |> List.rev
  |> List.hd
  |> W.get_first_elt ~condition:(W.is_tag "tbody")
  |.> W.get_childs_elt ~condition:(W.is_tag "tr")
  |> List.rev_map line_of_tr
  |> List.rev

(*********************************)
(*   CONTROL TRAFFIC AREAS (CTR) *)
(*      AIP France - AD 1.7      *)
(*********************************)

(* Extraction du nom de la CTR *)
let ctr_name cells =
  cells |> List.hd |> W.get_string ~concat_sep:" "

(* Extraction de la classe de la CTR *)
let ctr_classe cells =
  cells |> List.tl |> List.hd
  |> W.get_string

let ctr_vert cells =
  cells |> List.tl |> List.tl |> List.hd
  |> W.get_string ~concat_sep:" "
  |> Re.split (Re.compile @@ Re.str "------------")

let table_ctr = simple_table "AD-1" "AD-1.7"

(* Deux lignes par CTR *)
let ctrs =
  (* let table_ctr = simple_table "AD-1" "AD-1.7" in *)
  let rec ctr accu = function
    | ti :: dt :: tl ->

      let alts = ctr_vert dt in

      let ah =
        match
          Air_space.Pars_as_line.line Air_space.Lex_as.general
            (Lexing.from_string ("AH " ^ (List.hd alts)))
        with
        | Air_space.AirSpace.AltH ah -> ah
        | _ -> assert false in

      let al =    match
          Air_space.Pars_as_line.line Air_space.Lex_as.general
            (Lexing.from_string ("AL " ^ (List.hd @@ List.tl alts)))
        with
        | Air_space.AirSpace.AltL al -> al
        | _ -> assert false in

      let c = Air_space.AirSpace.{
          default_zone with
          nm = ctr_name ti;
          ac = CTR;
          ah = ah;
          al = al;
      } in
      ctr (c :: accu) tl

    | [] -> List.rev accu
    | _ -> assert false in
  ctr [] table_ctr;;

(* type bord = *)
(*     Edge of Gps.t *)
(*   | Arc of Gps.t * float *)
(*   | ArcAnti of Gps.t * float *)
(*   | Cercle of Gps.t * float *)
(*   | Frontiere of string *)
(*   | Unknown of string *)

(* type ctr_datas = { *)
(*   nom: string; *)
(*   lat: bord list; *)
(*   classe: string; *)
(*   vert: string list; *)
(* } *)

let edge_of_string s =
  Air_space.AirSpace.EdgePoint (Gps.of_dms_string1 s)

let arc_of_string s =
  Scanf.sscanf s "arc horaire de %f NM de rayon centré sur %s@z"
    (fun rayon centre -> Arc (Gps.of_dms_string1 centre, rayon))

let arc_anti_of_string s =
  Scanf.sscanf s "arc anti-horaire de %f NM de rayon centré sur %s@z"
    (fun rayon centre -> ArcAnti (Gps.of_dms_string1 centre, rayon))

let cercle_of_string s =
  Scanf.sscanf s "cercle de %f NM de rayon centré sur %s@z"
    (fun rayon centre ->
       let open Air_space.AirSpace in
       Circle { o = Gps.of_dms_string1 centre; r = rayon})

let frontiere_of_string s =
  Scanf.sscanf s "Frontière %s@z" (fun n -> Frontiere n)

let bord_of_string s =
  let rec bos s = function
    | [] -> Unknown s
    | hd :: tl ->
      try
        hd s
      with _ ->
        bos s tl in
  bos s [ edge_of_string;
          arc_of_string;
          arc_anti_of_string;
          cercle_of_string;
          frontiere_of_string; ]

(* Extraction du chemin *)
let ctr_coords cells =
  cells
  |> List.hd
  |> W.get_string ~concat_sep:" "
  (* On supprime les précisions de lieu entre parenthèses *)
  |> Re_str.global_replace (Re_str.regexp "([^()]*)") ""
  |> Re.split (Re.compile @@ Re.str " - ")
  |> List.map bord_of_string





(* let _ = List.iter (fun c -> *)
(*     List.iter (function *)
(*         | Edge e -> (\* Gps.to_deg e |> (fun (la, lo) -> Printf.printf "%f, %f\n" la lo) *\) () *)
(*         | Arc (c,r) -> Printf.printf "Arc(%s - %.1f mile n)\n" *)
(*                          (Gps.to_deg c |> (fun (la, lo) -> Printf.sprintf "%f, %f" la lo)) r *)
(*         | ArcAnti (c,r)  -> Printf.printf "ArcAnti(%s - %.1f mile n)\n" *)
(*                          (Gps.to_deg c |> (fun (la, lo) -> Printf.sprintf "%f, %f" la lo)) r *)
(*         | Cercle (c,r)  -> Printf.printf "Cercle(%s - %.1f mile n)\n" *)
(*                              (Gps.to_deg c |> (fun (la, lo) -> Printf.sprintf "%f, %f" la lo)) r *)
(*         | Frontiere f -> Printf.printf "Frontière : %s\n" f *)
(*         | Unknown e -> print_endline( "?" ^ e)) c.lat) ctrs;; *)

(* let _ = List.iter (fun c -> print_endline c.classe) ctrs;; *)

(* let _ = List.iter (fun c -> String.concat "/" c.vert |> print_endline) ctrs *)

let _ = List.iter (fun c ->
    let open Air_space.AirSpace in
    match c.ah with
    | Hauteur h -> Printf.printf "Hauteur %d\n" h             (** Above ground level, in m *)
    | Altitude a -> Printf.printf "Altitude %d\n" a             (** Above see level, in m *)
    | FlightLevel f -> Printf.printf "FL %d\n"  f        (** Flight Level, in 100 f *)
    | Unlimited -> Printf.printf "No limit\n"
  ) ctrs

let _ = List.iter (fun c ->
    let open Air_space.AirSpace in
    match c.al with
    | Hauteur h -> Printf.printf "Hauteur %d\n" h             (** Above ground level, in m *)
    | Altitude a -> Printf.printf "Altitude %d\n" a             (** Above see level, in m *)
    | FlightLevel f -> Printf.printf "FL %d\n"  f        (** Flight Level, in 100 f *)
    | Unlimited -> Printf.printf "No limit\n"
  ) ctrs

let _ =
  let open Air_space.AirSpace in
List.iter (fun c ->
      string_of_line (AirClassName c.nm) |> print_endline;
      string_of_line (AirClass c.ac) |> print_endline;
      string_of_line (AltH c.ah) |> print_endline;
      string_of_line (AltL c.al) |> print_endline;
      print_newline ()
  ) ctrs


(**************************************)
(*   MOYENS DE RADIOCOMMUNICATION ATS *)
(*      AIP France - AD 1.6           *)
(**************************************)

type moyen_comm_service = {
  serv: string;
  indicatif: string;
  freq: string;
  hor: string;
  rem: string;
}

type moyen_comm = {
  nm: string;
  oaci: string;
  services: moyen_comm_service list;
}



let comms =
  let table_moyens_comm = simple_table "AD-1" "AD-1.6" in
  (* Pour cette table, les titres prennent toutes les colonnes, et il peut y
     avoir plusieurs lignes par moyen de communication *)
  let rec comms (current, accu) = function
    | line :: tl ->
      if List.length line = 1
      then
        let l =
          line
          |> W.get_childs_elt ~condition:(W.is_tag "span")
          |> List.map (Webr.get_string ~condition:(fun _ -> true))
          |> List.map (fun s -> W.D s) in

        comms ([l], List.rev current :: accu) tl
      else comms (line::current, accu) tl
    | [] ->
      List.rev (List.rev current :: accu)
      |> List.filter ((<>) []) in
  comms ([],[]) table_moyens_comm

(* let clean_dels nds () = *)
(*   nds *)
(*   |> descendants *)
(*   |> iter *)
(*     begin fun n -> *)
(*       if is_element n *)
(*       && (element n |> require |> name) = "del" *)
(*       then *)
(*         delete n *)
(*     end *)

let clean_dels nds () =
  nds
  |> descendants
  |> elements
  |> iter (fun n -> if name n = "del" then delete n)

(* let services_of_servs servs = *)
(*   servs *)
(*   |> select "td" *)
(*   |> to_list *)
(*   |> List.rev_map texts *)
(*   |> List.rev_map (fun sl -> *)
(*       List.map String.trim sl *)
(*       |> List.filter ( (<>) "" ) *)
(*       |> String.concat " ") *)

let services_of_servs servs =
  let s =  servs
           |> select "td"                (* columns *)
           |> (fun x -> Printf.printf "%d\n%!" (to_list x |> List.length); x)
           |> map (fun n -> n |> trimmed_texts |> String.concat " " |> create_text)
           |> to_list
           |> Array.of_list in
  {
    serv = R.leaf_text s.(0);
    indicatif = R.leaf_text s.(1);
    freq = R.leaf_text s.(2);
    hor = R.leaf_text s.(3);
    rem = R.leaf_text s.(4);
  }

(* let moyen_comm_of_nodes nds = *)
(*   let lst = *)
(*     nds *)
(*     |> List.hd *)
(*     |> texts *)
(*     |> List.rev_map String.trim *)
(*     |> List.filter begin fun elt -> elt <> "" && elt <> "(" && elt <> ")" end in *)
(*   let oaci = List.hd lst *)
(*   and nm = String.concat " " (List.tl lst) *)
(*   and services = *)
(*     nds *)
(*     |> List.tl *)
(*     |> List.map services_of_servs in *)

(*   { nm; oaci; services} *)

let moyen_comm_of_nodes nds =

  (* let nds' : 'a Soup.node list = *)
  (*   List.map (filter (fun n -> name e <> "del")) nds in *)

  match nds with
  | [] -> assert false
  | (title :: datas) ->
    let nm, oaci =
      match
        title
        |> trimmed_texts
        |> List.filter (fun elt -> elt <> "(" && elt <> ")")
      with
      | name :: rev_oaci -> name, String.concat " " (List.rev rev_oaci)
      | _ -> assert false in
    Printf.printf "* %s :\n%!" oaci;

    let services =  List.map services_of_servs datas in

  { nm; oaci; services}

module SM = Map.Make(String)

let mix_table node =
  fold
    (fun (nxt, acc) table_row ->
       clean_dels table_row ();
       if
         (* TODO: données de merde de l’EAIP. VANNES MEUCON ( LFRV ) : la ligne
            de données a l'attribut keep-with-next-row.

            Donc il faut repasser sur une analyse des colspan
         *)
         List.mem "keep-with-next-row" (classes table_row)
       then
         (table_row::nxt, acc)
       else
         ([], (List.rev (table_row::nxt)) :: acc)
    )
    ([],[])
    node
  |> snd

let mix_table node =
  fold
    (fun (nxt, acc) table_row ->
       clean_dels table_row ();
       match nxt,
             table_row
             |> R.select_one "td"
             |> attribute "colspan"
       with
       | [], Some "1" ->
         ([table_row], acc)
       | nxt, Some "1" ->
         ([table_row], List.rev nxt :: acc)
       | nxt, Some "5" ->
         ( table_row :: nxt, acc )
       | _ -> assert false
    )
    ([],[])
    node
  |> snd

let moyens_comm_map =
  let t0 = Unix.time () in
  let tm i x =
    let () = Printf.printf "t%d = %.1f s\n" i (Unix.time () -. t0) in
    x in

  ad16
  |> select {|div[id="AD-1.6"] > table > tbody tr|}
  |> mix_table
  |> tm 1
  |> List.rev_map moyen_comm_of_nodes
  |> tm 2
  |> List.fold_left (fun sm mc -> SM.add mc.oaci mc sm) SM.empty
  |> tm 3

let pourvoir =
  ad16
  |> select {|div[id="AD-1.6"] > table > tbody tr|}
  |> to_list
  |> List.rev_map to_string
  |> List.rev
  |> String.concat "\n"



let nancy = SM.find "LFSO" moyens_comm_map
let romorantin = SM.find "LFYR" moyens_comm_map
let saint_ex = SM.find "LFLL" moyens_comm_map

(*********************************)
(*   CONTROL TRAFFIC AREAS (CTR) *)
(*      AIP France - AD 1.7      *)
(*********************************)

type ctr_datas = {
  nom: string list;
  lat: string list;
  classe: string list;
  vert: string list;
  indicatif: string list;
  observations: string list
}

let ctr_of_nodes nds =
  assert(List.length nds = 2);
  let nom =
    nds
    |> List.hd
    |> texts
    |> List.rev_map String.trim
    |> List.filter (fun elt -> elt <> ""
                               && elt <> "\194\160")
    |> List.rev in
  assert(List.hd nom = "CTR");

let datas = nds
    |> List.tl
    |> List.hd
    |> select "td"
    |> Soup.to_list
    |> List.rev_map
         (fun cell ->
           texts cell
           |> List.rev_map String.trim
           |> List.filter (fun elt -> elt <> "")
           |> List.rev)

 in
 assert(List.length datas = 5);
 match datas with
 | [observations; indicatif; vert; classe; lat] ->
    {nom; lat; classe; vert; indicatif; observations}
 | _ -> assert false

module SLS = Set.Make (struct
                        type t = ctr_datas
                        let compare = compare
                      end)

let ad17 = get_section eaip_addr "AD-1.7"

let ctr_list =
  ad17
  |> select {|body div[id="AD-1.7"] > table > tbody tr|}
  |> mix_table
  |> List.rev_map ctr_of_nodes

let tiret = Re.compile @@ Re.str " - "

(* let flt deg_lat min_lat sec_lat north_south deg_lon min_lon sec_lon east_west = *)

(*   let flt_of_dms pos neg deg mins sec dir = *)
(*     let sgn = if dir = pos then (~+.) else if dir = neg then (~-.) else assert false in *)
(*     float deg +. float mins /. 60. +. float sec /. 3600. |> sgn in *)

(*   flt_of_dms 'N' 'S' deg_lat min_lat sec_lat north_south, *)
(*   flt_of_dms 'E' 'W' deg_lon min_lon sec_lon east_west *)

(* let flt_of_string1 s = Scanf.sscanf s ("%2u°%2u'%2u\"%c , %3u°%2u'%2u\"%c") flt *)

(* let arrondi p = *)
(*   (\* signe de p *\) *)
(*   let s =  if p < 0. then (-. 1.) else 1. in *)
(*   let d = modf (abs_float p) in *)
(*     s *. (snd d +. floor (2. *. (fst d))) *)

(* let dms (lat, lon) = *)
(*   let dms_of_flt pos neg f = *)
(*     let dir = if f < 0. then neg else pos in *)
(*     let rem, deg = modf (abs_float f) in *)
(*     let rem, min = modf (60. *. rem) in *)
(*     let sec = 60. *. rem |> arrondi in *)
(*     truncate deg, truncate min, truncate sec, dir in *)
(*   let deg_lat, min_lat, sec_lat, north_south = dms_of_flt 'N' 'S' lat *)
(*   and deg_lon, min_lon, sec_lon, east_west = dms_of_flt 'E' 'W' lon in *)
(*   deg_lat, min_lat, sec_lat, north_south, deg_lon, min_lon, sec_lon, east_west *)

(* let string1_of_float coords = *)
(*   let deg_lat, min_lat, sec_lat, north_south, deg_lon, min_lon, sec_lon, east_west = dms coords in *)
(*   Printf.sprintf "%02d°%02d'%02d\"%c" deg_lat min_lat sec_lat north_south, *)
(*   Printf.sprintf "%03d°%02d'%02d\"%c" deg_lon min_lon sec_lon east_west *)

(* On ne peut pas stocker les points en degrés décimaux correctement : perte de précision.
   Il faut les stocker en d,m,s
 *)


let _  = List.rev_map (fun e -> String.concat " " e.nom) ctr_list

let _  = List.rev_map (fun e -> String.concat " " e.classe) ctr_list
let _  = List.rev_map (fun e -> String.concat " " e.vert) ctr_list
let _  = List.rev_map (fun e -> String.concat " " e.indicatif) ctr_list
let _  = ListLabels.rev_map  ctr_list
                             ~f:(fun e ->
                               String.concat " " e.lat
                               |> Re.split tiret
                               |> List.rev_map String.trim
                               |> List.rev
                               |> List.iter ( fun p ->
                                              try
                                                let res = p
                                                          |> Re.split (Re.compile @@ Re.char ',')
                                                          |> List.map String.trim in
                                                let p1 = List.hd res
                                                and p2 = List.tl res |> List.hd in

                                                let c = Point.of_string1 p |> Point.to_string2 in
                                                ()
                                              with
                                                 _ ->
                                                begin
                                                  try
                                                    let _ = Scanf.sscanf p "arc horaire de %f NM de rayon centré sur %r" Point.scan1 (fun d p -> d, p) in ()
                                                  with
                                                    Scanf.Scan_failure _ -> print_endline p
                                                end
                                            )

                             )

let _ = Scanf.sscanf "arc horaire de 11.5 NM de rayon centré sur 45°44'44\"N , 005°05'26\"E" "arc horaire de %f NM de rayon centré sur %r" Point.scan1 (fun d p -> d, p)

"cercle de %f NM de rayon centré sur %r" Point.scan1 (fun d p -> d, p)
"horaire de %f NM de rayon centré sur %r"

(*
#require "nettls-gnutls";;
#require "xmlm";;
#load"webr.cmo";;
 *)

open Webr

(* Combinateur fonctionnel utile *)
let flip f a b = f b a

(*
*)

(* Je récupère l’eAIP en cours avec une expression régulière, parce
   que le XML généré par le SIA est mal formé, et ne passe pas avec
   un parseur correct… *)
let current_url_base, current_name =
  let homepage =
    (*
<a href="https://www.sia.aviation-civile.gouv.fr/documents/htmlshow?f=dvd/eAIP_02_MAR_2017/FRANCE/home.html" title="">eAIP FRANCE</a>
 *)
    let refr = Re_str.regexp {|.*<a href="https://www.sia.aviation-civile.gouv.fr/documents/htmlshow\?.*=\(.*\)" title="">eAIP FRANCE</a>|} in
    let hm = Nethttp_client.Convenience.http_get "https://www.sia.aviation-civile.gouv.fr" in
    let _ = Re_str.search_forward refr hm 0 in
    let tail = Re_str.matched_group 1 hm in

    (* Marche pas. Il faut faire une requete plus compliquée, avec des paramètres... *)
    Nethttp_client.Convenience.http_get (Printf.sprintf {|https://www.sia.aviation-civile.gouv.fr/%s|} tail) in

  let regexp = Re_str.regexp ".*<td class=\"green\"><a href=\"\\(.*\\)index-fr-FR.html\" target=\"_blank\">\\(.*\\)</a></td>" in
  let _ = Re_str.search_forward regexp homepage 0 in
  "https://www.sia.aviation-civile.gouv.fr/aip/enligne/FRANCE/" ^ Re_str.matched_group 1 homepage, Re_str.matched_group 2 homepage

    https://www.sia.aviation-civile.gouv.fr/dvd/eAIP_30_MAR_2017/FRANCE/AIRAC-2017-03-30/html/index-fr-FR.html
            https://www.sia.aviation-civile.gouv.fr/dvd/eAIP_30_MAR_2017/FRANCE/AIRAC-2017-03-30/html/eAIP/FR-AD-1.6-fr-FR.html#AD-1.6

(* Récupération de la section x de l’eAIP sous la forme d’un arbre
   xml *)
let get_section x =
  let url = Printf.sprintf "%seAIP/FR-%s-fr-FR.html" current_url_base x in
  Nethttp_client.Convenience.http_get url
  |> begin fun xmlstring -> Xmlm.make_input (`String (0, xmlstring)) end
  |> tree_of_xmlinput

(* Extraction des données d'un tree, avec nettoyage des espaces *)
let rec extract_data ?(hide_void=true) ?(hide_spaces=true) = function
  | D "" when hide_void -> []
  | D "\194\160" when hide_spaces -> []
  | D " " when hide_spaces -> []
  | D d -> [Bytes.trim d]
  | E("del",_, childs)
  | E("br",_, childs)
    -> []
  | E("ins",_,childs)
  | E("span",_,childs)
  | E("td",_,childs)
  | E("tr",_,childs) ->
    List.rev_map (extract_data ~hide_void ~hide_spaces) childs
    |> List.rev
    |> List.flatten
  | E(e,_,_) ->
    Printf.printf "Problem : %s\n%!" e;
    assert false

(*********************************)
(*   ZONE REGLEMENTE TEMPORAIRE  *)
(*     SUP AIP France            *)
(*********************************)

(**************************************)
(*   MOYENS DE RADIOCOMMUNICATION ATS *)
(*      AIP France - AD 1.6           *)
(**************************************)


module SS = Set.Make(String)

let moyens_communication =
  let blank = Re.compile Re.blank in
  let has_class motif n =
    match attribute "class" n with
    | None -> false
    | Some valeurs ->
      valeurs = motif ||
      String.trim valeurs |> Re.split blank |> List.mem motif in
  get_section eaip_addr "AD-1.6"
  |> select {|body div[id="AD-1.6"] > table > tbody tr|}
  |> fold (fun (next, accu) tr ->
      if has_class "keep-with-next-row" tr
      then (tr::next, accu)
      else ([], (List.rev (tr::next)) :: accu)
    ) ([],[])
  |> snd
  |> List.rev

let extract_nom_et_indicatif mc =
  List.hd mc
  |> texts
  |> List.rev_map String.trim
  |> List.filter (fun elt -> elt <> "" && elt <> "(" && elt <> ")")
  |> (fun mots -> List.tl mots |> String.concat " ", List.hd mots)

(* (bytes * bytes list list) list

   Chaque moyen de communication a un nom,
   et une liste de bytes list.

   Les colonnes des bytes list sont :
   [Service, Indicatif d'appel (Call-sign), FREQ, HOR, Observations (Remarks)]  *)
let moyens_communication =

  (* Détection start moyen communication *)
  let is_moyen_com_name r =
    r
    |> exactly_one_element_child_by_name "td"
    |> has_attribute "colspan" "5" in

  (* Construction de la liste des moyens de communication *)
  let rec coms_rec curr accu = function
    | [] -> (List.rev curr)::accu
    | hd :: tl ->
      if is_moyen_com_name hd
      then coms_rec [hd] ((List.rev curr)::accu) tl
      else coms_rec (hd::curr) accu tl in

  get_section "AD-1.6"
  |> exactly_one_element_child_by_name "body"
  |> element_childs_by_name "div"
  |> List.filter (has_attribute "id" "AD-1")
  |> List.hd
  |> element_childs_by_name "div"
  |> List.filter (has_attribute "id" "AD-1.6")
  |> List.hd
  |> exactly_one_element_child_by_name "table"
  |> exactly_one_element_child_by_name "tbody"
  |> element_childs_by_name "tr"
  |> coms_rec [] []
  |> List.filter ((<>) [])
  |> List.rev_map
    begin fun com ->
      match com with
      | [] -> assert false
      | hd :: tl ->

        (* Mise en forme du nom *)
        ( hd
          |> exactly_one_element_child_by_name "td"
          |> extract_data
          |> List.map (function
              | "(" -> " ("
              | autre -> autre)
          |> String.concat ""),

        (* Mise en forme du tableau de valeurs *)
        ( tl
          |> List.rev_map (element_childs_by_name "td")
          |> List.rev_map (fun x ->
              List.rev_map extract_data x
              |> List.rev_map (function [] -> "" | x -> String.concat " " x)
              |> List.map Bytes.trim
            )
        )
    end
  |> List.filter (fun (_, d) -> d <> [])

(* |> List.filter (fun (n, _) -> n.[0] = 'D') *)
(* |> List.iter (fun (n,d) -> *)
(*     Printf.printf "%s:\n" n; *)
(*     List.iter (fun e -> Printf.printf "  %s\n" (List.nth e 4)) d *)
(*   ) *)

(* |> List.filter (fun x -> *)
(*     Re_str.string_match (Re_str.regexp ".*TWR") (List.hd x) 0) *)
(* |> List.filter (fun x -> *)
(*     Re_str.string_match (Re_str.regexp ".*Tour") (List.nth x 1) 0) *)
(* |> List.filter (fun x -> *)
(*     not (Re_str.string_match (Re_str.regexp ".*Réservée MIL") (List.nth x 4) 0)) *)
(* |> List.filter (fun x -> *)
(*     not (Re_str.string_match (Re_str.regexp ".*Utilisation suspendue.") (List.nth x 4) 0)) *)
(* |> List.filter (fun x -> *)
(*     not (Re_str.string_match (Re_str.regexp ".*Zone OTAN") (List.nth x 4) 0)) *)
(* |> List.filter (fun x -> *)
(*     not (Re_str.string_match (Re_str.regexp ".*Pour décollage") (List.nth x 4) 0)) *)
(* |> List.filter (fun x -> *)
(*     not (Re_str.string_match (Re_str.regexp ".*Roulage") (List.nth x 4) 0)) *)

let cherche_radio nm =
  List.filter
    (fun (nom, _) -> Re_str.string_match (Re_str.regexp (".*" ^ nm)) nom 0)
    moyens_communication

(*********************************)
(*   CONTROL TRAFFIC AREAS (CTR) *)
(*      AIP France - AD 1.7      *)
(*********************************)

type ctr_datas = {
  nom: bytes;
  lat: bytes;
  classe: bytes;
  vert: bytes;
  indicatif: bytes;
  observations: bytes
}

(* Tree list 2 :  ZONES DE CONTROLE (CTR) *)
let tl2 =

  let ctr_name l =
    l
    |> exactly_one_element_child_by_name "td"
    |> extract_data
    |> List.filter ((<>) "")
    |> String.concat " " in

  let tl2_datas r =
    r
    |> element_childs_by_name "td"
    |> List.map extract_data
    |> List.map (List.filter ((<>) ""))
    |> List.filter ((<>) []) in

  let ctr_datas_of_string_list_list_list l = match l with
    | [[nom]; lat; [classe]; vert; indicatif; observations] ->
      {
        nom = nom;
        lat = String.concat "" lat;
        classe = classe;
        vert = String.concat "\n" vert;
        indicatif = String.concat "\n" indicatif;
        observations = String.concat "\n" observations
      }
    | _ -> assert false in

  let associate_name_and_data r =

    let rec anad accu = function
      | Some n, []       -> assert false
      | None, hd :: tl   -> anad accu ((Some (ctr_name hd)), tl)
      | Some n, hd :: tl -> anad (( [n] :: tl2_datas hd) :: accu) (None, tl)
      | None, []         ->
        List.rev accu |> List.map ctr_datas_of_string_list_list_list in
    anad [] (None, r) in

  get_section "AD-1.7"
  |> exactly_one_element_child_by_name "body"
  |> element_childs_by_name "div"
  |> List.filter (has_attribute ~name:"id" ~value:"AD-1")
  |> List.hd
  |> element_childs_by_name "div"
  |> List.filter (has_attribute ~name:"id" ~value:"AD-1.7")
  |> List.hd
  |> exactly_one_element_child_by_name "table"
  |> exactly_one_element_child_by_name "tbody"
  |> element_childs_by_name "tr"
  |> associate_name_and_data

let alts t =
  let rec alts ah al = function
    | [] -> ah, al
    | "------------" :: tl -> alts ah tl []
    | x :: tl -> alts (x :: ah) al tl in
  alts [] [] t

(*
for i = 0 to 102 do
List.nth tl2 (i * 2 + 1) |> element_childs_by_name "td" |> flip List.nth 2 |> extract_data |> List.filter ((<>) "") |> alts |> (fun (a,b) -> Printf.printf "%03d : %s - %s\n" i (String.concat "" a) (String.concat " " b)) ; done;;
*)

let zone nm datas =

  let datas =
    datas
    |> element_childs_by_name "td" in

  let airclass =
    datas
    |> flip List.nth 1
    |> extract_data
    |> String.concat "" in

  let alth, altl =
    datas
    |> flip List.nth 2
    |> extract_data
    |> List.filter ((<>) "")
    |> alts in

  let open AirSpace in

  {
    default_zone with
    nm = nm;
    ac = airclass_of_string airclass;
  }

let ctrs l =
  let rec ctrs accu = function
    | _ :: [] -> assert false
    | [] -> List.rev accu
    | nm :: datas :: tl -> ctrs (zone (ctr_name nm) datas :: accu) tl in
  ctrs [] l

let xx = ctrs tl2

let essai =
  List.rev_map (fun ctr ->
      let first_name =
        Re_str.split (Re_str.regexp " ") ctr.AirSpace.nm
        |> List.filter ((<>) "LE")
        |> List.filter ((<>) "LA")
        |> List.filter ((<>) "SAINT")
        |> flip List.nth 1 in

      let first_name = match first_name with
        | "SAINT-YAN" -> "SAINT YAN"
        | "PAU-PYRENEES" -> "PAU PYRENEES"
        | _ -> first_name in
      ctr.AirSpace.nm, List.rev_map fst (cherche_radio first_name))
    xx

(* let essai =  *)

(*   let rec const accu = function *)
(*     | [] -> List.rev accu *)
(*     | hd :: content :: tl -> *)
(*       let accu =  *)

let essai =

  let ctr_name t =
    t
    |> exactly_one_element_child_by_name "td"
    |> extract_data
    |> String.concat " "
    |> Bytes.trim in

  let datas t =
    t
    |> element_childs_by_name "td"
    |> List.map extract_data
    |> List.map (Bytes.concat "") in

  let rec construct accu = function
    | [] -> List.rev accu
    | n :: d :: tl ->
      let [lat; classe; vert; indicatif; observations] = datas d in
      assert(classe = "D");
      let accu = {nom = ctr_name n; lat; vert; indicatif; observations} :: accu in
      construct accu tl
    | _ -> assert false in

  construct [] tl2

(* tl2 *)
(* |> List.map *)
(*   (fun l -> *)
(*      l *)
(*      |> element_childs_by_name "td" *)
(*      |> List.rev_map extract_data *)
(*      |> List.rev *)
(*      |> List.filter ((<>) []) *)
(*   ) *)
(* |> List.filter (fun x -> List.length x = 1) *)
(* |> List.map (fun x -> x |> List.hd |> String.concat " ") *)
(* |> List.map String.trim *)
(* |> List.iter print_endline *)

(* let essai = *)
(*   let is_ctr_name l = *)
(*     List.length l = 1 in *)

(*  (\* Construction de la liste des CTR *\) *)
(*   let rec ctrs_rec curr accu = function *)
(*     | [] -> (List.rev curr)::accu *)
(*     | hd :: tl -> *)
(*       if *)
(*         is_ctr_name hd *)
(*       then *)
(*         let hd' = hd in *)
(*         ctrs_rec [hd'] ((List.rev curr)::accu) tl *)
(*       else *)
(*         ctrs_rec (hd::curr) accu tl in *)

(*   tl2 *)
(*   |> List.map *)
(*     ( fun l -> *)
(*         l *)
(*         |> element_childs_by_name "td" *)
(*         |> List.rev_map extract_data *)
(*         |> List.filter ((<>) []) ) *)

(*   |> ctrs_rec [] [] *)

(* (\* Récupération du nom d’une CTR depuis une ligne de tl2 *\) *)
(* let ctr_name line = *)
(*   line *)
(*   |> exactly_one_element_child_by_name "td" *)
(*   |> exactly_one_element_child_by_name "span" *)
(*   |> element_childs_by_name "span" *)
(*   |> List.rev_map extract_data *)
(*   |> List.rev |> List.flatten *)
(*   |> List.filter ( (<>) "") *)
(*   |> String.concat " " *)

(* (\* Récupération des limites latérales d’une CTR depuis une ligne de *)
(*    tl2 *\) *)
(* let ctr_limites_laterales line = *)
(*   line *)
(*   |> exactly_one_element_child_by_name "td" *)
(*   |> extract_data *)
(*   |> Bytes.concat " " *)
(*   |> Re.split (Re.str " - " |> Re.compile) *)


(* (\* Récupération de la classe d’une CTR depuis une ligne de tl2 *\) *)
(* let ctr_class line = *)
(*   line *)
(*   |> element_childs_by_name "td" *)
(*   |> flip List.nth 1 *)
(*   |> extract_data *)

(* (\* Récupération des limites verticales d’une CTR depuis une ligne de *)
(*    tl2 *\) *)
(* let ctr_limites_verticales line = *)
(*   line *)
(*   |> element_childs_by_name "td" *)
(*   |> flip List.nth 2 *)
(*   |> extract_data *)

(* let ctr_indicatif line = *)
(*   line *)
(*   |> element_childs_by_name "td" *)
(*   |> flip List.nth 3 *)
(*   |> extract_data *)

(* let rec ctrs ?(accu=[]) = function *)
(*     | [] -> List.rev accu *)
(*     | nom :: desc :: tl -> *)
(*       let n = ctr_name nom in *)
(*       let l = ctr_limites_laterales desc in *)
(*       let v = ctr_limites_verticales desc in *)
(*       let i = ctr_indicatif desc in *)
(*       ctrs ~accu:((n, (l, v, i)) :: accu) tl *)
(*     | nom -> assert false *)

(* let limites_verticales_of_bytes_list bl = *)

(*   let rec list_index a ?(accu=0) = function *)
(*     | [] -> -1 *)
(*     | hd :: tl -> *)
(*       if hd = a *)
(*       then accu *)
(*       else list_index a ~accu:(succ accu) tl in *)

(*   let x = List.f *)

(********************************)
(* TERMINAL CONTROL AREAS (TMA) *)
(*     AIP France - ENR 2.3     *)
(********************************)

(* Tree list 3 : REGIONS DE CONTROLE TERMINALES (TMA) *)
let tl3 = get_section "ENR-2.3"
          |> exactly_one_element_child_by_name "body"
          |> element_childs_by_name "div"
          |> List.filter (has_attribute "id" "ENR-2")
          |> List.hd
          |> element_childs_by_name "div"
          |> List.filter (has_attribute "id" "ENR-2.3")
          |> List.hd
          |> exactly_one_element_child_by_name "table"
          |> exactly_one_element_child_by_name "tbody"
          |> element_childs_by_name "tr"

(******************************)
(* CONTROL TRAFIC AREAS (CTA) *)
(*    AIP France - ENR 2.4    *)
(******************************)

(* Tree list 4 : UTA, CTA *)
let tl4 = get_section "ENR-2.4"
          |> exactly_one_element_child_by_name "body"
          |> element_childs_by_name "div"
          |> List.filter (has_attribute "id" "ENR-2")
          |> List.hd
          |> element_childs_by_name "div"
          |> List.filter (has_attribute "id" "ENR-2.4")
          |> List.hd
          |> exactly_one_element_child_by_name "table"
          |> exactly_one_element_child_by_name "tbody"
          |> element_childs_by_name "tr"

(***************************)
(*   PROHIBITED AREAS (P)  *)
(*   AIP France - ENR 5.1  *)
(***************************)

(* Tree list 5 : ZONES INTERDITES, REGLEMENTEES ET DANGEREUSES *)
let tl5 = get_section "ENR-5.1"
          |> exactly_one_element_child_by_name "body"
          |> element_childs_by_name "div"
          |> List.filter (has_attribute "id" "ENR-5")
          |> List.hd
          |> element_childs_by_name "div"
          |> List.filter (has_attribute "id" "ENR-5.1")
          |> List.hd
          |> element_childs_by_name "table"
          |> flip List.nth 5
          |> exactly_one_element_child_by_name "tbody"
          |> element_childs_by_name "tr"

(********************************)
(* Zones Interdites Temporaires *)
(********************************)

(********************************)
(*     RESTRICTED AREAS (R)     *)
(*     AIP France - ENR 5.1     *)
(********************************)

(************************)
(* DANGER AREAS (D)     *)
(* AIP France - ENR 5.1 *)
(************************)

(****************************************)
(* SPORTING AND RECREATIONAL ACTIVITIES *)
(* AIP France - ENR 5.5                 *)
(****************************************)

(* Tree list 6 : ACTIVITES AERIENNES SPORTIVES ET RECREATIVES *)
let tl6 = get_section "ENR-5.5"
          |> exactly_one_element_child_by_name "body"
          |> element_childs_by_name "div"
          |> List.filter (has_attribute "id" "ENR-5")
          |> List.hd
          |> element_childs_by_name "div"
          |> List.filter (has_attribute "id" "ENR-5.5")
          |> List.hd
          |> exactly_one_element_child_by_name "table"
          |> exactly_one_element_child_by_name "tbody"
          |> element_childs_by_name "tr"

(**************************************)
(* NATIONAL PARKS AND NATURE RESERVES *)
(* AIP France - ENR 5.6               *)
(**************************************)

(* Tree list 7 : PARCS NATIONAUX ET RESERVES NATURELLES *)
let tl7 = get_section "ENR-5.6"
          |> exactly_one_element_child_by_name "body"
          |> element_childs_by_name "div"
          |> List.filter (has_attribute "id" "ENR-5")
          |> List.hd
          |> element_childs_by_name "div"
          |> List.filter (has_attribute "id" "ENR-5.6")
          |> List.hd
          |> exactly_one_element_child_by_name "table"
          |> exactly_one_element_child_by_name "tbody"
          |> element_childs_by_name "tr"


(***************)
(* Derogations *)
(***************)

(**************************)
(* DIVERS / MISCELLANEOUS *)
(**************************)
