open Webr

(* Exécute [f ()], mesure le temps d’exécution en s, l’affiche, puis retourne le
   résultat de [f ()] *)
let duree f a =
  let t0 = Unix.time () in
  let v = f a in
  let dt = Unix.time () -. t0 in
  Printf.printf "Temps d'exécution : %.1f s\n%!" dt;
  v

(*
   http://overpass-turbo.eu/

   Exemple :

   rel[name="France métropolitaine (terres)"];
   rel(r:outer)[name!=Corse];
   >;
   out;

   Remarque : il y a un fichier openair sur ffvl.fr. Je ne sais pas si
   tous les parcs sont dedans ?

   Réserves/Parcs :
   rel[name="Réserve nationale de chasse et de faune sauvage des Bauges"];
   rel[name="Parco Nazionale Gran Paradiso"];

   Frontières :
   rel[name="France - Belgique / België / Belgien"];
   rel[name="France - Deutschland"];
   rel[name="France - España"];
   rel[name="France - Italia"];
   rel[name="France - Schweiz/Suisse/Svizzera/Svizra"]

   Visualisateur de fichier openair : http://cunimb.net/openair2map.php

*)


(* Extraction de données depuis la base de données openstreetmap avec
   l’api overpass, en utilisant le language QL *)
let recup_chemins () =
    Nethttp_client.Convenience.http_post
      "http://overpass-api.de/api/interpreter"
      [

        "data",
        (* On récupère la relation « France métropolitaine (terres) »
           relation Metropolitan France (lands) 1362232

           Dans : France - Limite côtière - Bouches-du-Rhône (13) = 1995472
           il manque : way  517022456
           J’ajoute la relation au chemin le 26 sep 2017
        *)
        "rel[name=\"France métropolitaine (terres)\"];\n"
        (* Dont on extrait les relations qui ont le rôle outer, excepté la Corse. *)
        ^ "rel(r:outer)[name!=Corse];\n"
        (* On prend tous les nœuds et chemins qui font partie des relations. *)
        ^ ">;\n"
        (* On rend ça en mode « Geom », pour récupérer les coordonnées
           des nœuds de chaque chemin. *)
        ^ "out geom;"
      ]

    (* On convertit ça en arbre [tree]. *)
    (* |> begin fun xmlstring -> Xmlm.make_input (`String (0, xmlstring)) end *)
    |> tree_of_xmlstring

    (* Puis on ne conserve que la liste des "way". *)
    |> get_childs_elt ~condition:(is_tag "way")

(* Un nœud « openstreetmap »  *)
type node = {id:int; lat: float; lon: float}

let northest_node n1 n2 = if n1.lat > n2.lat then n1 else n2

let tuple_of_node n = n.lon, n.lat

(* Un chemin « openstreetmap » *)
type way = {id:int; nodes: node list; end1: node; end2: node}

let noeud_of_nd id nd =
  let lon = attribute "lon" nd |> float_of_string
  and lat = attribute "lat" nd |> float_of_string in
  {id; lat; lon }

(* Transformation de [tree_list:tree list] en [way list] *)
let compacter tree_list =

  (* Module accessoire pour stocker les noeuds sans duplication *)
  let module Noeuds = MoreLabels.Map.Make(struct
      type t = int
      let compare : int -> int -> int = compare
    end) in

  let (_,northest, ways_list) =
    ListLabels.fold_left
      tree_list
      ~init:(Noeuds.empty, {id=0;lat=(-89.);lon=0.},[])
      ~f:begin
        fun (map_noeuds, northest, ways_list) way ->

          (* Identification du chemin *)
          let way_id = attribute "id" way |> int_of_string in

          (* Récupération des sous-noeuds de chaque [way] *)
          let sous_noeuds = get_childs_elt ~condition:(is_tag "nd") way in

          (* Construction d'un nouveau chemin et mise-à-jour de map_noeuds et
             northest *)
          let (map_noeuds, northest, w) =
            match sous_noeuds with
            | [] -> assert false
            | first_nd :: tl ->
              let first_id = attribute "ref" first_nd |> int_of_string in
              let first_node = noeud_of_nd first_id first_nd in
              ListLabels.fold_left
                ~init:( map_noeuds,
                        northest,
                        { id=way_id;
                          nodes = [first_node];
                          end1 = first_node;
                          end2 = first_node })
                ~f:begin
                  fun (map_noeuds, northest, w) nd ->
                    let id = attribute "ref" nd |> int_of_string in
                    let map_noeuds, current_node =
                      try
                        map_noeuds, Noeuds.find id map_noeuds
                      with
                        Not_found ->
                        let current_node = noeud_of_nd id nd in
                        Noeuds.add ~key:id ~data:current_node map_noeuds,
                        current_node
                    in
                    (map_noeuds,
                     northest_node current_node northest,
                     { w with
                       nodes = current_node :: w.nodes;
                       end1 = current_node})
                end
                sous_noeuds in

          (map_noeuds, northest, w :: ways_list)

      end
  in

  List.rev ways_list, northest

let reload_way_list_and_northest_node () : way list * node =
  let cache_name = "cache_way_list_and_northest_node" in

  let download_and_cache () =
    Printf.printf "Interrogation de la base de donnée OpenStreetMap. \
                   Ça peut prendre environ 5 min.\n%!";
    let res = duree recup_chemins () |> compacter in
    let oc = open_out_bin cache_name in
    Marshal.to_channel oc res [];
    close_out oc;
    res in

  if
    Sys.file_exists cache_name
  then
      let ic = open_in_bin cache_name in
    try
      Printf.printf "Chargement depuis '%s'\xe2\x80\xa6\n%!" cache_name;
      let res = (Marshal.from_channel ic : way list * node) in
      close_in ic;
      res
    with Failure _ ->
      begin
        close_in ic;
        Printf.printf "Problème lors du décodage de '%s'. %!" cache_name;
        download_and_cache ()
      end
  else
    begin
      Printf.printf "'%s' non trouvé. %!" cache_name;
      download_and_cache ()
    end

(* Les [ways:way list] peuvent être stocké à l’endroit, ou à l’envers… *)
type sens = F | R

let construct ?(dbg=false) first ways =
  let module IS = Set.Make(struct
      type t = int
      let compare : int -> int -> int = compare
    end
    ) in

  (* Tables de hachage pour récupérer les ways en utilisant leur premier ou
     dernier noeud *)
  let end1_ways_map, end2_ways_map =
    let e1 = Hashtbl.create 4000
    and e2 =  Hashtbl.create 4000 in
    List.iter
      (fun w ->
         Hashtbl.add e1 w.end1.id w;
         Hashtbl.add e2 w.end2.id w)
      ways;
    (e1, e2) in

  let rec cst accu seen_ways end_node =

    let accu, seen_ways, next_end_node =
      match
        Hashtbl.find_all end1_ways_map end_node
        |> List.filter (fun w -> not (IS.mem w.id seen_ways))
      with
      | w :: suite ->
        if dbg then
          Printf.printf "accu <- w%d (%d, %d) %s\n%!" w.id w.end1.id w.end2.id
            (if suite <> [] then "[X]" else "");

        (w, F) :: accu,
        IS.add w.id seen_ways,
        w.end2.id
      | [] ->
        begin
          match
            Hashtbl.find_all end2_ways_map end_node
            |> List.filter (fun w -> not (IS.mem w.id seen_ways))
          with
          | w :: suite ->
            if dbg then
              Printf.printf "accu <- w%d (%d, %d) %s\n%!"
                w.id w.end1.id w.end2.id (if suite <> [] then "[X]" else "");
            (w, R) :: accu,
            IS.add w.id seen_ways,
            w.end1.id
          | [] ->
            begin
              Printf.printf "No way could be found with node %d\n%!" end_node;
              raise Not_found
            end
        end in

    if
      first = next_end_node             (* FINI *)
    then
      List.rev accu
    else
      cst accu seen_ways next_end_node
  in

  cst [] IS.empty first

(* Retournement des chemins orientés, et applatissement en liste de
     noeuds *)
  let redresser contour =
    ListLabels.fold_left
      ~init:[]
      ~f:begin fun accu w ->
        match w with
        | ({nodes}, F) -> List.rev nodes :: accu
        | ({nodes}, R) -> nodes :: accu
      end
      contour
    |> List.flatten

let main () =
  let ways, northest = reload_way_list_and_northest_node () in
  let oc = open_out_bin "contour_france.ml" in

let dump =
  construct northest.id ways
  |> redresser
  |> List.rev_map tuple_of_node
  |> List.rev
  |> fun france -> Marshal.to_string france [] in

  Printf.fprintf oc
{foo|let dumped_france = {fr|%s|fr}

let france_ref = ref None

let france =
  let f () =
  match !france_ref with
  | None ->
    begin
      let fr = (Marshal.from_string dumped_france 0 : (float * float) list ) in
      france_ref := Some fr;
      fr
    end
  | Some fr -> fr in

  f ()
|foo} dump;
  (* construct northest.id ways *)
  (* |> redresser *)
  (* |> List.rev_map tuple_of_node *)



  (* |> List.rev_map (fun (la, lo) -> string_of_float la ^ "," ^ string_of_float lo) *)
  (* |> String.concat ";\n" *)
  (* |> output_string oc; *)

  (* output_string oc "]\n"; *)
  flush oc;
  close_out oc;
  exit 0

let _ = main ()
