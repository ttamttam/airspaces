(** GPS coordinates *)
type t

(** {1 Construct} *)

val of_deg : lat:float -> lon:float -> t
val of_dms : lat:int * int * float -> lon:int * int * float -> t

val of_dms_string1 : string -> t
(** Examples:
    - 10°20'30"N , 040°50'55"W
    - 10°20'30"S , 040°50'55"E *)

val of_dms_string2 : string -> t
(** Examples:
    - 10:20:30 N 040:50:55 W
    - 10:20:30 S 040:50:55 E *)

(** {1 Read} *)

val latitude_deg : t -> float
val latitude_rad : t -> float
val longitude_deg : t -> float
val longitude_rad : t -> float

(** {1 Export} *)

val to_dms : t -> (int * int * float) * (int * int * float)
val to_dms_string1 : t -> string
(** Examples:
    - 10°20'30"N , 040°50'55"W
    - 10°20'30"S , 040°50'55"E *)

val to_dms_string2 : t -> string
(** Examples:
    - 10:20:30 N 040:50:55 W
    - 10:20:30 S 040:50:55 E *)

val to_deg : t -> float * float
val to_rad : t -> float * float

(** {1 Distance} *)

(** Vincenty distance *)
val distance : t -> t -> float
(** in m *)
