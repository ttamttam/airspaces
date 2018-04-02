(** GPS coordinates *)
type t

(** {1 Construct} *)

(** From a latitude and a longitude expressed in degrees *)
val of_deg : lat:float -> lon:float -> t

(** From a latitude and a longitude expressed in degrees, minutes and seconds *)
val of_dms : lat:int * int * float -> lon:int * int * float -> t

(** From a cartesian coordinates tuple expressed in meters *)
val of_xyz : float * float * float -> t

val of_dms_string1 : string -> t
(** Examples:
    - 10°20'30"N , 040°50'55"W
    - 10°20'30"S , 040°50'55"E *)

val of_dms_string2 : string -> t
(** Examples:
    - 10:20:30 N 040:50:55 W
    - 10:20:30 S 040:50:55 E *)

(** {1 Access latitude and longitude} *)

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

val pp: Format.formatter -> t -> unit

val to_deg : t -> float * float
val to_rad : t -> float * float

(** To a cartesian coordinate tuple (in m) *)
val to_xyz : t -> float * float * float

(** {1 Distance} *)

(** Vincenty distance *)
val distance : t -> t -> float
(** in m *)
