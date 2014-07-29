(** [to_pb p] converts [p] to protobuffer format. *)
val to_pb : Bil.stmt -> string

(** [to_json p] converts [p] to JSON format. *)
val to_json : Bil.stmt -> string

(** [to_xml p] converts [p] to XML format. *)
val to_xml : Bil.stmt -> string

val pb_of_stmts : Bil.stmt list -> string
val json_of_stmts : Bil.stmt list -> string
val xml_of_stmts : Bil.stmt list -> string

(** all three of these functions take the name of a file
 *  previously output by Bil_piqi and read it in to get
 *  a BIL program *)
val bil_of_pb : string -> Bil.stmt list
val bil_of_json : string -> Bil.stmt list
val bil_of_xml : string -> Bil.stmt list
