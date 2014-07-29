(** [to_pb p] converts [p] to protobuffer format. *)
val to_pb : Bil.stmt -> string

(** [to_json p] converts [p] to JSON format. *)
val to_json : Bil.stmt -> string

(** [to_xml p] converts [p] to XML format. *)
val to_xml : Bil.stmt -> string

val pb_of_stmts : Bil.stmt list -> string
val json_of_stmts : Bil.stmt list -> string
val xml_of_stmts : Bil.stmt list -> string
