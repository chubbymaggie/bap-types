type var = V of int * string * Type.typ
type t = var

let hash (V (id, _   , _  )) = id
let name (V (_ , name, _  )) = name
let typ  (V (_ , _   , typ)) = typ

let equal = ( == )
let compare (V (x, _, _)) (V (y, _, _)) = compare x y

let newvar =
  let varcounter = ref 0 in
  begin
    fun name typ ->
      let id = !varcounter in
      if id = -1
      then failwith "newvar: varcounter wrapped around"
      else varcounter := id + 1; V (id, name, typ)
  end

let renewvar (V (_, name, typ)) = newvar name typ
