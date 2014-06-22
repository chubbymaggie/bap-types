type t = V of int * string * Type.typ
type var = t

let hash (V (id, _   , _  )) = id
let name (V (_ , name, _  )) = name
let typ  (V (_ , _   , typ)) = typ

let equal = ( == )
let compare (V (x, _, _)) (V (y, _, _)) = compare x y

let new_var =
  let var_counter = ref 0 in
  begin
    fun name typ ->
      let id = !var_counter in
      if id = -1 then
        failwith "new_var: var_counter wrapped around"
      else begin
        var_counter := id + 1;
        V (id, name, typ)
      end
  end

let renew_var (V (_, name, typ)) = new_var name typ

let tmp_prefix = "T_"
let tmp_prefix_len = String.length tmp_prefix

let is_tmp_name name =
  String.length name > tmp_prefix_len
  && String.sub name 0 tmp_prefix_len = tmp_prefix

let is_tmp (V (_, name, _)) = is_tmp_name name

let new_tmp name typ =
  new_var (if is_tmp_name name then name else tmp_prefix ^ name) typ
