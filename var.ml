type var = V of int * string * Type.typ
type t = var

let hash (V(i,_,_)) = i

let equal = ( == )

let compare (V(x,_,_)) (V(y,_,_)) = compare x y

let newvar =
  let varcounter = ref 0 in
  (fun s t ->
     let n = !varcounter in
     if n = -1 then failwith "newvar: counter wrapped around";
     (varcounter := n+1;
      V(n,s,t))
  )

let renewvar (V(_,name,t)) = newvar name t

let typ (V(_,_,t)) = t

let name (V(_,n,_)) = n
