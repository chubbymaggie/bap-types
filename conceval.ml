open Bil

exception Abort of string

(** Return the width of a register type. *)
let width_of typ = match typ with
  | Type.Reg n -> n
  | Type.TMem (_, _) -> raise (Abort "Only registers have a width.")

(** Given 8*n, return n.
  * useful for operating on memory. *)
let bits_to_bytes n =
  if n mod 8 <> 0 then raise (Abort "Width should be multiple of 8.")
  else n / 8

(* Comes before Memory module to make signatures play nice. *)
module MemoryMap = Map.Make (Bitvector)

(** Represents memory.
  * Supports: load and store byte sequences. *)
module rec Memory : sig
  type t = Value.value MemoryMap.t
  val empty: t
  val load : Value.value -> Value.value ->
    endian -> Type.typ -> Value.value option
  val store: Value.value -> Value.value -> Value.value ->
    endian -> Type.typ -> Value.value
end = struct
  open Value
  type t = value MemoryMap.t
  let empty = MemoryMap.empty
  let load mem idx endianness typ = match mem, idx with
    | (Mem mem, BV idx) ->
      (* This section is hard to read, but it's just pruning the tree
       * down to derived bounds and folding it into a bitvector. *)
      let left_bound = Bitvector.decr idx in
      let i_width = bits_to_bytes (width_of typ) in
      let width = Bitvector.lit i_width (Bitvector.width_of idx) in
      let right_bound = Bitvector.plus idx width in
      let mem =
        if Bitvector.compare Bitvector.bv_false idx <> 0 then
          let (_, _, mem) = MemoryMap.split left_bound mem in mem
        else mem in
      let mem =
        if Bitvector.compare Bitvector.bv_false right_bound <> 0 then
          let (mem, _, _) = MemoryMap.split right_bound mem in mem
        else mem in
      let data = MemoryMap.fold (fun key v acc -> (v::acc)) mem [] in
      if List.length data = i_width then Some (
          let data = match endianness with
            | LittleEndian -> data
            | BigEndian -> List.rev data in
          List.fold_left (fun a b -> match (a, b) with
              | (BV a), (BV b) -> BV (Bitvector.concat a b)
              | ((Un (_, _) as un), _) | (_, (Un (_, _) as un)) -> un
              | (_, _) -> raise (Abort "Memory byte has wrong type.")
            ) (BV (Bitvector.lit 0 0)) data
        )
      else None
    | (Mem _, (Un (_, _) as un)) -> Some un
    | (_, _) -> raise (Abort "Memory or index has wrong type.")
  let store mem idx v endianness typ = match mem, idx, v with
    | (Mem mem, BV idx, BV v) ->
      let v = match endianness with
        | BigEndian -> Bitvector.bytes_of v
        | LittleEndian -> List.rev (Bitvector.bytes_of v) in
      let v = List.map (fun x -> BV (Bitvector.of_bytes [x])) v in
      let (ret, _) = List.fold_right
          (fun byte (mem, i) -> (MemoryMap.add i byte mem), Bitvector.incr i)
          v (mem, idx) in
      Mem ret
    | (Mem mem, BV idx, (Un (_, _) as un)) ->
      let ret = ref mem in
      let ii = ref idx in
      for i = 1 to width_of typ do
        ret := MemoryMap.add !ii un !ret;
        ii := Bitvector.incr !ii
      done;
      Mem !ret
    | (Mem mem, Un (_, _), _) ->
      print_endline "Warning: writing to unknown memory index"; 
      Mem mem
    | (_, _, _) -> raise (Abort "Memory, index, or value has wrong type.")
end
and Value : sig
  type value = BV of Bitvector.t | Mem of Memory.t | Un of string * Type.typ
end = struct
  type value = BV of Bitvector.t | Mem of Memory.t | Un of string * Type.typ
end

include Value
open Value

module State = struct
  module StateMap = Map.Make(Var)
  type t = value StateMap.t
  let empty = StateMap.empty
  let move = StateMap.add
  let peek_exn = StateMap.find
  let peek var state =
    try let ret = StateMap.find var state in Some ret
    with Not_found -> None

  (** Remove all temporary variables from a state. *)
  let remove_tmp state = StateMap.filter (fun k _ -> not (Var.is_tmp k)) state
end

(** If v is a bitvector, perform some action on it.
  * Otherwise, handle the other value. *)
let bv_action_or_unknown v action =
  match v with
  | Mem _ -> raise (Abort "Operation cannot be performed on memory.")
  | Un (a, b) -> Un (a, b)
  | BV v -> action v

(** Handle a unary operator. *)
let handle_unop op v = let open Type in bv_action_or_unknown v
    (fun v -> match op with
       | NEG -> BV (Bitvector.neg v)
       | NOT -> BV (Bitvector.lognot v))

(** Handle a binary operator. *)
let handle_binop op l r =
  let open Type in
  match (l, r) with
  | (Mem _, _) | (_, Mem _) ->
    raise (Abort "Operation cannot be performed on memory.")
  | (Un (a, b), _) | (_, Un (a, b)) -> Un (a, b)
  | (BV l, BV r) -> BV (Bitvector.(match op with
      | PLUS    -> plus
      | MINUS   -> minus
      | TIMES   -> times
      | DIVIDE  -> divide
      | SDIVIDE -> sdivide
      | MOD     -> unsigned_mod
      | SMOD    -> signed_mod
      | LSHIFT  -> lshift
      | RSHIFT  -> rshift
      | ARSHIFT -> arshift
      | AND     -> logand
      | OR      -> logor
      | XOR     -> logxor
      | EQ      -> eq
      | NEQ     -> neq
      | LT      -> lt
      | LE      -> le
      | SLT     -> slt
      | SLE     -> sle) l r)

let handle_cast cast_kind new_type v =
  let open Type in bv_action_or_unknown v
    (fun v -> BV (Bitvector.(match cast_kind with
         | CAST_UNSIGNED -> cast_unsigned
         | CAST_SIGNED   -> cast_signed
         | CAST_HIGH     -> cast_high
         | CAST_LOW      -> cast_low) v (width_of new_type)))

(** Given state, evaluate a single BIL expression. *)
let rec eval_exp state exp =
  let result = match exp with
    | Load (arr, idx, endian, t) ->
      (match Memory.load (eval_exp state arr) (eval_exp state idx) endian t with
       | Some v -> v
       | None -> Un ("Load from unitialized memory", t))
    | Store (arr, idx, v, endian, t) ->
      Memory.store (eval_exp state arr) (eval_exp state idx) (eval_exp state v)
        endian t
    | BinOp (op, l, r) -> handle_binop op (eval_exp state l) (eval_exp state r)
    | UnOp (op, v) -> handle_unop op (eval_exp state v)
    | Var v -> (match State.peek v state with
        | Some v -> v
        | None -> raise (Abort (Printf.sprintf "No such variable %s" (Var.name v))))
    | Int v -> BV v
    | Cast (cast_kind, new_type, v) ->
      handle_cast cast_kind new_type (eval_exp state v)
    | Let (v, a, b) -> (* FIXME Should there be typechecking done here? *)
      let state = State.move v (eval_exp state a) state in
      eval_exp state b
    | Unknown (str, typ) -> Un (str, typ)
    | Ite (cond, t_case, f_case) ->
      bv_action_or_unknown (eval_exp state cond)
        (fun v ->
           if Bitvector.bool_of v then eval_exp state t_case
           else eval_exp state f_case)
    | Extract (h, l, v) -> bv_action_or_unknown (eval_exp state v)
                             (fun v -> BV (Bitvector.extract h l v))
    | Concat (l, r) -> (match eval_exp state l, eval_exp state r with
        | (Mem _, _) | (_, Mem _) ->
          raise (Abort "Operation cannot be performed on memory.")
        | ((Un (_, _) as un), _) | (_, (Un (_, _) as un)) -> un
        | (BV l, BV r) -> BV (Bitvector.concat l r))
  in result

(** Take a detailed state and a BIL statement and yield the successor state and
  * a location to jump to, if any. *)
let rec eval_stmt state = function
  | Move (v, exp) -> (State.move v (eval_exp state exp) state), None
  | Jmp (exp) -> state, Some (eval_exp state exp)
  | While (cond, stmts) ->
    (match eval_exp state cond with
     | Mem _ -> raise (Abort "Operation cannot be performed on memory.")
     | Un (a, b) -> raise (Abort "Condition in While loop is Unknown.")
     | BV v ->
       if Bitvector.bool_of v then
         let (state, addr) = (eval_stmt_list state stmts) in
         (match addr with
          | None -> eval_stmt state (While (cond, stmts))
          | Some addr as jump_to -> (state, jump_to))
       else (state, None))
  | If (cond, t_case_stmts, f_case_stmts) ->
    (match eval_exp state cond with
     | Mem _ ->
       raise (Abort "Operation cannot be performed on memory.")
     | Un (_, _) -> raise (Abort "Condition in If statement is Unknown.")
     | BV v -> eval_stmt_list state
                 (if Bitvector.bool_of v then t_case_stmts
                  else f_case_stmts))
  | Special str ->
    raise (Abort (Printf.sprintf "Aborting with Special '%s'" str))
  | CpuExn i -> raise (Abort (Printf.sprintf "Aborting with CpuExn %d" i))
(** Helper function:
  * evaluate a list of BIL statements from a starting state. *)
and eval_stmt_list state = function
  | [] -> state, None
  | (hd::tl) -> let (state, addr) = eval_stmt state hd in
    (match addr with
     | None -> eval_stmt_list state tl
     | Some _ as jump_to -> (state, jump_to))

(** Evaluate a list of instructions and discard temporary state,
  * as when evaluating an assembly instruction. *)
let eval_asm state instructions =
  let (state, addr) = eval_stmt_list state instructions in
  (State.remove_tmp state, addr)
