open Core_kernel.Std

module V = Var
open Bil
open Stmt_piqi
open Type

let casttype_to_piqi : Type.cast_type -> Stmt_piqi.cast_type = function
  | CAST_UNSIGNED -> `cast_unsigned
  | CAST_SIGNED -> `cast_signed
  | CAST_HIGH -> `cast_high
  | CAST_LOW -> `cast_low

let casttype_of_piqi = function
  | `cast_unsigned -> CAST_UNSIGNED
  | `cast_signed -> CAST_SIGNED
  | `cast_high -> CAST_HIGH
  | `cast_low -> CAST_LOW

let unop_to_piqi : Type.unop_type -> Stmt_piqi.unop_type = function
  | NEG -> `uneg
  | NOT -> `unot

let unop_of_piqi = function
  | `uneg -> NEG
  | `unot -> NOT

let binop_to_piqi : Type.binop_type -> Stmt_piqi.binop_type = function
  | PLUS -> `plus
  | MINUS -> `minus
  | TIMES -> `times
  | DIVIDE -> `divide
  | SDIVIDE -> `sdivide
  | MOD -> `modbop
  | SMOD -> `smod
  | LSHIFT -> `lshift
  | RSHIFT -> `rshift
  | ARSHIFT -> `arshift
  | AND -> `andbop
  | OR -> `orbop
  | XOR -> `xor
  | EQ -> `eq
  | NEQ -> `neq
  | LT -> `lt
  | LE -> `le
  | SLT -> `slt
  | SLE -> `sle

let binop_of_piqi = function
  | `plus -> PLUS
  | `minus -> MINUS
  | `times -> TIMES
  | `divide -> DIVIDE
  | `sdivide -> SDIVIDE
  | `modbop -> MOD
  | `smod -> SMOD
  | `lshift -> LSHIFT
  | `rshift -> RSHIFT
  | `arshift -> ARSHIFT
  | `andbop -> AND
  | `orbop -> OR
  | `xor -> XOR
  | `eq -> EQ
  | `neq -> NEQ
  | `lt -> LT
  | `le -> LE
  | `slt -> SLT
  | `sle -> SLE

let rec type_to_piqi : Type.typ -> Stmt_piqi.typ = function
  | Reg n -> `reg n
  | TMem (t, t') -> `tmem {Tmem.index_type=type_to_piqi t; element_type=type_to_piqi t';}

let rec type_of_piqi = function
  | `reg n -> Reg n
  | `tmem {Tmem.index_type; element_type} ->
    TMem (type_of_piqi index_type, type_of_piqi element_type)

let var_to_piqi v : Stmt_piqi.var =
  {Var.name=V.name v; Var.id=V.hash v; Var.typ = type_to_piqi (V.typ v)}

let var_of_piqi {Var.name; id; typ} =
  V.V (id, name, type_of_piqi typ)

let endianness_to_piqi : Bil.endian -> Stmt_piqi.endian = function
  | LittleEndian -> `little_endian
  | BigEndian -> `big_endian

let endianness_of_piqi = function
  | `little_endian -> LittleEndian
  | `big_endian -> BigEndian

let rec exp_to_piqi : Bil.exp -> Stmt_piqi.exp = function
  | Load (m, i, e, t) ->
    let m = exp_to_piqi m in
    let i = exp_to_piqi i in
    let e = endianness_to_piqi e in
    let t = type_to_piqi t in
    `load {Load.memory=m; address=i; endian=e; typ=t;}
  | Store (m, i, v, e, t) ->
    let m = exp_to_piqi m in
    let i = exp_to_piqi i in
    let v = exp_to_piqi v in
    let e = endianness_to_piqi e in
    let t = type_to_piqi t in
    `store {Store.memory=m; address=i; value=v; endian=e; typ=t;}
  | BinOp (bop, e1, e2) ->
    let bop = binop_to_piqi bop in
    let e1 = exp_to_piqi e1 in
    let e2 = exp_to_piqi e2 in
    `binop {Binop.op=bop; lexp=e1; rexp=e2;}
  | UnOp (uop, e) ->
    let uop = unop_to_piqi uop in
    let e = exp_to_piqi e in
    `unop {Unop.op=uop; exp=e}
  | Var v ->
    `var (var_to_piqi v)
  | Int i ->
    let i = String.of_char_list (Bitvector.bytes_of i) in
    `inte {Inte.int=i;}
  | Cast (ct, t, e) ->
    let ct = casttype_to_piqi ct in
    let t = type_to_piqi t in
    let e = exp_to_piqi e in
    `cast {Cast.cast_type=ct; new_type=t; exp=e}
  | Let (v, e, e') ->
    let v = var_to_piqi v in
    let e = exp_to_piqi e in
    let e' = exp_to_piqi e' in
    `let_exp {Let_exp.bound_var=v; definition=e; open_exp=e'}
  | Unknown (s, t) ->
    let t = type_to_piqi t in
    `unknown {Unknown.descr=s; typ=t}
  | Ite (e, te, fe) ->
    let e = exp_to_piqi e in
    let te = exp_to_piqi te in
    let fe = exp_to_piqi fe in
    `ite {Ite.condition=e; iftrue=te; iffalse=fe}
  | Extract (h, l, e) ->
    let e = exp_to_piqi e in
    `extract {Extract.hbit=h; lbit=l; exp=e}
  | Concat (e1, e2) ->
    let e1 = exp_to_piqi e1 in
    let e2 = exp_to_piqi e2 in
    `concat {Concat.lexp=e1; rexp=e2}

let rec exp_of_piqi = function
  | `load {Load.memory; address; endian; typ} ->
    let m = exp_of_piqi memory in
    let i = exp_of_piqi address in
    let e = endianness_of_piqi endian in
    let t = type_of_piqi typ in
    Load (m, i, e, t)
  | `store {Store.memory; address; value; endian; typ} ->
    let m = exp_of_piqi memory in
    let i = exp_of_piqi address in
    let v = exp_of_piqi value in
    let e = endianness_of_piqi endian in
    let t = type_of_piqi typ in
    Store (m, i, v, e, t)
  | `binop {Binop.op; lexp; rexp} ->
    let bop = binop_of_piqi op in
    let e1 = exp_of_piqi lexp in
    let e2 = exp_of_piqi rexp in
    BinOp (bop, e1, e2)
  | `unop {Unop.op; exp} ->
    let uop = unop_of_piqi op in
    let e = exp_of_piqi exp in
    UnOp (uop, e)
  | `var v ->
    Var (var_of_piqi v)
  | `inte {Inte.int=i} ->
    let i = Bitvector.of_bytes (String.to_list i) in
    Int i
  | `cast {Cast.cast_type; new_type; exp} ->
    let ct = casttype_of_piqi cast_type in
    let t = type_of_piqi new_type in
    let e = exp_of_piqi exp in
    Cast (ct, t, e)
  | `let_exp {Let_exp.bound_var; definition; open_exp} ->
    let v = var_of_piqi bound_var in
    let d = exp_of_piqi definition in
    let e = exp_of_piqi open_exp in
    Let (v, d, e)
  | `unknown {Unknown.descr; typ} ->
    let t = type_of_piqi typ in
    Unknown (descr, t)
  | `ite {Ite.condition; iftrue; iffalse} ->
    let cond = exp_of_piqi condition in
    let te = exp_of_piqi iftrue in
    let fe = exp_of_piqi iffalse in
    Ite (cond, te, fe)
  | `extract {Extract.hbit; lbit; exp} ->
    let e = exp_of_piqi exp in
    Extract (hbit, lbit, e)
  | `concat {Concat.lexp; rexp} ->
    let e1 = exp_of_piqi lexp in
    let e2 = exp_of_piqi rexp in
    Concat (e1, e2)

let rec stmt_to_piqi : Bil.stmt -> Stmt_piqi.stmt = function
  | Move (v, e) ->
    let v = var_to_piqi v in
    let e = exp_to_piqi e in
    `move {Move.lvar=v; rexp=e}
  | Jmp targ ->
    let targ = exp_to_piqi targ in
    `jmp {Jmp.target=targ}
  | Special s -> `special s
  | While (e, stmts) ->
    let e = exp_to_piqi e in
    let stmts = stmts_to_piqi stmts in
    `while_stmt {While_stmt.cond=e; loop_body=stmts}
  | If (e, then_branch, else_branch) ->
    let e = exp_to_piqi e in
    let then_branch = stmts_to_piqi then_branch in
    let else_branch = stmts_to_piqi else_branch in
    `if_stmt {If_stmt.cond=e; true_branch=then_branch; false_branch=else_branch}
  | CpuExn n -> `cpuexn {Cpuexn.errno=n}

and stmts_to_piqi l = List.map ~f:stmt_to_piqi l

let rec stmt_of_piqi = function
  | `move {Move.lvar; rexp} ->
    let v = var_of_piqi lvar in
    let e = exp_of_piqi rexp in
    Move (v, e)
  | `jmp {Jmp.target} ->
    let t = exp_of_piqi target in
    Jmp t
  | `special s -> Special s
  | `while_stmt {While_stmt.cond; loop_body} ->
    let e = exp_of_piqi cond in
    let b = stmts_of_piqi loop_body in
    While (e, b)
  | `if_stmt {If_stmt.cond; true_branch; false_branch} ->
    let e = exp_of_piqi cond in
    let then_branch = stmts_of_piqi true_branch in
    let else_branch = stmts_of_piqi false_branch in
    If (e, then_branch, else_branch)
  | `cpuexn {Cpuexn.errno} -> CpuExn errno

and stmts_of_piqi l = List.map ~f:stmt_of_piqi l

let to_pb p   = Stmt_piqi_ext.gen_stmt (stmt_to_piqi p) `pb
let to_json p = Stmt_piqi_ext.gen_stmt (stmt_to_piqi p) `json_pretty
let to_xml p  = Stmt_piqi_ext.gen_stmt (stmt_to_piqi p) `xml_pretty

let pb_of_stmts p = Stmt_piqi_ext.gen_stmt_list (stmts_to_piqi p) `pb
let json_of_stmts p = Stmt_piqi_ext.gen_stmt_list (stmts_to_piqi p) `json_pretty
let xml_of_stmts p = Stmt_piqi_ext.gen_stmt_list (stmts_to_piqi p) `xml_pretty

let bil_of_pb filename =
  stmts_of_piqi (In_channel.with_file filename ~binary:true
                  ~f:(fun ic -> Stmt_piqi_ext.parse_stmt_list (In_channel.input_all ic) `pb))

let bil_of_json filename =
  stmts_of_piqi (In_channel.with_file filename ~binary:true
                  ~f:(fun ic -> Stmt_piqi_ext.parse_stmt_list (In_channel.input_all ic) `json))

let bil_of_xml filename =
  stmts_of_piqi (In_channel.with_file filename ~binary:true
                  ~f:(fun ic -> Stmt_piqi_ext.parse_stmt_list (In_channel.input_all ic) `xml))
