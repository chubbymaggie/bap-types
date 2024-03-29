open Core_kernel.Std
open Or_error

type endian = LittleEndian | BigEndian
with bin_io, compare, sexp

exception Width with sexp

module Bignum = struct
  module Repr : Stringable with type t = Z.t = struct
    type t = Z.t
    let to_string z = sprintf "0x%s" (Z.format "%X" z)
    let of_string = Z.of_string
  end
  include Z
  include Sexpable.Of_stringable(Repr)
  include Binable.Of_stringable(Repr)
  include (Repr : Stringable with type t := Z.t)
end

type bignum = Bignum.t

module type Compare = sig
  val compare: int -> int -> int
end

module Size_poly = struct
  let compare = Int.compare
end

module Size_mono = struct
  let compare x y = if x <> y then raise Width else 0
end

module Internal = struct
  type t = {
    z : Bignum.t;
    w : int;
    signed : bool;
  } with bin_io, sexp
end

module type Kernel = sig
  type t = Internal.t with bin_io, compare, sexp
  val create : bignum -> int -> t
  val signed : t -> t
  val is_signed: t -> bool
  val z : t -> bignum
  val with_z : t -> bignum -> t
  val lift1 : (bignum -> bignum) -> t -> t
  val lift2 : (bignum -> bignum -> bignum) -> t -> t -> t
  val unop  : (bignum -> 'a) -> t -> 'a
  val binop : (bignum -> bignum -> 'a) -> t -> t -> 'a
  val bitsub : ?hi:int -> ?lo:int -> t -> t Or_error.t
  val bitwidth : t -> int
  val bits_of_z  : t -> string
  val compare  : t -> t -> int
  val hash : t -> int
  val module_name : string
  include Stringable with type t := t
end


(** internal representation *)
module Make(Size : Compare) : Kernel = struct
  type t = Internal.t with bin_io, sexp
  open Internal

  let module_name = "Bap_types.Bap_bitvector"

  let znorm z w = Bignum.(z land ((one lsl w) - one))

  (** Extend that sign bit.
    * This makes Zarith treat the normalized input as if it were signed. *)
  let signed_z t : Bignum.t = Bignum.signed_extract t.z 0 t.w

  let z t = t.z

  let create z w = {z = znorm z w; w; signed=false}

  let hash = Hashtbl.hash

  let signed t = { t with signed = true }
  let is_signed t = t.signed

  let z t = t.z
  let with_z t z = { t with z = znorm z t.w }
  let bitwidth t = t.w
  let bits_of_z t = Bignum.to_bits t.z

  let unop op t =
    op (if t.signed then signed_z t else z t)

  let binop op t1 t2 =
    let z = if t1.signed || t2.signed then signed_z else z in
    op (z t1) (z t2)

  let lift1 op t = with_z t (unop op t)
  let lift2 op t1 t2 = create (binop op t1 t2) t1.w

  let compare l r =
    let s = binop Bignum.compare l r in
    if s <> 0 then s else Size.compare l.w r.w

  let to_string = function
    | {z; w=1} -> if Bignum.equal z Bignum.zero then "false" else "true"
    | {z; w=n} -> Bignum.to_string z ^ ":" ^ string_of_int n

  let of_string = function
    | "false" -> create Bignum.zero 1
    | "true"  -> create Bignum.one  1
    | s -> match String.split ~on:':' s with
      | [z; n] -> create (Bignum.of_string z) (Int.of_string n)
      | _ -> failwith "Bitvector.of_string"


  let with_validation t ~f = Or_error.map ~f (Validate.result t)

  let bitsub ?hi ?(lo=0) t =
    let n = bitwidth t in
    let hi = Option.value ~default:(n-1) hi in
    let extract = if t.signed then
        Bignum.signed_extract else
        Bignum.extract in
    let do_extract () =
      let len = hi-lo+1 in
      create (extract t.z lo len) len in
    with_validation ~f:do_extract
      Validate.(name_list "bitsub" [
          name "(hi >= 0)"    @@ Int.validate_non_negative hi;
          name "(lo >= 0)"    @@ Int.validate_non_negative lo;
          name "(hi > lo)"    @@ Int.validate_positive (hi - lo);
          name "(hi < width)" @@ Int.validate_positive (n - hi);
        ])

end

(* About monomorphic comparison.

   With monomorphic size comparison functions [hash] and [compare]
   will not be coherent with each other, since we can't prohibit
   someone to compare hashes from bitvectors with different sizes. For
   example, it means, that we can't really guarantee that in a Table
   all keys are bitvectors with the same size. So, as a consequence we
   can't make bitvector a real value implementing [Identifiable]
   interface. Since, monomorphic behaviour is rather specific and
   unintuitive we will move it in a separate submodule and use size
   polymorphic comparison by default.
*)
module T = Make(Size_poly)
include T

let b0 = create (Bignum.of_int 0) 1
let b1 = create (Bignum.of_int 1) 1
let of_bool v = if v then b1 else b0

let of_int32 n = create (Bignum.of_int32 n) 32
let of_int64 n = create (Bignum.of_int64 n) 64
let of_int ~width v = create (Bignum.of_int v) width
let ones  n = of_int (-1) ~width:n
let zeros n = of_int (0)  ~width:n
let zero  n = of_int 0    ~width:n
let one   n = of_int 1    ~width:n


let safe f t = try_with (fun () -> f t)

let to_int   = unop (safe Bignum.to_int)
let to_int32 = unop (safe Bignum.to_int32)
let to_int64 = unop (safe Bignum.to_int64)

let of_binary ?width endian num  =
  let num = match endian with
    | LittleEndian -> num
    | BigEndian -> String.rev num in
  let w = Option.value width ~default:(String.length num * 8) in
  create (Bignum.of_bits num) w



let nsucc t n = with_z t Bignum.(z t + of_int n)
let npred t n = with_z t Bignum.(z t - of_int n)

let (++) t n = nsucc t n
let (--) t n = npred t n
let succ n = n ++ 1
let pred n = n -- 1

(* warning, concatination looses sign *)
let concat t1 t2 =
  let lhs = Bignum.shift_left (z t1) (bitwidth t2) in
  let w = bitwidth t1 + bitwidth t2 in
  let z = Bignum.add lhs (z t2) in
  create z w

let (@.) = concat

module Safe = struct
  include Or_error.Monad_infix

  type m = t Or_error.t

  let (!$) v = Ok v

  let validate_equal (n,m) : Validate.t =
    if m = n then Validate.pass
    else Validate.failf "expected width %d, but got %d" m n

  let lift m t : m =
    validate_equal (m, bitwidth t) |> Validate.result >>|
    fun () -> t

  let lift1 op x : m = x >>| lift1 op

  let lift2 op (x : m) (y : m) : m =
    x >>= fun x -> y >>= fun y ->
    let v = validate_equal (bitwidth x, bitwidth y) in
    Validate.result v >>| fun () -> lift2 op x y




  let int = lift
  let i1  = lift 1
  let i4  = lift 4
  let i8  = lift 8
  let i16 = lift 16
  let i32 = lift 32
  let i64 = lift 64
  let of_word_size = function
    | Word_size.W64 -> i64
    | Word_size.W32 -> i32

  module Base = struct
    type t = m
    let one = i1 (one 1)
    let zero = i1 (zero 1)
    let succ = lift1 Bignum.succ
    let pred = lift1 Bignum.pred
    let abs  = lift1 Bignum.abs
    let neg  = lift1 Bignum.neg

    let lnot = lift1 Bignum.lognot

    let logand = lift2 Bignum.logand
    let logor  = lift2 Bignum.logor
    let logxor = lift2 Bignum.logxor
    let add    = lift2 Bignum.add
    let sub    = lift2 Bignum.sub
    let mul    = lift2 Bignum.mul
    let sdiv   = lift2 Bignum.div
    let udiv   = lift2 Bignum.ediv
    let srem   = lift2 Bignum.rem
    let urem   = lift2 Bignum.erem

    let sign_disp ~signed ~unsigned x y =
      x >>= fun x -> y >>= fun y ->
      let op = if is_signed x || is_signed y then signed else unsigned in
      op !$x !$y

    let div = sign_disp ~signed:sdiv ~unsigned:udiv
    let rem = sign_disp ~signed:srem ~unsigned:urem
    let modulo  = rem

    let shift dir (x : m) (y : m) : m =
      x >>= fun x -> y >>= fun y ->
      if unop Bignum.fits_int y
      then Ok (with_z x ((dir (z x) (Bignum.to_int (z y)))))
      else Or_error.errorf
          "cannot perform shift, because rhs doesn't fit int: %s" @@
        to_string y

    let lshift = shift Bignum.shift_left
    let rshift = shift Bignum.shift_right

    let arshift (x : m) y : m =
      x >>= fun x -> rshift (Ok (signed x)) y
  end
  include Bap_integer.Make(Base)
end

module Int_exn = struct
  module Base = struct
    type t = T.t
    let one = one 1
    let zero = zero 1

    let succ = succ
    let pred = pred

    open Safe

    let exn1 op x = ok_exn (op !$x)
    let exn2 op x y = ok_exn (op !$x !$y)

    let abs = exn1 abs
    let neg = exn1 neg
    let add = exn2 add
    let sub = exn2 sub
    let mul = exn2 mul
    let div = exn2 div
    let modulo = exn2 modulo
    let lnot = exn1 lnot
    let logand = exn2 logand
    let logor  = exn2 logor
    let logxor = exn2 logxor
    let lshift = exn2 lshift
    let rshift = exn2 rshift
    let arshift = exn2 arshift
  end
  include Bap_integer.Make(Base)
end

let bitsub_exn ?hi ?lo z =
  Or_error.ok_exn @@ bitsub ?hi ?lo z

let is_zero = unop Bignum.(equal zero)
let is_positive = unop Bignum.(fun z -> gt z zero)
let is_non_positive  = Fn.non is_positive
let is_negative = unop Bignum.(fun z -> lt z zero)
let is_non_negative = Fn.non is_negative

let validate check msg x =
  if check x then Validate.pass
  else Validate.fails msg x sexp_of_t

let validate_positive =
  validate is_positive "should be positive"
let validate_non_positive =
  validate is_non_positive "should be non positive"
let validate_negative =
  validate is_negative "should be negative"
let validate_non_negative =
  validate is_non_negative "should be non negative"

let to_chars t endian  =
  let open Sequence in
  let n = (bitwidth t + 7) / 8 in
  if is_zero t then take (repeat '\x00') n
  else
    let bytes = bits_of_z t in
    let len = String.length bytes in
    let len_diff = len - n in
    let start,next,finish,cat,cut = match endian with
      | LittleEndian ->
        0, Int.succ, len,
        Fn.flip append, Fn.flip take (len - len_diff)
      | BigEndian ->
        len-1, Int.pred, ~-1,
        append, Fn.flip drop len_diff in
    let s = unfold ~init:start ~f:(fun i ->
        if i = finish then None
        else Some (bytes.[i], next i)) in
    if len_diff > 0 then cut s
    else
      let zeros = take (repeat '\x00') (-len_diff) in
      cat zeros s


let to_bytes t endian =
  Sequence.map (to_chars t endian) ~f:(fun c ->
      of_int ~width:8 (Char.to_int c))

let bits_of_byte byte =
  let byte = Char.to_int byte in
  Sequence.unfold ~init:7 ~f:(fun n ->
      if n < 0 then None
      else Some (byte land (1 lsl n) <> 0, n - 1))

let to_bits bv endian =
  to_chars bv endian |> Sequence.map ~f:bits_of_byte |> Sequence.concat

module Mono = Comparable.Make(Make(Size_mono))

include Or_error.Monad_infix
include Identifiable.Make(T)
module Int = Safe
