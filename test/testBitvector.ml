open OUnit
open Core_kernel.Std

module B = Bitvector

let tests =
  "Bitvector" >:::
  [
    "normalize" >:: (fun () ->
        assert_equal (B.lit 13 7) (Bitvector.lit (13+1024) 7);
        assert_equal (B.lit 13 7) (Bitvector.lit (13-1024) 7);
        assert_equal (B.lit 3 2) (Bitvector.lit (4+3) 2);
        assert_equal (B.lit 3 2) (Bitvector.lit (3-4) 2);
      );
    "to_string" >:: (fun () ->
        assert_equal (B.to_string B.bv_false) "false";
        assert_equal (B.to_string B.bv_true) "true";
        assert_equal (B.to_string (B.lit 7 4)) "7:4";
        assert_equal (B.to_string (B.lit 7 3)) "7:3";
        assert_equal (B.to_string (B.lit 7 2)) "3:2";
      );
    "bool_of" >:: (fun () ->
        assert_equal (B.bool_of B.bv_false) false;
        assert_equal (B.bool_of B.bv_true) true;
      );
    "bytes_of" >:: (fun () ->
        assert_equal (B.bytes_of (B.lit 6373730 24)) ['b' ; 'A' ; 'a'];
        assert_equal (B.bytes_of (B.lit 0 32))
          ['\000' ; '\000' ; '\000' ; '\000'];
      );
    "of_bytes" >:: (fun () ->
        assert_equal (B.of_bytes ['b' ; 'A' ; 'a']) (B.lit 6373730 24);
      );
    "concat" >:: (fun () ->
        assert_equal (B.concat (B.lit 0xDEAD 16) (B.lit 0xBEEF 16))
          (B.lit64 0xDEADBEEFL 32);
        assert_equal (B.concat (B.lit 40 8) (B.lit 20 7)) (B.lit 5140 15);
        assert_equal (B.concat (B.lit 20 7) (B.lit 40 8)) (B.lit 5160 15);
      );
    "neg" >:: (fun () ->
        assert_equal (B.neg (B.lit 13 8)) (B.lit (-13) 8);
        assert_equal (B.neg (B.lit 1398765 4)) (B.lit (-1398765) 4);
      );
    "lognot" >:: (fun () ->
        assert_equal (B.lognot (B.lit 13 8)) (B.lit (-14) 8);
        assert_equal (B.lognot (B.lit 1398765 4)) (B.lit (-1398766) 4);
        assert_equal (B.lognot (B.lit 0 4)) (B.lit (-1) 4);
      );
    "compare" >:: (fun () ->
        assert_equal (B.compare (B.lit 13 8) (B.lit 14 8)) (-1);
        assert_equal (B.compare (B.lit 14 8) (B.lit 13 8)) 1;
        assert_equal (B.compare (B.lit 14 8) (B.lit 14 8)) 0;
      );
    "subtraction" >:: (fun () ->
        assert_equal (B.minus (B.lit 0 8) (B.lit 1 8)) (B.lit 0xFF 8)
      );
    "shift" >:: (fun () ->
        assert_equal (B.lshift (B.lit 1 8) (B.lit 10 5)) (B.lit 0 8)
      );
    "cast_high" >:: (fun () ->
        assert_equal (B.cast_high (B.lit 0xDAD5 16) 4) (B.lit 0xD 4)
      );
    "cast_low" >:: (fun () ->
        assert_equal (B.cast_low (B.lit 0xDAD5 16) 4) (B.lit 0x5 4)
      );
  ]
