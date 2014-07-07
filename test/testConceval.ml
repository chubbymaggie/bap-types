open OUnit
open Conceval

let tests =
  "Conceval" >:::
  [
    "Memory" >:::
    [
      "Load without write" >:: (fun () ->
        let mem = Memory.empty in
        assert_equal
          (Memory.load mem (Bitvector.lit 0 8) Bil.LittleEndian (Type.Reg 32))
          None;
        );

      "Basic write and load" >:: (fun () ->
          let mem = Memory.store Memory.empty (Bitvector.lit 0 2)
              (Bitvector.lit 0xDEADBEEF 32) Bil.LittleEndian (Type.Reg 32) in
          assert_equal
            (Memory.load mem (Bitvector.lit 0 2) Bil.LittleEndian (Type.Reg 32))
            (Some (Bitvector.lit 0xDEADBEEF 32));
          assert_equal
            (Memory.load mem (Bitvector.lit 0 2) Bil.BigEndian (Type.Reg 32))
            (Some (Bitvector.lit 0xEFBEADDE 32));
        );

      "Read low" >:: (fun () ->
          let mem = Memory.store Memory.empty (Bitvector.lit 0 2)
              (Bitvector.lit 0xDEADBEEF 32) Bil.LittleEndian (Type.Reg 32) in
          assert_equal
            (Memory.load mem (Bitvector.lit 0 2) Bil.LittleEndian (Type.Reg 16))
            (Some (Bitvector.lit 0xBEEF 16));
          assert_equal
            (Memory.load mem (Bitvector.lit 0 2) Bil.BigEndian (Type.Reg 16))
            (Some (Bitvector.lit 0xEFBE 16));
        );

      "Read high" >:: (fun () ->
          let mem = Memory.store Memory.empty (Bitvector.lit 0 2)
              (Bitvector.lit 0xDEADBEEF 32) Bil.LittleEndian (Type.Reg 32) in
          assert_equal
            (Memory.load mem (Bitvector.lit 2 2) Bil.LittleEndian (Type.Reg 16))
            (Some (Bitvector.lit 0xDEAD 16));
          assert_equal
            (Memory.load mem (Bitvector.lit 2 2) Bil.BigEndian (Type.Reg 16))
            (Some (Bitvector.lit 0xADDE 16));
        );

      "Read middle" >:: (fun () ->
          let mem = Memory.store Memory.empty (Bitvector.lit 0 2)
              (Bitvector.lit 0xDEADBEEF 32) Bil.LittleEndian (Type.Reg 32) in
          assert_equal
            (Memory.load mem (Bitvector.lit 1 2) Bil.LittleEndian (Type.Reg 16))
            (Some (Bitvector.lit 0xADBE 16));
          assert_equal
            (Memory.load mem (Bitvector.lit 1 2) Bil.BigEndian (Type.Reg 16))
            (Some (Bitvector.lit 0xBEAD 16));
        );

      "Read low (bigendian)" >:: (fun () ->
          let mem = Memory.store Memory.empty (Bitvector.lit 0 2)
              (Bitvector.lit 0xEFBEADDE 32) Bil.BigEndian (Type.Reg 32) in
          assert_equal
            (Memory.load mem (Bitvector.lit 0 2) Bil.LittleEndian (Type.Reg 16))
            (Some (Bitvector.lit 0xBEEF 16));
          assert_equal
            (Memory.load mem (Bitvector.lit 0 2) Bil.BigEndian (Type.Reg 16))
            (Some (Bitvector.lit 0xEFBE 16));
        );

      "Read high (bigendian)" >:: (fun () ->
          let mem = Memory.store Memory.empty (Bitvector.lit 0 2)
              (Bitvector.lit 0xEFBEADDE 32) Bil.BigEndian (Type.Reg 32) in
          assert_equal
            (Memory.load mem (Bitvector.lit 2 2) Bil.LittleEndian (Type.Reg 16))
            (Some (Bitvector.lit 0xDEAD 16));
          assert_equal
            (Memory.load mem (Bitvector.lit 2 2) Bil.BigEndian (Type.Reg 16))
            (Some (Bitvector.lit 0xADDE 16));
        );

      "Read middle (bigendian)" >:: (fun () ->
          let mem = Memory.store Memory.empty (Bitvector.lit 0 2)
              (Bitvector.lit 0xEFBEADDE 32) Bil.BigEndian (Type.Reg 32) in
          assert_equal
            (Memory.load mem (Bitvector.lit 1 2) Bil.LittleEndian (Type.Reg 16))
            (Some (Bitvector.lit 0xADBE 16));
          assert_equal
            (Memory.load mem (Bitvector.lit 1 2) Bil.BigEndian (Type.Reg 16))
            (Some (Bitvector.lit 0xBEAD 16));
        );
    ]
  ]
