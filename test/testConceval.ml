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
    ];
    "eval_exp" >:::
    [
      "BinOp" >:: (fun () ->
          let state = State.new_state () in
          assert_equal
            (eval_exp state Bil.(BinOp (Type.TIMES, Int (Bitvector.lit 3 8), Int (Bitvector.lit 3 8))))
            (BV (Bitvector.lit 9 8))
        );
      "UnOp" >:: (fun () ->
          let state = State.new_state () in
          assert_equal
            (eval_exp state Bil.(UnOp (Type.NOT, Int (Bitvector.lit 3 8))))
            (BV (Bitvector.lit (-4) 8))
        );
    ];
    "eval_stmt" >:::
    [
      "Move" >:: (fun () ->
          let state = State.new_state () in
          let var = Var.new_var "Garfield" (Type.Reg 8) in
          let state, _ =
            eval_stmt state Bil.(Move (var, (Int (Bitvector.lit 3 8)))) in
          assert_equal
            (State.peek var state)
            (BV (Bitvector.lit 3 8))
        );
      "While" >:: (fun () ->
          let var = Var.new_var "Daisy" (Type.Reg 8) in
          let state, _ = eval_stmt (State.new_state ())
              Bil.(Move (var, (Int (Bitvector.lit 0 8)))) in
          let state, _ =
            eval_stmt state Bil.(While (BinOp (Type.NEQ, Int (Bitvector.lit 101 8), Var var), [Move (var, (BinOp (Type.PLUS, Int (Bitvector.lit 1 8), Var var)))])) in
          assert_equal
            (State.peek var state)
            (BV (Bitvector.lit 101 8))
        );
      "collatz" >:: (fun () ->
          (* Starting with 17, we take 12 steps in the `3x+1` problem (OEIS). *)
          let v_steps = Var.new_var "# of steps" (Type.Reg 64) in
          let v_n = Var.new_var "T_n" (Type.Reg 64) in
          let state = State.new_state () in
          let state, _ =
            eval_asm state Bil.([
                Move (v_n, (Int (Bitvector.lit 17 64)));
                Move (v_steps, (Int (Bitvector.lit 0 64)));
                (While ((BinOp (Type.NEQ, Int (Bitvector.lit 1 64), Var v_n)),
                        [If ((BinOp (Type.EQ, Int (Bitvector.lit 0 64), BinOp (Type.MOD, Var v_n, Int (Bitvector.lit 2 64)))),
                             [Move (v_n, (BinOp (Type.DIVIDE, Var v_n, Int (Bitvector.lit 2 64))))],
                             [Move (v_n, (BinOp (Type.PLUS, (BinOp (Type.TIMES, Var v_n, Int (Bitvector.lit 3 64))), Int (Bitvector.lit 1 64))))]);
                         Move (v_steps, (BinOp (Type.PLUS, (Var v_steps), Int (Bitvector.lit 1 64))))
                        ]))
              ]) in
          assert_raises Not_found (fun () -> State.peek v_n state);
          assert_equal
            (State.peek v_steps state)
            (BV (Bitvector.lit 12 64))
        );
    ];
  ]
