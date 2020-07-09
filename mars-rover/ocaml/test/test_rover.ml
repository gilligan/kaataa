open Angstrom
open OUnit2
open Rover

let assert_rover_eq r1 r2 = assert_equal ~printer:string_of_rover r1 r2

let assert_instructions_eq i1 i2 =
  assert_equal ~printer:string_of_instructions i1 i2

let test_parse_orientation _ =
  let n = parse_string parse_orientation "N" in
  let s = parse_string parse_orientation "S" in
  let e = parse_string parse_orientation "E" in
  let w = parse_string parse_orientation "W" in
  assert_equal n (Ok N);
  assert_equal s (Ok S);
  assert_equal e (Ok E);
  assert_equal w (Ok W)

let test_parse_instruction _ =
  let l = parse_string parse_instruction "L" in
  let r = parse_string parse_instruction "R" in
  let m = parse_string parse_instruction "M" in
  assert_equal l (Ok Left);
  assert_equal r (Ok Right);
  assert_equal m (Ok Move)

let test_parse_instructions _ =
  let is = "LRMMRL" in
  let res = parse_string parse_instructions is in
  assert_equal res (Ok [ Left; Right; Move; Move; Right; Left ])

let test_parse_coordinate _ =
  let a = parse_string parse_coordinate "1 1" in
  let b = parse_string parse_coordinate "0 2" in
  assert_equal a (Ok (1, 1));
  assert_equal b (Ok (0, 2))

let test_parse_program _ =
  let p = "5 5\n1 2 N\nLRMRLM\n1 1 N\nLRM\n" in
  let res = Result.get_ok (parse_string parse_program p) in
  let expected =
    [
      (make_rover 1 2 N, [ Left; Right; Move; Right; Left; Move ]);
      (make_rover 1 1 N, [ Left; Right; Move ]);
    ]
  in
  assert_equal ~printer:string_of_programs expected res

let test_parse_rover _ =
  let r1 = parse_string parse_rover "1 2 N" in
  let r1_expected = Ok (make_rover 1 2 N) in
  let r2 = parse_string parse_rover "0 8 E" in
  let r2_expected = Ok (make_rover 0 8 E) in
  assert_equal r1 r1_expected;
  assert_equal r2 r2_expected

let test_run_rover _ =
  let r = { x = 1; y = 1; o = N } in
  assert_rover_eq (run_rover r [ Move; Move; Move ]) { r with y = r.y + 3 };
  assert_rover_eq (run_rover r [ Left; Move ]) { r with o = W; x = r.x - 1 };
  assert_rover_eq (run_rover r [ Left; Left; Left; Left ]) r

let test_move_rover _ =
  let r = { x = 1; y = 1; o = N } in
  assert_rover_eq (move_rover r) { r with y = r.y + 1 }

let test_rotate _ =
  let r = { x = 1; y = 1; o = N } in
  assert_rover_eq (rotate_left r) { x = 1; y = 1; o = W };
  assert_rover_eq (rotate_right r) { x = 1; y = 1; o = E }

let test_exec _ =
  let r = { x = 1; y = 1; o = N } in
  assert_rover_eq (exec_command r Move) { r with y = r.y + 1 };
  assert_rover_eq (exec_command r Left) { r with o = W };
  assert_rover_eq (exec_command r Right) { r with o = E }

let test_run_programs _ =
  let progs =
    [ (make_rover 1 0 N, [ Left; Move ]); (make_rover 1 2 N, [ Right; Move ]) ]
  in
  let res = run_programs progs in
  let expected = [ make_rover 0 0 W; make_rover 2 2 E ] in
  assert_equal expected ~printer:string_of_rovers res

let suite =
  "ExampleTestList"
  >::: [
         "test_run_rover" >:: test_run_rover;
         "test_move" >:: test_move_rover;
         "test_rotate" >:: test_rotate;
         "test_exec" >:: test_exec;
         "test_run_programs" >:: test_run_programs;
         "test_parse_orientation" >:: test_parse_orientation;
         "test_parse_instruction" >:: test_parse_instruction;
         "test_parse_coordinate" >:: test_parse_coordinate;
         "test_parse_rover" >:: test_parse_rover;
         "test_parse_program" >:: test_parse_program;
       ]

let () = run_test_tt_main suite
