open OUnit2
open Angstrom

let empty_list = []

let list_a = [ 1; 2; 3 ]

let number_tuple = (1, 2)

let rec len xs = match xs with [] -> 0 | _ :: t -> 1 + len t

type orientation = N | S | E | W

let string_of_orientation o =
  match o with N -> "N" | S -> "S" | E -> "E" | W -> "W"

type instruction = Move | Left | Right

type rover = { x : int; y : int; o : orientation }

type program = rover * instruction list

let string_of_rover r =
  String.concat " "
    [
      "Rover { x:";
      string_of_int r.x;
      "y:";
      string_of_int r.y;
      "o:";
      string_of_orientation r.o;
      "}";
    ]

let string_of_rovers is = String.concat " " (List.map string_of_rover is)

let string_of_instruction i =
  match i with Move -> "Move" | Left -> "Left" | Right -> "Right"

let string_of_instructions is =
  String.concat " " (List.map string_of_instruction is)

let string_of_program p =
  String.concat " " [ string_of_rover (fst p); string_of_instructions (snd p) ]

let string_of_programs ps = String.concat " " (List.map string_of_program ps)

let make_rover x y o = { x; y; o }

let rotate_left r =
  match r.o with
  | N -> { r with o = W }
  | W -> { r with o = S }
  | S -> { r with o = E }
  | E -> { r with o = N }

let rotate_right r =
  match r.o with
  | N -> { r with o = E }
  | E -> { r with o = S }
  | S -> { r with o = W }
  | W -> { r with o = N }

let move_rover r =
  match r.o with
  | N -> { r with y = r.y + 1 }
  | S -> { r with y = r.y - 1 }
  | E -> { r with x = r.x + 1 }
  | W -> { r with x = r.x - 1 }

let exec_command r i =
  match i with
  | Left -> rotate_left r
  | Right -> rotate_right r
  | Move -> move_rover r

let run_rover (r : rover) (insts : instruction list) =
  List.fold_left exec_command r insts

let uncurried f (a, b) = f a b

let run_programs (ps : program list) =
  List.map (fun x -> uncurried run_rover x) ps

(* -------------------------------------------------------------------------- *)
(* Parser *)
(* -------------------------------------------------------------------------- *)

let mk_tuple a b = (a, b)

let parse_int =
  take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

let parse_orientation =
  choice
    [
      char 'N' *> return N;
      char 'S' *> return S;
      char 'W' *> return W;
      char 'E' *> return E;
    ]

let parse_instruction =
  choice
    [
      string "L" *> return Left;
      string "R" *> return Right;
      string "M" *> return Move;
    ]

let parse_instructions = many parse_instruction

let parse_coordinate = mk_tuple <$> parse_int <*> char ' ' *> parse_int

let parse_rover =
  make_rover <$> parse_int
  <*> char ' ' *> parse_int
  <*> char ' ' *> parse_orientation

let parse_program =
  parse_coordinate *> char '\n'
  *> many1
       ( mk_tuple <$> parse_rover
       <*> char '\n' *> parse_instructions
       <* char '\n' )

(* -------------------------------------------------------------------------- *)
(* Tests *)
(* -------------------------------------------------------------------------- *)

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
