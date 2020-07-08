open OUnit2

let empty_list = []

let list_a = [ 1; 2; 3 ]

let number_tuple = (1, 2)

let rec len xs = match xs with [] -> 0 | _ :: t -> 1 + len t

type orientation = N | S | E | W

let string_of_orientation o =
  match o with N -> "N" | S -> "S" | E -> "E" | W -> "W"

type instruction = Move | Left | Right

type rover = { x : int; y : int; o : orientation }

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

(* -------------------------------------------------------------------------- *)
(* Tests *)
(* -------------------------------------------------------------------------- *)

let assert_rover_eq r1 r2 = assert_equal ~printer:string_of_rover r1 r2

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

let suite =
  "ExampleTestList"
  >::: [
         "test_run_rover" >:: test_run_rover;
         "test_move" >:: test_move_rover;
         "test_rotate" >:: test_rotate;
         "test_exec" >:: test_exec;
       ]

let () = run_test_tt_main suite
