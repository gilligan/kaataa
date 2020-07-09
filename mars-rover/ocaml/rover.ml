open Angstrom

type orientation = N | S | E | W

type rover = { x : int; y : int; o : orientation }

type instruction = Move | Left | Right

type program = rover * instruction list

let string_of_orientation o =
  match o with N -> "N" | S -> "S" | E -> "E" | W -> "W"


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
