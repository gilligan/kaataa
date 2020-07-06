package main

import (
	"fmt"
    "strings"
)


func main() {
}

type Orientation int

const (
	N Orientation = iota
	S
	W
	E
)

type Rover struct {
	x           int
	y           int
	orientation Orientation
}

type Instruction int

const (
	Move Instruction = iota
	Left
	Right
)

func rotateLeft(r Rover) Rover {
	switch r.orientation {
	case N:
		r.orientation = W
	case S:
		r.orientation = E
	case E:
		r.orientation = N
	case W:
		r.orientation = S

	}
	return r
}

func rotateRight(r Rover) Rover {
	switch r.orientation {
	case N:
		r.orientation = E
	case S:
		r.orientation = W
	case E:
		r.orientation = S
	case W:
		r.orientation = N
	}
	return r
}

func moveRover(r Rover) Rover {
	switch r.orientation {
	case N:
		r.y += 1
	case E:
		r.x += 1
	case S:
		r.y -= 1
	case W:
		r.x -= 1
	}

	return r
}

func execRoverInstruction(r Rover, inst Instruction) (res Rover) {
	switch inst {
	case Move:
		res = moveRover(r)
	case Left:
		res = rotateLeft(r)
	case Right:
		res = rotateRight(r)
	}
	return res
}

func parseOrientation(s string) Orientation {
    switch s {
    case "S":
        return S;
    case "N":
        return N;
    case "E":
        return E;
    case "W":
        return W;
    }
    panic("tried to parse invalid orientation");
}

func parseRover(str string) Rover {
    var x, y int;
    var o string;

    _, err := fmt.Sscanf(str, "%d %d %s", &x, &y, &o);
    if err != nil {
        panic(err);
    }

    return Rover {x, y, parseOrientation(o)};
}

func parseInstruction(s int32) Instruction {
    switch s {
    case 'L':
        return Left;
    case 'R':
        return Right;
    case 'M':
        return Move;
    }
    panic("tried to parse invalid instruction");
}

func parseInstructions(insts string) []Instruction {
    var res []Instruction;
    for _, inst := range insts {
        res = append(res, parseInstruction(inst));
    }
    return res;
}

func execRoverInstructions(r Rover, insts []Instruction) Rover {
    for _, inst := range insts {
        r = execRoverInstruction(r, inst)
    }
    return r
}

func runProgram(prog string) (res []Rover) {
    lines := strings.Split(prog, "\n")[1:]

    for len(lines) > 1 {
        rover := parseRover(lines[0])
        instructions := parseInstructions(lines[1])
        rover = execRoverInstructions(rover, instructions)
        res = append(res, rover)
        lines = lines[2:]
    }

    return
}
