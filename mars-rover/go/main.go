package main

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

func execRoverInstructions(r Rover, insts []Instruction) Rover {
    for _, inst := range insts {
        r = execRoverInstruction(r, inst)
    }
    return r
}
