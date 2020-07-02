package main

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestRover(t *testing.T) {
	var r Rover
	r = Rover{0, 0, N}

	assert.Equal(t, r.x, 0, "x")
	assert.Equal(t, r.y, 0, "x")
	assert.Equal(t, r.orientation, N, "x")
}

func TestRotateRover(t *testing.T) {
	r := Rover{0, 0, N}
	assert.Equal(t, execRoverInstruction(r, Left).orientation, W, "rotated left")
	assert.Equal(t, execRoverInstruction(r, Right).orientation, E, "rotated right")

	r = Rover{0, 0, E}
	assert.Equal(t, execRoverInstruction(r, Left).orientation, N, "rotated left")
	assert.Equal(t, execRoverInstruction(r, Right).orientation, S, "rotated right")

	r = Rover{0, 0, S}
	assert.Equal(t, execRoverInstruction(r, Left).orientation, W, "rotated left")
	assert.Equal(t, execRoverInstruction(r, Right).orientation, E, "rotated right")

	r = Rover{0, 0, W}
	assert.Equal(t, execRoverInstruction(r, Left).orientation, S, "rotated left")
	assert.Equal(t, execRoverInstruction(r, Right).orientation, N, "rotated right")
}

func TestMoveRover(t *testing.T) {
	r := Rover{0, 0, N}
	r2 := execRoverInstruction(r, Move)
	assert.Equal(t, r2.orientation, N, "moved")
	assert.Equal(t, r2.x, r.x, "moved")
	assert.Equal(t, r2.y, r.y+1, "moved")

	r = Rover{0, 0, E}
	r2 = execRoverInstruction(r, Move)
	assert.Equal(t, r2.orientation, E, "moved")
	assert.Equal(t, r2.x, r.x+1, "moved")
	assert.Equal(t, r2.y, r.y, "moved")

	r = Rover{0, 0, S}
	r2 = execRoverInstruction(r, Move)
	assert.Equal(t, r2.orientation, S, "moved")
	assert.Equal(t, r2.x, r.x, "moved")
	assert.Equal(t, r2.y, r.y-1, "moved")

	r = Rover{0, 0, W}
	r2 = execRoverInstruction(r, Move)
	assert.Equal(t, r2.orientation, W, "moved")
	assert.Equal(t, r2.x, r.x-1, "moved")
	assert.Equal(t, r2.y, r.y, "moved")
}

func TestExecuteInstruction(t *testing.T) {
	instructions := []Instruction{L, L, L, L}
	r := Rover{0, 0, N}
	r2 := execRoverInstructions(r, instructions)
	assert.Equal(t, r.orientation, N, "went around the clock once")
}
