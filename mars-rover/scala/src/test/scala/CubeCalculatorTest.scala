class RoverTest extends org.scalatest.FunSuite {

  test("Coordinate") {
    val p1 = Coord(1,1)
    val p2 = Coord(1,1)
    assert(p1 == p2)
    assert(p1.x == p2.x)
    assert(p1.y == p2.y)
  }

  test("Direction") {
    assert(Orientation.N === Orientation.N)
    assert(Orientation.N.toString === "North")
  }

  test("Rover") {
    val r = new Rover(Coord(1,1), Orientation.N)
    assert(r.pos.x === 1)
    assert(r.pos.y === 1)
    assert(r.orientation === Orientation.N)
  }

  test("Rover.rotateLeft") {
    val r1 = new Rover(Coord(1,1), Orientation.N)
    assert((r1.rotateLeft).orientation === Orientation.W)

    val r2 = new Rover(Coord(1,1), Orientation.W)
    assert((r2.rotateLeft).orientation === Orientation.S)

    val r3 = new Rover(Coord(1,1), Orientation.S)
    assert((r3.rotateLeft).orientation === Orientation.E)

    val r4 = new Rover(Coord(1,1), Orientation.E)
    assert((r4.rotateLeft).orientation === Orientation.N)
  }

  test("Rover.rotateRight") {
    val r1 = new Rover(Coord(1,1), Orientation.N)
    assert((r1.rotateRight).orientation === Orientation.E)

    val r2 = new Rover(Coord(1,1), Orientation.E)
    assert((r2.rotateRight).orientation === Orientation.S)

    val r3 = new Rover(Coord(1,1), Orientation.S)
    assert((r3.rotateRight).orientation === Orientation.W)

    val r4 = new Rover(Coord(1,1), Orientation.W)
    assert((r4.rotateRight).orientation === Orientation.N)
  }

  test("Rover.advance") {
    assert((new Rover(Coord(1,1), Orientation.N)).advance().pos === Coord(1, 2))
    assert((new Rover(Coord(1,1), Orientation.S)).advance().pos === Coord(1, 0))
    assert((new Rover(Coord(1,1), Orientation.E)).advance().pos === Coord(2, 1))
    assert((new Rover(Coord(1,1), Orientation.W)).advance().pos === Coord(0, 1))
  }

  test("Rover.move") {
    assert((new Rover(Coord(1,1), Orientation.N)).move(Instruction.Move) == (new Rover(Coord(1,2), Orientation.N)))
    assert((new Rover(Coord(1,1), Orientation.N)).move(Instruction.Left) == (new Rover(Coord(1,1), Orientation.W)))
    assert((new Rover(Coord(1,1), Orientation.N)).move(Instruction.Right) == (new Rover(Coord(1,1), Orientation.E)))
  }

  test("Rover move (list)") {
    assert((new Rover(Coord(1,1), Orientation.N)).move(List(Instruction.Move, Instruction.Move)) == new Rover(Coord(1,3), Orientation.N))
    assert((new Rover(Coord(1,1), Orientation.N)).move(List(Instruction.Move, Instruction.Left)) == new Rover(Coord(1,2), Orientation.W))
    assert((new Rover(Coord(1,1), Orientation.N)).move(List(Instruction.Move, Instruction.Right)) == new Rover(Coord(1,2), Orientation.E))
  }

}
