import fastparse._

class RoverTest extends org.scalatest.FunSuite {

  test("Coordinate") {
    val p1 = Coord(1, 1)
    val p2 = Coord(1, 1)
    assert(p1 == p2)
    assert(p1.x == p2.x)
    assert(p1.y == p2.y)
  }

  test("Direction") {
    assert(Orientation.N === Orientation.N)
    assert(Orientation.N.toString === "North")
  }

  test("Rover") {
    val r = new Rover(Coord(1, 1), Orientation.N)
    assert(r.pos.x === 1)
    assert(r.pos.y === 1)
    assert(r.orientation === Orientation.N)
  }

  test("Rover.rotateLeft") {
    val r1 = new Rover(Coord(1, 1), Orientation.N)
    assert((r1.rotateLeft).orientation === Orientation.W)

    val r2 = new Rover(Coord(1, 1), Orientation.W)
    assert((r2.rotateLeft).orientation === Orientation.S)

    val r3 = new Rover(Coord(1, 1), Orientation.S)
    assert((r3.rotateLeft).orientation === Orientation.E)

    val r4 = new Rover(Coord(1, 1), Orientation.E)
    assert((r4.rotateLeft).orientation === Orientation.N)
  }

  test("Rover.rotateRight") {
    val r1 = new Rover(Coord(1, 1), Orientation.N)
    assert((r1.rotateRight).orientation === Orientation.E)

    val r2 = new Rover(Coord(1, 1), Orientation.E)
    assert((r2.rotateRight).orientation === Orientation.S)

    val r3 = new Rover(Coord(1, 1), Orientation.S)
    assert((r3.rotateRight).orientation === Orientation.W)

    val r4 = new Rover(Coord(1, 1), Orientation.W)
    assert((r4.rotateRight).orientation === Orientation.N)
  }

  test("Rover.advance") {
    assert(
      (new Rover(Coord(1, 1), Orientation.N)).advance().pos === Coord(1, 2)
    )
    assert(
      (new Rover(Coord(1, 1), Orientation.S)).advance().pos === Coord(1, 0)
    )
    assert(
      (new Rover(Coord(1, 1), Orientation.E)).advance().pos === Coord(2, 1)
    )
    assert(
      (new Rover(Coord(1, 1), Orientation.W)).advance().pos === Coord(0, 1)
    )
  }

  test("Rover.move") {
    assert(
      (new Rover(Coord(1, 1), Orientation.N))
        .move(Instruction.Move) == (new Rover(Coord(1, 2), Orientation.N))
    )
    assert(
      (new Rover(Coord(1, 1), Orientation.N))
        .move(Instruction.Left) == (new Rover(Coord(1, 1), Orientation.W))
    )
    assert(
      (new Rover(Coord(1, 1), Orientation.N))
        .move(Instruction.Right) == (new Rover(Coord(1, 1), Orientation.E))
    )
  }

  test("Rover move (list)") {
    assert(
      (new Rover(Coord(1, 1), Orientation.N)).move(
        List(Instruction.Move, Instruction.Move)
      ) == new Rover(Coord(1, 3), Orientation.N)
    )
    assert(
      (new Rover(Coord(1, 1), Orientation.N)).move(
        List(Instruction.Move, Instruction.Left)
      ) == new Rover(Coord(1, 2), Orientation.W)
    )
    assert(
      (new Rover(Coord(1, 1), Orientation.N)).move(
        List(Instruction.Move, Instruction.Right)
      ) == new Rover(Coord(1, 2), Orientation.E)
    )
  }

  test("RoverParser.orientationP") {
    val Parsed.Success(n, _) = fastparse.parse("N", RoverParser.orientationP(_))
    assert(n == Orientation.N)

    val Parsed.Success(s, _) = fastparse.parse("S", RoverParser.orientationP(_))
    assert(s == Orientation.S)

    val Parsed.Success(e, _) = fastparse.parse("E", RoverParser.orientationP(_))
    assert(e == Orientation.E)

    val Parsed.Success(w, _) = fastparse.parse("W", RoverParser.orientationP(_))
    assert(w == Orientation.W)
  }

  test("RoverPars.instructionP") {
    val Parsed.Success(l, _) = fastparse.parse("L", RoverParser.instructionP(_))
    assert(l == Instruction.Left)

    val Parsed.Success(r, _) = fastparse.parse("R", RoverParser.instructionP(_))
    assert(r == Instruction.Right)

    val Parsed.Success(m, _) = fastparse.parse("M", RoverParser.instructionP(_))
    assert(m == Instruction.Move)
  }

  test("RoverPars.coordinateP") {
    val Parsed.Success(r, _) =
      fastparse.parse("1 1", RoverParser.coordinateP(_))
    assert(r == (Coord(1, 1)))
  }

  test("RoverPars.roverP") {
    val Parsed.Success(r, _) =
      fastparse.parse("1 1 N", RoverParser.roverP(_))
    assert(r == (new Rover(Coord(1, 1), Orientation.N)))
  }
  test("RoverPars.programP") {
    val Parsed.Success(r, _) =
      fastparse.parse("5 5\n1 1 N\nLLLMM", RoverParser.programP(_))

    val rprog = (RoverProgram(
      new Rover(Coord(1, 1), Orientation.N),
      List(
        Instruction.Left,
        Instruction.Left,
        Instruction.Left,
        Instruction.Move,
        Instruction.Move
      )
    ))
    assert(
      r == Program(List(rprog))
    )
  }

  //test("RoverPars.programP multiple") {
  //val Parsed.Success(r, _) =
  //fastparse.parse(
  //"5 5\n1 1 N\nLLLMM\n1 1 N\nLLLMM",
  //RoverParser.programP(_)
  //)

  //val res1 = (
  //new Rover(Coord(1, 1), Orientation.N),
  //List(
  //Instruction.Left,
  //Instruction.Left,
  //Instruction.Left,
  //Instruction.Move,
  //Instruction.Move
  //)
  //)

  //assert(
  //r == Program(
  //List(res1, res1)
  //)
  //)
  //}
}
