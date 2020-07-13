import fastparse._, NoWhitespace._

object Orientation extends Enumeration {
  val N = Value("North")
  val S = Value("South")
  val E = Value("East")
  val W = Value("West")
}

object RoverParser {

  def orientationP[_: P] =
    P("N" | "S" | "E" | "W").!.map {
      case "N" => Orientation.N
      case "S" => Orientation.S
      case "E" => Orientation.E
      case "W" => Orientation.W
    }

  def instructionP[_: P] =
    P("L" | "R" | "M").!.map {
      case "L" => Instruction.Left
      case "R" => Instruction.Right
      case "M" => Instruction.Move
    }

  def number[_: P]: P[Int] = P(CharIn("0-9").rep(1).!.map(_.toInt))

  def coordinateP[_: P] =
    P(
      number ~ " " ~ number
    ).map { z: ((Int, Int)) => Coord(z._1, z._2) }

  def roverP[_: P] =
    P(
      coordinateP ~ " " ~ orientationP
    ).map { z: ((Coord, Orientation.Value)) => new Rover(z._1, z._2) }

  def programP[_: P] =
    P(
      coordinateP ~ "\n" ~
        (roverP ~ "\n" ~
          instructionP.rep(1) ~ "\n".?).rep(1) ~ End
    ).map {
      case (_, res) =>
        Program(res.map(r => RoverProgram(r._1, r._2.toList)).toList)
    }

}

object Instruction extends Enumeration {
  val Left = Value("Left")
  val Right = Value("Right")
  val Move = Value("Move")
}

case class Coord(x: Int, y: Int)

case class RoverProgram(rover: Rover, insts: List[Instruction.Value])

case class Program(programs: List[RoverProgram])

case class Rover(val pos: Coord, val orientation: Orientation.Value) {

  def rotateLeft(): Rover = {
    val res = this.orientation match {
      case Orientation.N => Orientation.W
      case Orientation.W => Orientation.S
      case Orientation.S => Orientation.E
      case Orientation.E => Orientation.N
    }
    new Rover(this.pos, res)
  }

  def rotateRight(): Rover = {
    val res = this.orientation match {
      case Orientation.N => Orientation.E
      case Orientation.E => Orientation.S
      case Orientation.S => Orientation.W
      case Orientation.W => Orientation.N
    }
    new Rover(this.pos, res)
  }

  def advance(): Rover = {
    val p = this.pos

    val c = this.orientation match {
      case Orientation.N => Coord(p.x, p.y + 1)
      case Orientation.E => Coord(p.x + 1, p.y)
      case Orientation.S => Coord(p.x, p.y - 1)
      case Orientation.W => Coord(p.x - 1, p.y)
    }

    new Rover(c, this.orientation)
  }

  def move(inst: Instruction.Value): Rover = {
    inst match {
      case Instruction.Left  => this.rotateLeft()
      case Instruction.Right => this.rotateRight()
      case Instruction.Move  => this.advance()
    }
  }

  def move(inst: List[Instruction.Value]): Rover =
    inst.foldLeft(this) { _.move(_) }

}
