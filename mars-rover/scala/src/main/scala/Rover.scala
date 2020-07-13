import fastparse._

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
}

object Instruction extends Enumeration {
  val Left = Value("Left")
  val Right = Value("Right")
  val Move = Value("Move")
}

case class Coord(x: Int, y: Int)

class Rover(val pos: Coord, val orientation: Orientation.Value) {

  override def equals(that: Any): Boolean =
    that match {
      case that: Rover =>
        that.isInstanceOf[
          Rover
        ] && this.pos == that.pos && this.orientation == that.orientation
      case _ => false
    }

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

    this.orientation match {
      case Orientation.N => new Rover(Coord(p.x, p.y + 1), this.orientation)
      case Orientation.E => new Rover(Coord(p.x + 1, p.y), this.orientation)
      case Orientation.S => new Rover(Coord(p.x, p.y - 1), this.orientation)
      case Orientation.W => new Rover(Coord(p.x - 1, p.y), this.orientation)
    }
  }

  def move(inst: Instruction.Value): Rover = {
    inst match {
      case Instruction.Left  => this.rotateLeft()
      case Instruction.Right => this.rotateRight()
      case Instruction.Move  => this.advance()
    }
  }

  def move(inst: List[Instruction.Value]): Rover =
    inst.foldLeft(this) { (res, inst) => res.move(inst) }

}
