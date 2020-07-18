package mars.rover

data class Pos(val x: Int, val y: Int)

enum class Instruction { Move, Left, Right }

enum class Dir { N, S, E, W }

data class Rover(val pos: Pos, val dir: Dir)

fun Rover.rotateLeft(): Rover = when (this.dir) {
    Dir.N -> this.copy(dir = Dir.W)
    Dir.S -> this.copy(dir = Dir.E)
    Dir.E -> this.copy(dir = Dir.N)
    Dir.W -> this.copy(dir = Dir.S)
}

fun Rover.rotateRight(): Rover = when (this.dir) {
    Dir.N -> this.copy(dir = Dir.E)
    Dir.S -> this.copy(dir = Dir.W)
    Dir.E -> this.copy(dir = Dir.S)
    Dir.W -> this.copy(dir = Dir.N)
}

fun Rover.advance(): Rover = when (this.dir) {
    Dir.N -> this.copy(pos = this.pos.copy(y = this.pos.y + 1))
    Dir.S -> this.copy(pos = this.pos.copy(y = this.pos.y - 1))
    Dir.E -> this.copy(pos = this.pos.copy(x = this.pos.x + 1))
    Dir.W -> this.copy(pos = this.pos.copy(x = this.pos.x - 1))
}

fun Rover.exec(i: Instruction): Rover = when (i) {
    Instruction.Left -> this.rotateLeft()
    Instruction.Right -> this.rotateRight()
    Instruction.Move -> this.advance()
}

fun Rover.exec(insts: List<Instruction>): Rover = insts.fold(this, {
    r: Rover, i: Instruction -> r.exec(i)
})
