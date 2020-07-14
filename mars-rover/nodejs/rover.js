function Coord(x, y) {
    this.x = x;
    this.y = y;
}

Coord.prototype.equals = function(that) {
    return (that instanceof Coord)
        && this.x == that.x
        && this.y == that.y;
}

function Rover(pos, dir) {
    this.pos = pos;
    this.dir = dir;
}

/**
 * Parses "1 2 N"
 * Returns a new Rover instance
*/
const parseRover = (str) => {
    const [x, y, dir] = str.split(" ");
    return mkRover(parseInt(x), parseInt(y), dir);
}

/**
 * Parses "1 2 N\nLMRLLMRL"
 * Returns an object { rover, instructions }
*/
const parseRoverSpec = (str) => {
    const [r, i] = str.split("\n");
    return { rover: parseRover(r), instructions: i };
}

/**
 * partition([1,2,3,4], 2) == [[1,2], [3,4]]
 * Returns the given list partitioned into sublists of size n
*/
function partition(array, n) {
  return array.length ? [array.splice(0, n)].concat(partition(array, n)) : [];
}

/**
 * Parses a sequence of Programs starting with a "<Int> <Int>"
 * line which is discarded
 * Returns [{rover, instructions}]
*/
const parseProgram = (str) => {
    const lines = str.split("\n").slice(1);
    const roverSpecs = partition(lines, 2);

    return roverSpecs.map(([r, i]) => parseRoverSpec(`${r}\n${i}`));
}


/**
 * Executes a program parsed by `parseProgram`
 * Returns [Rover]
*/
const runProgram = (p) => parseProgram(p).map(({rover, instructions}) => rover.exec(instructions));

const mkRover = (x,y,dir) => new Rover(new Coord(x,y), dir);

const leftRotation = {
    "N": "W",
    "W": "S",
    "S": "E",
    "E": "N"
};

const rightRotation = {
    "N": "E",
    "E": "S",
    "S": "W",
    "W": "N"
};

Rover.prototype.equals = function(that) {
    return (that instanceof Rover)
        && this.pos.equals(that.pos)
        && this.dir == that.dir
}

/**
 * Rotates a rover to the left
 * Returns a new, rotated rover instance
*/
Rover.prototype.rotateLeft = function() {
    const d = leftRotation[this.dir];
    return new Rover(this.pos, d);
}

/**
 * Rotates a rover to the right
 * Returns a new, rotated rover instance
*/
Rover.prototype.rotateRight = function() {
    const d = rightRotation[this.dir];
    return new Rover(this.pos, d);
}

/**
 * Moves a rover forward 1 unit towards the direction it is pointed at
 * Returns a new, moved instance
 */
Rover.prototype.advance = function() {
    switch (this.dir) {
        case "N":
            return new Rover(new Coord(this.pos.x, this.pos.y + 1), this.dir);
            break;
        case "S":
            return new Rover(new Coord(this.pos.x, this.pos.y - 1), this.dir);
            break;
        case "E":
            return new Rover(new Coord(this.pos.x + 1, this.pos.y), this.dir);
            break;
        case "W":
            return new Rover(new Coord(this.pos.x - 1, this.pos.y), this.dir);
            break;
    }
}

/**
 * Executes a single command: "L", "R" or "M"
 * Returns a new Rover instance with the command applied
 */
Rover.prototype.execInstruction = function (i) {
    switch (i) {
        case "L":
            return this.rotateLeft();
            break;
        case "R":
            return this.rotateRight();
            break;
        case "M":
            return this.advance();
            break;
    }
}

/**
 * Execute one or more commands: 
 * exec("M")
 * exec("MLR")
 * exec(["M", "L", "R"])
 * Returns a new Rover instances with commands applied
*/
Rover.prototype.exec = function (is) {
    if (typeof(is) == "string" && is.length == 1) {
        // "M" | "L" | "R"
        return this.execInstruction(is);
    } else if (typeof(is) == "string" && is.length > 1) {
        // "MLRLMMRLLRM"
        return is.split("").reduce((res, i) => res.exec(i), this);
    }

    // ["M", "L", "R",.. ]
    return is.reduce((res, i) => res.exec(i), this);
}

module.exports = {
    Coord,
    Rover,
    mkRover,
    parseRover,
    parseRoverSpec,
    parseProgram,
    runProgram
};
