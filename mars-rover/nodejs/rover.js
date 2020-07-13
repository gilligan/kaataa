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

Rover.prototype.toString = function () {
    return `Rover (${this.pos.x}, ${this.pos.y}) ${this.dir}`;
}

Rover.prototype.equals = function(that) {
    return (that instanceof Rover)
        && this.pos.equals(that.pos)
        && this.dir == that.dir
}

Rover.prototype.rotateLeft = function() {
    const d = leftRotation[this.dir];
    return new Rover(this.pos, d);
}

Rover.prototype.rotateRight = function() {
    const d = rightRotation[this.dir];
    return new Rover(this.pos, d);
}

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

Rover.prototype.exec = function (i) {
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

Rover.prototype.execList = function (is) {
    return is.reduce((res, i) => res.exec(i), this);
}

module.exports = {
    Coord,
    Rover,
    mkRover,

};
