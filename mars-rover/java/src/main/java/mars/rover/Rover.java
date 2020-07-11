package mars.rover;

import java.util.Arrays;
import java.util.List;
import lombok.*;

@EqualsAndHashCode
@ToString
public class Rover {

    public static enum Orientation {
        N, S, W, E
    };

    public static enum Instruction {
        Left, Right, Move
    };


    public boolean someLibraryMethod() {
        return false;
    }

    private Integer x;
    private Integer y;
    public Orientation orientation;

    public Rover(Integer x, Integer y, Orientation o) {
        this.x = x;
        this.y = y;
        this.orientation = o;
    }

    public Rover move() {
        switch (orientation) {
        case N:
            return new Rover(x, y + 1, orientation);
        case S:
            return new Rover(x, y - 1, orientation);
        case E:
            return new Rover(x + 1, y, orientation);
        default:
        case W:
            return new Rover(x - 1, y, orientation);
        }
    }

    public Rover exec(List<Instruction> is) {
        Rover r = this;

        for (Instruction i : is)
            r = r.exec(i);

        return r;
    }

    public Rover exec(Instruction i) {
        switch (i) {
        case Move:
            return this.move();
        case Left:
            return this.rotateLeft();
        default:
        case Right:
            return this.rotateRight();
        }
    }

    public Rover rotateRight() {
        return rotateLeft().rotateLeft().rotateLeft();
    }

    public Rover rotateLeft() {
        switch (this.orientation) {
        case N:
            return new Rover(x, y, Orientation.W);
        case W:
            return new Rover(x, y, Orientation.S);
        case S:
            return new Rover(x, y, Orientation.E);
        case E:
            return new Rover(x, y, Orientation.N);
        default:
            return this;
        }
    }
}
