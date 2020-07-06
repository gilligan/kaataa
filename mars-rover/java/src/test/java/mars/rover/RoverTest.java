/*
 * This Java source file was generated by the Gradle 'init' task.
 */
package mars.rover;

import org.junit.Test;
import static org.junit.Assert.*;

public class RoverTest {

    @Test
    public void testRotateRoverLeft() {
        {
            Rover r = new Rover(1, 2, Rover.Orientation.N);
            assertEquals(r.rotateLeft(), new Rover(1, 2, Rover.Orientation.W));
        }

        {
            Rover r = new Rover(1, 2, Rover.Orientation.W);
            assertEquals(r.rotateLeft(), new Rover(1, 2, Rover.Orientation.S));
        }
        {
            Rover r = new Rover(1, 2, Rover.Orientation.S);
            assertEquals(r.rotateLeft(), new Rover(1, 2, Rover.Orientation.E));
        }
        {
            Rover r = new Rover(1, 2, Rover.Orientation.E);
            assertEquals(r.rotateLeft(), new Rover(1, 2, Rover.Orientation.N));
        }
    }
    @Test
    public void testRotateRoverRight() {
        {
            Rover r = new Rover(1, 2, Rover.Orientation.N);
            assertEquals(r.rotateRight(), new Rover(1, 2, Rover.Orientation.E));
        }

        {
            Rover r = new Rover(1, 2, Rover.Orientation.W);
            assertEquals(r.rotateRight(), new Rover(1, 2, Rover.Orientation.N));
        }
        {
            Rover r = new Rover(1, 2, Rover.Orientation.S);
            assertEquals(r.rotateRight(), new Rover(1, 2, Rover.Orientation.W));
        }
        {
            Rover r = new Rover(1, 2, Rover.Orientation.E);
            assertEquals(r.rotateRight(), new Rover(1, 2, Rover.Orientation.S));
        }
    }

    @Test
    public void testExecInstruction() {
        {
            Rover r = new Rover(1, 2, Rover.Orientation.N);
            assertEquals(r.exec(Rover.Instruction.Move), new Rover(1, 3, Rover.Orientation.N));
        }

        {
            Rover r = new Rover(1, 2, Rover.Orientation.S);
            assertEquals(r.exec(Rover.Instruction.Move), new Rover(1, 1, Rover.Orientation.S));
        }

    }

    @Test
    public void testEquals() {
        Rover r = new Rover(1, 2, Rover.Orientation.N);
        Rover r2 = new Rover(1, 2, Rover.Orientation.N);

        assertEquals(r, r2);
    }
}
