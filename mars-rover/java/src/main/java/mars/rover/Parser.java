package mars.rover;

import mars.rover.Rover;
import java.util.Arrays;
import java.util.ArrayList;

public class Parser {

    public static Rover.Instruction[] parseInstructions(String s) {
        ArrayList<Rover.Instruction> res = new ArrayList<Rover.Instruction>();
        for (char c : s.toCharArray()) {
            res.add(parseInstruction(Character.toString(c)));
        }
        Rover.Instruction[] ourResultActually = new Rover.Instruction[res.size()];
        res.toArray(ourResultActually);
        return ourResultActually;
    }

    public static Rover.Instruction parseInstruction(String s) {
        if (s.equals("M")) {
            return Rover.Instruction.Move;
        } else if (s.equals("L")) {
            return Rover.Instruction.Left;
        } else if (s.equals("R")) {
            return Rover.Instruction.Right;
        }

        throw new RuntimeException("Tried to parse invalid instruction " + s);
    }

    public static Rover.Orientation parseOrientation(String s) {
        if (s.equals("S")) {
            return Rover.Orientation.S;
        } else if (s.equals("N")) {
            return Rover.Orientation.N;
        } else if (s.equals("E")) {
            return Rover.Orientation.E;
        } else if (s.equals("W")) {
            return Rover.Orientation.W;
        }

        throw new RuntimeException("Tried to parse invalid orientation " + s);
    }

    public static Rover parseRover(String s) {
        String[] parts = s.split(" ");

        Integer x = Integer.parseInt(parts[0]);
        Integer y = Integer.parseInt(parts[1]);
        Rover.Orientation o = parseOrientation(parts[2]);

        return new Rover(x, y, o);
    }

    public static Rover[] runProgram(String s) {

        ArrayList<Rover> results = new ArrayList<Rover>();
        String[] lines = s.split("\n");


        while (lines.length > 1) {
            Rover r = parseRover(lines[1]);
            Rover.Instruction[] instructions = parseInstructions(lines[2]);

            lines = Arrays.copyOfRange(lines, 2, lines.length);
            r = r.exec(instructions);
            results.add(r);
        }

        Rover[] actualResults = new Rover[results.size()];
        results.toArray(actualResults);
        return actualResults;
    }
}
