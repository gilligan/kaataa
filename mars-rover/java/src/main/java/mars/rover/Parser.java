package mars.rover;

import mars.rover.Library;
import java.util.Arrays;
import java.util.ArrayList;

public class Parser {

    public static Library.Instruction[] parseInstructions(String s) {
        ArrayList<Library.Instruction> res = new ArrayList<Library.Instruction>();
        for (char c : s.toCharArray()) {
            res.add(parseInstruction(Character.toString(c)));
        }
        Library.Instruction[] ourResultActually = new Library.Instruction[res.size()];
        res.toArray(ourResultActually);
        return ourResultActually;
    }

    public static Library.Instruction parseInstruction(String s) {
        if (s.equals("M")) {
            return Library.Instruction.Move;
        } else if (s.equals("L")) {
            return Library.Instruction.Left;
        } else if (s.equals("R")) {
            return Library.Instruction.Right;
        }

        throw new RuntimeException("Tried to parse invalid instruction " + s);
    }

    public static Library.Orientation parseOrientation(String s) {
        if (s.equals("S")) {
            return Library.Orientation.S;
        } else if (s.equals("N")) {
            return Library.Orientation.N;
        } else if (s.equals("E")) {
            return Library.Orientation.E;
        } else if (s.equals("W")) {
            return Library.Orientation.W;
        }

        throw new RuntimeException("Tried to parse invalid orientation " + s);
    }

    public static Library.Rover parseRover(String s) {
        String[] parts = s.split(" ");

        Integer x = Integer.parseInt(parts[0]);
        Integer y = Integer.parseInt(parts[1]);
        Library.Orientation o = parseOrientation(parts[2]);

        return new Library.Rover(x, y, o);
    }

    public static Library.Rover[] runProgram(String s) {

        ArrayList<Library.Rover> results = new ArrayList<Library.Rover>();
        String[] lines = s.split("\n");


        while (lines.length > 1) {
            Library.Rover r = parseRover(lines[1]);
            Library.Instruction[] instructions = parseInstructions(lines[2]);

            lines = Arrays.copyOfRange(lines, 2, lines.length);
            r = r.exec(instructions);
            results.add(r);
        }

        Library.Rover[] actualResults = new Library.Rover[results.size()];
        results.toArray(actualResults);
        return actualResults;
    }
}
