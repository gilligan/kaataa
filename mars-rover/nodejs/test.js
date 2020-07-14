const test = require('ava');
const r = require('./rover.js');

const Coord = r.Coord;
const Rover = r.Rover;
const mkRover = r.mkRover;
const parseRover = r.parseRover;
const parseRoverSpec = r.parseRoverSpec;
const parseProgram = r.parseProgram;
const runProgram = r.runProgram;

test('Create a Coord', t => {
    const c = new Coord(1,1);
    t.is(c.x, 1);
    t.is(c.y, 1);
});

test('Coord equality', t => {
    const c1 = new Coord(1,1);
    const c2 = new Coord(1,1);
    const c3 = new Coord(2,2);

    t.is(c1.equals(c2), true);
    t.is(c1.equals(c3), false);
});

test('Create a Rover', t => {
    const c = mkRover(1, 1, "N");
    t.is(c.pos.x, 1);
    t.is(c.pos.y, 1);
    t.is(c.dir, "N");
});

test('Rover equality', t => {
    const c1 = mkRover(0, 0, "S");
    const c2 = mkRover(0, 0, "S");
    const c3 = mkRover(0, 0, "W");

    t.true(c1.equals(c2));
    t.true(c2.equals(c1));
    t.false(c1.equals(c3));
});

test('Rover.rotateLeft', t => {
    const n = mkRover(0, 0, "N");
    const s = mkRover(0, 0, "S");
    const e = mkRover(0, 0, "E");
    const w = mkRover(0, 0, "W");

    t.true(n.rotateLeft().equals(w));
    t.true(w.rotateLeft().equals(s));
    t.true(s.rotateLeft().equals(e));
    t.true(e.rotateLeft().equals(n));
});

test('Rover.rotateRight', t => {
    const n = mkRover(0, 0, "N");
    const s = mkRover(0, 0, "S");
    const e = mkRover(0, 0, "E");
    const w = mkRover(0, 0, "W");

    t.true(n.rotateRight().equals(e));
    t.true(e.rotateRight().equals(s));
    t.true(s.rotateRight().equals(w));
    t.true(w.rotateRight().equals(n));
});

test('Rover.advance', t => {
    const n = mkRover(0, 0, "N");
    const s = mkRover(0, 0, "S");
    const e = mkRover(0, 0, "E");
    const w = mkRover(0, 0, "W");

    t.true(n.advance().equals(mkRover(0, 1, "N")));
    t.true(s.advance().equals(mkRover(0, -1, "S")));
    t.true(e.advance().equals(mkRover(1, 0, "E")));
    t.true(w.advance().equals(mkRover(-1, 0, "W")));
});

test('Rover.exec (single)', t => {
    const n = mkRover(0, 0, "N");
    t.true(n.exec("M").equals(mkRover(0, 1, "N")));
    t.true(n.exec("L").equals(mkRover(0, 0, "W")));
    t.true(n.exec("R").equals(mkRover(0, 0, "E")));
});

test('Rover.exec (string)', t => {
    const n = mkRover(0, 0, "N");
    const res = n.exec("MLM");
    const expected = mkRover(-1, 1, "W");

    t.true(res.equals(expected));
});

test('Rover.exec (list)', t => {
    const n = mkRover(0, 0, "N");
    const res = n.exec(["M", "L", "M"]);
    const expected = mkRover(-1, 1, "W");

    t.true(res.equals(expected));
});

test('parseRover', t => {
    const s = "1 2 N";
    const r = mkRover(1, 2, "N");

    t.true(parseRover(s).equals(r));
});

test('parseRoverSpec', t => {
    const spec = "1 2 N\nLMRLMR";
    const r = mkRover(1, 2, "N");
    const expected = { rover: r, instructions: "LMRLMR" };
    t.deepEqual(parseRoverSpec(spec), expected);
});

test('parseProgram', t => {
    const program = "5 5\n1 2 N\nLMRLMR\n0 0 S\nMMM";
    const expected = [{
        rover: mkRover(1, 2, "N"),
        instructions: "LMRLMR"
    }, {
        rover: mkRover(0, 0, "S"),
        instructions: "MMM"
    }];

    t.deepEqual(parseProgram(program), expected);
});

test('runProgram', t => {
    const p = "5 5\n1 2 N\nLMLMLMLMM";
    const expected = [ mkRover(1, 3, "N") ];

    t.deepEqual(runProgram(p), expected);
});
