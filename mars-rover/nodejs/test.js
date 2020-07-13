const test = require('ava');
const r = require('./rover.js');

const Coord = r.Coord;
const Rover = r.Rover;
const mkRover = r.mkRover;

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

test('Rover.exec', t => {
    const n = mkRover(0, 0, "N");
    t.true(n.exec("M").equals(mkRover(0, 1, "N")));
    t.true(n.exec("L").equals(mkRover(0, 0, "W")));
    t.true(n.exec("R").equals(mkRover(0, 0, "E")));
});

test('Rover.execList', t => {
    const n = mkRover(0, 0, "N");
    const res = n.execList(["M", "L", "M"]);
    const expected = mkRover(-1, 1, "W");

    t.true(res.equals(expected));
});
