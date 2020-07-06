use nom::IResult;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::bytes::complete::take_while1;
use nom::combinator::map;
use nom::combinator::map_res;

#[derive(Debug, PartialEq)]
enum Orientation {
    N,
    S,
    W,
    E,
}

#[derive(Debug, PartialEq)]
struct Rover {
    x: i32,
    y: i32,
    orientation: Orientation,
}

#[derive(Debug, PartialEq)]
enum Instruction {
    Move,
    Left,
    Right,
}

fn parse_instruction(input: &str) -> IResult<&str, Instruction> {
    alt((
        map(tag("L"), |_| Instruction::Left),
        map(tag("R"), |_| Instruction::Right),
        map(tag("M"), |_| Instruction::Move),
    ))(input)
}

fn parse_int(input: &str) -> IResult<&str, i32> {
    map_res(take_while1(|c: char| c >= '0' && c <= '9'), |x: &str| {
        x.parse::<i32>()
    })(input)
}

fn parse_orientation(input: &str) -> IResult<&str, Orientation> {
    alt((
        map(tag("N"), |_| Orientation::N),
        map(tag("S"), |_| Orientation::S),
        map(tag("E"), |_| Orientation::E),
        map(tag("W"), |_| Orientation::W),
    ))(input)
}

impl Rover {
    fn parse(input: &str) -> IResult<&str, Rover> {
        let (input, x) = parse_int(input)?;
        // whitespace
        let (input, _) = tag(" ")(input)?;
        let (input, y) = parse_int(input)?;
        let (input, _) = tag(" ")(input)?;
        let (input, o) = parse_orientation(input)?;

        Ok((
            input,
            Rover {
                x,
                y,
                orientation: o,
            },
        ))
    }

    fn move_rover(self) -> Rover {
        match &self.orientation {
            N => Rover {
                y: self.y + 1,
                ..self
            },
            S => Rover {
                y: self.y - 1,
                ..self
            },
            E => Rover {
                x: self.x + 1,
                ..self
            },
            W => Rover {
                x: self.x - 1,
                ..self
            },
        }
    }

    fn exec(self, inst: Instruction) -> Rover {
        match inst {
            Move => self.move_rover(),
            Left => self.rotate_left(),
            Right => self.rotate_right(),
        }
    }

    fn rotate_left(self) -> Rover {
        use Orientation::*;
        let o = match self.orientation {
            N => W,
            W => S,
            S => E,
            E => N,
        };
        Rover {
            orientation: o,
            ..self
        }
    }

    fn rotate_right(self) -> Rover {
        use Orientation::*;
        let o = match self.orientation {
            N => E,
            W => N,
            S => W,
            E => S,
        };
        Rover {
            orientation: o,
            ..self
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Orientation::*;

    #[test]
    fn can_construct_a_rover() {
        Rover {
            x: 1,
            y: 2,
            orientation: N,
        };
    }

    #[test]
    fn can_rotate_rover_left() {
        let r = Rover {
            x: 1,
            y: 1,
            orientation: N,
        };
        let rotated = r.rotate_left();
        assert_eq!(
            rotated,
            Rover {
                x: 1,
                y: 1,
                orientation: W
            }
        );
    }

    #[test]
    fn can_rotate_rover_right() {
        let r = Rover {
            x: 1,
            y: 1,
            orientation: N,
        };
        let rotated = r.rotate_right();
        assert_eq!(
            rotated,
            Rover {
                x: 1,
                y: 1,
                orientation: E
            }
        );
    }

    #[test]
    fn can_execute_command() {
        use Instruction::*;
        let r = Rover {
            x: 1,
            y: 1,
            orientation: N,
        };
        let updated = r.exec(Move);
        assert_eq!(
            updated,
            Rover {
                x: 1,
                y: 2,
                orientation: N
            }
        );
    }

    #[test]
    fn can_parse_Rover() {
        let expected = Rover {
            x: 1,
            y: 1,
            orientation: N,
        };
        let (_, rover) = Rover::parse("1 1 N").unwrap();
        assert_eq!(rover, expected);
    }

    #[test]
    fn can_parse_Instruction() {
        let (_, inst) = parse_instruction("L").unwrap();
        assert_eq!(inst, Instruction::Left);

        let (_, inst) = parse_instruction("R").unwrap();
        assert_eq!(inst, Instruction::Right);

        let (_, inst) = parse_instruction("M").unwrap();
        assert_eq!(inst, Instruction::Move);
    }
}
