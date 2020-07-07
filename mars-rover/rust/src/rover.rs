use nom::IResult;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::bytes::complete::take_while1;
use nom::combinator::map;
use nom::combinator::map_res;
use nom::multi::many0;
use nom::sequence::preceded;
use nom::sequence::terminated;
use nom::sequence::tuple;

#[derive(Debug, PartialEq, Clone, Copy)]
enum Orientation {
    N,
    S,
    W,
    E,
}

#[derive(Debug, PartialEq, Clone)]
struct Rover {
    x: i32,
    y: i32,
    orientation: Orientation,
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Instruction {
    Move,
    Left,
    Right,
}

fn run_program(input: &str) -> Result<Vec<Rover>, &str> {
    match parse_program(input) {
        Ok((_, programs)) => {
            let res: Vec<_> = programs
                .into_iter()
                .map(|(rover, insts)| rover.exec_instructions(&insts))
                .collect();

            Ok(res)
        }
        Err(_) => Err("failed to parse program"),
    }
}

type Program = (Rover, Vec<Instruction>);

fn parse_program(input: &str) -> IResult<&str, Vec<Program>> {
    let (input, _) = parse_int(input)?;
    let (input, _) = tag(" ")(input)?;
    let (input, _) = parse_int(input)?;
    let (input, _) = tag("\n")(input)?;

    many1!(
        input,
        tuple((
            Rover::parse,
            preceded(tag("\n"), terminated(parse_instructions, many0(tag("\n"))))
        ))
    )
}

fn parse_instruction(input: &str) -> IResult<&str, Instruction> {
    alt((
        map(tag("L"), |_| Instruction::Left),
        map(tag("R"), |_| Instruction::Right),
        map(tag("M"), |_| Instruction::Move),
    ))(input)
}

fn parse_instructions(input: &str) -> IResult<&str, Vec<Instruction>> {
    many1!(input, parse_instruction)
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

    fn exec_instructions(&self, insts: &[Instruction]) -> Rover {
        insts.into_iter().fold(self.clone(), |r, i| r.exec(*i))
    }

    fn move_rover(&self) -> Rover {
        use Orientation::*;
        match &self.orientation {
            N => Rover {
                y: self.y + 1,
                ..*self
            },
            S => Rover {
                y: self.y - 1,
                ..*self
            },
            E => Rover {
                x: self.x + 1,
                ..*self
            },
            W => Rover {
                x: self.x - 1,
                ..*self
            },
        }
    }

    fn exec(&self, inst: Instruction) -> Rover {
        match inst {
            Instruction::Move => self.move_rover(),
            Instruction::Left => self.rotate_left(),
            Instruction::Right => self.rotate_right(),
        }
    }

    fn rotate_left(&self) -> Rover {
        use Orientation::*;
        let o = match self.orientation {
            N => W,
            W => S,
            S => E,
            E => N,
        };
        Rover {
            orientation: o,
            ..*self
        }
    }

    fn rotate_right(&self) -> Rover {
        use Orientation::*;
        let o = match self.orientation {
            N => E,
            W => N,
            S => W,
            E => S,
        };
        Rover {
            orientation: o,
            ..*self
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
    fn rover_rotates_left() {
        use Instruction::*;
        let r = Rover {
            x: 1,
            y: 1,
            orientation: N,
        };
        let r = r.rotate_left();
        assert_eq!(
            r,
            Rover {
                x: 1,
                y: 1,
                orientation: W,
            }
        );
        let r = r.rotate_left();
        assert_eq!(
            r,
            Rover {
                x: 1,
                y: 1,
                orientation: S,
            }
        );
        let r = r.rotate_left();
        assert_eq!(
            r,
            Rover {
                x: 1,
                y: 1,
                orientation: E,
            }
        );
        let r = r.rotate_left();
        assert_eq!(
            r,
            Rover {
                x: 1,
                y: 1,
                orientation: N,
            }
        );
    }

    #[test]
    fn rover_rotates_right() {
        use Instruction::*;
        let r = Rover {
            x: 1,
            y: 1,
            orientation: N,
        };
        let r = r.rotate_right();
        assert_eq!(
            r,
            Rover {
                x: 1,
                y: 1,
                orientation: E,
            }
        );
        let r = r.rotate_right();
        assert_eq!(
            r,
            Rover {
                x: 1,
                y: 1,
                orientation: S,
            }
        );
        let r = r.rotate_right();
        assert_eq!(
            r,
            Rover {
                x: 1,
                y: 1,
                orientation: W,
            }
        );
        let r = r.rotate_right();
        assert_eq!(
            r,
            Rover {
                x: 1,
                y: 1,
                orientation: N,
            }
        );
    }

    #[test]
    fn rover_moves() {
        use Instruction::*;
        let r = Rover {
            x: 1,
            y: 1,
            orientation: N,
        };
        let r = r.rotate_right().move_rover();
        assert_eq!(
            r,
            Rover {
                x: 2,
                y: 1,
                orientation: E,
            }
        );
        let r = r.rotate_right().move_rover();
        assert_eq!(
            r,
            Rover {
                x: 2,
                y: 0,
                orientation: S,
            }
        );
        let r = r.rotate_right().move_rover();
        assert_eq!(
            r,
            Rover {
                x: 1,
                y: 0,
                orientation: W,
            }
        );
        let r = r.rotate_right().move_rover();
        assert_eq!(
            r,
            Rover {
                x: 1,
                y: 1,
                orientation: N,
            }
        );
    }

    #[test]
    fn can_execute_a_series_of_commands_1() {
        use Instruction::*;
        let r = Rover {
            x: 1,
            y: 1,
            orientation: N,
        };
        let instructions = [Left /*Left, Move*/];

        let result = r.exec_instructions(&instructions);

        assert_eq!(
            result,
            Rover {
                x: 1,
                y: 1,
                orientation: W,
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

    #[test]
    fn can_parse_Instructions() {
        let (_, insts) = parse_instructions("LRMMLR").unwrap();
        assert_eq!(
            insts,
            vec![
                Instruction::Left,
                Instruction::Right,
                Instruction::Move,
                Instruction::Move,
                Instruction::Left,
                Instruction::Right
            ]
        );
    }

    #[test]
    fn can_run_a_program() {
        let program = "5 5
1 2 N
LMLMLMLMM";
        let r = run_program(program).unwrap();
        assert_eq!(
            r,
            vec![Rover {
                x: 1,
                y: 3,
                orientation: N,
            }]
        );
    }

    #[test]
    fn can_run_a_program_with_multiple_rovers() {
        let program = "5 5
1 2 N
LMLMLMLMM
1 2 N
LMLMLMLMM
";
        let r = run_program(program).unwrap();
        assert_eq!(
            r,
            vec![
                Rover {
                    x: 1,
                    y: 3,
                    orientation: N,
                },
                Rover {
                    x: 1,
                    y: 3,
                    orientation: N,
                }
            ]
        );
    }

    #[test]
    fn test_parse_program() {
        let program = "5 5
1 2 N
LMLMLMLMM
";
        let (rem, programs) = parse_program(program).unwrap();
        assert_eq!(rem.len(), 0);

        assert_eq!(programs.len(), 1);
        assert_eq!(
            programs[0].0,
            Rover {
                x: 1,
                y: 2,
                orientation: N
            }
        );
        assert_eq!(
            programs[0].1,
            vec![
                Instruction::Left,
                Instruction::Move,
                Instruction::Left,
                Instruction::Move,
                Instruction::Left,
                Instruction::Move,
                Instruction::Left,
                Instruction::Move,
                Instruction::Move
            ]
        );
    }
    #[test]
    fn test_parse_program_multiple_rovers() {
        let program = "5 5
1 2 N
LMLMLMLMM
1 2 N
LMLMLMLMM
";
        let (rem, programs) = parse_program(program).unwrap();
        assert_eq!(rem.len(), 0);

        assert_eq!(programs.len(), 2);
        assert_eq!(
            programs[0].0,
            Rover {
                x: 1,
                y: 2,
                orientation: N
            }
        );
        assert_eq!(
            programs[0].1,
            vec![
                Instruction::Left,
                Instruction::Move,
                Instruction::Left,
                Instruction::Move,
                Instruction::Left,
                Instruction::Move,
                Instruction::Left,
                Instruction::Move,
                Instruction::Move
            ]
        );
    }
}
