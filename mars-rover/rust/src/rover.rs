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

impl Rover {
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
}
