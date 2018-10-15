#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Dir { Up, Down }
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Side { Left, Right }

impl Side {
    pub fn as_str(&self) -> &str {
        match *self {
            Side::Left => "left",
            Side::Right => "right",
        }
    }
}

impl Dir {
    pub fn opposite(&self) -> Self {
        match *self {
            Dir::Up => Dir::Down,
            Dir::Down => Dir::Up,
        }
    }
}

