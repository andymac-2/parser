use std::collections::HashSet;
use std::hash::Hash;

use super::Monoid;

pub trait ParseError<T, C, D = (), E = ()>: Monoid {
    fn simple_error(
        position: SourcePos,
        unexpected: Option<ErrItem<T, C, D>>,
        expected: HashSet<ErrItem<T, C, D>>,
    ) -> Self;
    fn description(&mut self, label: D);
    fn fancy_error(position: SourcePos, error: E) -> Self;
    fn get_position(&self) -> SourcePos;
}

#[derive(Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum ErrItem<T, C, D> {
    Description(D),
    Chunk(C),
    Token(T),
    Eof,
}
impl<T, C, D> Monoid for ErrItem<T, C, D> {
    fn zero() -> Self {
        ErrItem::Eof
    }

    // TODO improve this later.
    fn concat(&mut self, other: Self) {
        use ErrItem::{Chunk, Description, Eof, Token};
        match *self {
            Description(_) => (),
            Chunk(_) => match other {
                Description(_) => *self = other,
                _ => (),
            },
            Token(_) => match other {
                Description(_) => *self = other,
                Chunk(_) => *self = other,
                _ => (),
            },
            Eof => match other {
                Eof => (),
                _ => *self = other,
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FullError<T: Hash + Eq, C: Hash + Eq, D: Hash + Eq, E> {
    Simple(
        SourcePos,
        Option<ErrItem<T, C, D>>,
        HashSet<ErrItem<T, C, D>>,
    ),
    Fancy(SourcePos, E),
}
impl<T, C, D, E> Monoid for FullError<T, C, D, E>
where
    T: Hash + Eq,
    C: Hash + Eq,
    D: Hash + Eq,
    E: Monoid,
{
    fn zero() -> Self {
        FullError::Simple(SourcePos::zero(), Option::zero(), HashSet::zero())
    }
    fn concat(&mut self, other: Self) {
        use FullError::{Fancy, Simple};
        if self.get_position() < other.get_position() {
            return;
        }
        if self.get_position() > other.get_position() {
            *self = other;
            return;
        }

        match *self {
            Fancy(_, ref mut err1) => match other {
                Fancy(_, err2) => err1.concat(err2),
                _ => (),
            },
            Simple(_, ref mut unexpected1, ref mut expected1) => match other {
                Fancy(_, _) => *self = other,
                Simple(_, unexpected2, expected2) => {
                    unexpected1.concat(unexpected2);
                    expected1.concat(expected2);
                }
            },
        }
    }
}
impl<T, C, D, E> ParseError<T, C, D, E> for FullError<T, C, D, E>
where
    T: Hash + Eq,
    C: Hash + Eq,
    D: Hash + Eq,
    E: Monoid,
{
    fn simple_error(
        position: SourcePos,
        unexpected: Option<ErrItem<T, C, D>>,
        expected: HashSet<ErrItem<T, C, D>>,
    ) -> Self {
        FullError::Simple(position, unexpected, expected)
    }
    fn description(&mut self, label: D) {
        match *self {
            FullError::Simple(_, _, ref mut expected) => {
                *expected = super::single_hash(ErrItem::Description(label));
            }
            FullError::Fancy(_, _) => (),
        }
    }
    fn fancy_error(position: SourcePos, error: E) -> Self {
        FullError::Fancy(position, error)
    }
    fn get_position(&self) -> SourcePos {
        match *self {
            FullError::Simple(ref pos, _, _) => pos.clone(),
            FullError::Fancy(ref pos, _) => pos.clone(),
        }
    }
}

impl Monoid for () {
    fn zero() -> Self {}
    fn concat(&mut self, _other: Self) {}
}
impl<T, C> ParseError<T, C, (), ()> for () {
    fn simple_error(
        _position: SourcePos,
        _unexpected: Option<ErrItem<T, C, ()>>,
        _expected: HashSet<ErrItem<T, C, ()>>,
    ) -> Self {
    }
    fn description(&mut self, _label: ()) {}
    fn fancy_error(_position: SourcePos, _error: ()) -> Self {}
    fn get_position(&self) -> SourcePos {
        SourcePos(0)
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct SourcePos(pub usize);
impl Monoid for SourcePos {
    fn zero() -> Self {
        SourcePos(0)
    }
    fn concat(&mut self, other: Self) {
        self.0 = std::cmp::min(self.0, other.0);
    }
}
impl<T, C> ParseError<T, C, (), ()> for SourcePos {
    fn simple_error(
        position: SourcePos,
        _unexpected: Option<ErrItem<T, C, ()>>,
        _expected: HashSet<ErrItem<T, C, ()>>,
    ) -> Self {
        position
    }
    fn description(&mut self, _label: ()) {}
    fn fancy_error(position: SourcePos, _error: ()) -> Self {
        position
    }
    fn get_position(&self) -> SourcePos {
        self.clone()
    }
}