
pub mod alternate;
pub mod error;
pub mod sequence;
pub mod stream;

use std::collections::HashSet;
use std::hash::Hash;

use error::{ErrItem, ParseError};

type PResult<R = (), E = ()> = Result<R, E>;

pub trait Monoid {
    fn zero() -> Self;
    fn concat(&mut self, other: Self);
}
impl<T: Monoid> Monoid for Option<T> {
    fn zero() -> Self {
        None
    }
    fn concat(&mut self, other: Self) {
        match *self {
            Some(ref mut a) => match other {
                Some(b) => a.concat(b),
                _ => (),
            },
            _ => match other {
                Some(_) => *self = other,
                _ => (),
            },
        }
    }
}
impl<T: Hash + Eq> Monoid for HashSet<T> {
    fn zero() -> Self {
        HashSet::new()
    }
    fn concat(&mut self, mut other: Self) {
        other.drain().for_each(|elem| {
            self.insert(elem);
        })
    }
}

pub trait Parser<S, R = (), E = (), D = (), Err = ()>
where
    Self: Sized,
    S: stream::Stream,
    E: error::ParseError<S::Item, S::Slice, D, Err>,
{
    fn parse(&self, stream: &mut S) -> Result<R, E>;
    fn parse_qualified(&self, stream: &mut S, _err: &E) -> PResult<R, E> {
        self.parse(stream)
    }

    fn eat(&self, stream: &mut S) -> Result<(), E> {
        self.parse(stream).map(|_| ())
    }
    fn eat_qualified(&self, stream: &mut S, _err: &E) -> PResult<(), E> {
        self.eat(stream)
    }

    fn label(self, token_description: D) -> Label<Self, D> {
        label(self, token_description)
    }

    fn alt<P>(self, other: P) -> Alt<Self, P>
    where
        P: Parser<S, R, E, D, Err>,
    {
        alt(self, other)
    }

    fn attempt(self) -> Attempt<Self>
    where
        Attempt<Self>: Parser<S, R, E, D, Err>,
    {
        Attempt(self)
    }
    fn many(self) -> Many0<Self>
    where
        Many0<Self>: Parser<S, Vec<R>, E, D, Err>,
    {
        Many0(self)
    }
    fn many1(self) -> Many1<Self>
    where
        Many0<Self>: Parser<S, Vec<R>, E, D, Err>,
    {
        Many1(self)
    }
    fn optional(self) -> Optional<Self>
    where
        Optional<Self>: Parser<S, Option<R>, E, D, Err>,
    {
        Optional(self)
    }
}

trait NDParser<T, R, E>: Sized {
    type Parsers: Iterator<Item = Self>;
    type Results: Iterator<Item = T>;

    /// Parse a single token nondeterministically, returns an interator of all
    /// possible results, and an iterator of all posible parsers.
    fn parse_nd(&self, elem: T) -> (Self::Parsers, Self::Results);

    /// Parse a single token nondeterministically, returns an interator of all
    /// possible parsers, and a count of all results.
    fn eat_nd(&self, elem: T) -> (Self::Parsers, usize) {
        let (parsers, results) = self.parse_nd(elem);
        (parsers, results.count())
    }
}

fn single_hash<T: Eq + Hash>(elem: T) -> HashSet<T> {
    let mut set = HashSet::with_capacity(1);
    set.insert(elem);
    set
}

/// A parser which never suceeds
#[derive(Debug, Clone)]
pub struct Failure<Err> {
    error: Err,
}
pub fn failure<Err>(error: Err) -> Failure<Err> {
    Failure { error: error }
}
impl<S, R, E, D, Err> Parser<S, R, E, D, Err> for Failure<Err>
where
    Err: Clone,
    S: stream::Stream,
    E: ParseError<S::Item, S::Slice, D, Err>,
{
    fn parse(&self, stream: &mut S) -> Result<R, E> {
        Err(E::fancy_error(stream.get_position(), self.error.clone()))
    }
    fn eat(&self, stream: &mut S) -> Result<(), E> {
        Err(E::fancy_error(stream.get_position(), self.error.clone()))
    }
}

#[derive(Debug, Clone)]
pub struct Label<P, D> {
    parser: P,
    token_description: D,
}
pub fn label<P, D>(parser: P, token_description: D) -> Label<P, D> {
    Label {
        parser: parser,
        token_description: token_description,
    }
}
impl<P, S, R, E, D, Err> Parser<S, R, E, D, Err> for Label<P, D>
where
    P: Parser<S, R, E, D, Err>,
    S: stream::Stream,
    S::Item: Eq + Hash,
    S::Slice: Eq + Hash,
    D: Eq + Hash + Clone,
    E: ParseError<S::Item, S::Slice, D, Err>,
{
    fn parse(&self, stream: &mut S) -> Result<R, E> {
        self.parser.parse(stream).map_err(|mut err| {
            err.label(self.token_description.clone());
            err
        })
    }
    fn eat(&self, stream: &mut S) -> Result<(), E> {
        self.parser.eat(stream).map_err(|mut err| {
            err.label(self.token_description.clone());
            err
        })
    }
}


/// The `Char` parser will try to match a single token of input. It will not
/// consume it's input if it fails.
pub struct Char<T>(T);
pub fn single<T>(token: T) -> Char<T> {
    Char(token)
}
impl<S, R, E, D, Err> Parser<S, R, E, D, Err> for Char<R>
where
    S: stream::Stream<Item = R>,
    S::Slice: Eq + Hash,
    R: Eq + Hash + Clone,
    E: ParseError<S::Item, S::Slice, D, Err>,
    D: Eq + Hash,
{
    fn parse(&self, stream: &mut S) -> Result<R, E> {
        if let Some(token) = stream.peek() {
            if token == self.0 {
                stream.advance();
                Ok(token.clone())
            } else {
                let pos = stream.get_position();
                let unexpected = Some(ErrItem::Token(token.clone()));
                let expected = single_hash(ErrItem::Token(self.0.clone()));
                Err(E::simple_error(pos, unexpected, expected))
            }
        } else {
            let pos = stream.get_position();
            let unexpected = Some(ErrItem::Eof);
            let expected = single_hash(ErrItem::Token(self.0.clone()));
            Err(E::simple_error(pos, unexpected, expected))
        }
    }

    fn eat(&self, stream: &mut S) -> Result<(), E> {
        if let Some(token) = stream.peek() {
            if token == self.0 {
                stream.advance();
                Ok(())
            } else {
                let pos = stream.get_position();
                let unexpected = Some(ErrItem::Token(token.clone()));
                let expected = single_hash(ErrItem::Token(self.0.clone()));
                Err(E::simple_error(pos, unexpected, expected))
            }
        } else {
            let pos = stream.get_position();
            let unexpected = Some(ErrItem::Eof);
            let expected = single_hash(ErrItem::Token(self.0.clone()));
            Err(E::simple_error(pos, unexpected, expected))
        }
    }
}

/// Parse a "chunk" of input. As this is a basic parser, it will not consume any
/// of the stream if it fails.
pub struct Chunk<C>(C);
pub fn chunk<C>(chunk: C) -> Chunk<C> {
    Chunk(chunk)
}
impl<S, R, E, D, Err> Parser<S, R, E, D, Err> for Chunk<R>
where
    S: stream::Stream<Slice = R>,
    S::Item: Eq + Hash,
    R: Clone + Eq + Hash + PartialEq + stream::SliceLen,
    E: ParseError<S::Item, S::Slice, D, Err>,
    D: Eq + Hash,
{
    fn parse(&self, stream: &mut S) -> Result<R, E> {
        let len = self.0.length();
        let pos = stream.get_position();
        stream.lookahead(len).map_or_else(
            || {
                let unexpected = Some(ErrItem::Eof);
                let expected = single_hash(ErrItem::Chunk(self.0.clone()));
                Err(E::simple_error(pos, unexpected, expected))
            },
            |input| {
                if self.0 == input {
                    stream.seek(len);
                    Ok(input)
                } else {
                    let unexpected = Some(ErrItem::Chunk(input));
                    let expected = single_hash(ErrItem::Chunk(self.0.clone()));
                    Err(E::simple_error(pos, unexpected, expected))
                }
            },
        )
    }

    fn eat(&self, stream: &mut S) -> Result<(), E> {
        let len = self.0.length();
        let pos = stream.get_position();
        stream.lookahead(len).map_or_else(
            || {
                let unexpected = Some(ErrItem::Eof);
                let expected = single_hash(ErrItem::Chunk(self.0.clone()));
                Err(E::simple_error(pos, unexpected, expected))
            },
            |input| {
                if self.0 == input {
                    stream.seek(len);
                    Ok(())
                } else {
                    let unexpected = Some(ErrItem::Chunk(input));
                    let expected = single_hash(ErrItem::Chunk(self.0.clone()));
                    Err(E::simple_error(pos, unexpected, expected))
                }
            },
        )
    }
}

#[derive(Debug, Clone)]
pub struct Alt<P, Q> {
    first: P,
    second: Q,
}
pub fn alt<P, Q>(first: P, second: Q) -> Alt<P, Q> {
    Alt {
        first: first,
        second: second,
    }
}
impl<P, Q, S, R, E, D, Err> Parser<S, R, E, D, Err> for Alt<P, Q>
where
    P: Parser<S, R, E, D, Err>,
    Q: Parser<S, R, E, D, Err>,
    S: stream::Stream,
    E: ParseError<S::Item, S::Slice, D, Err>,
{
    fn parse(&self, stream: &mut S) -> Result<R, E> {
        match self.first.parse(stream) {
            Ok(result1) => Ok(result1),
            Err(mut e1) => match self.second.parse(stream) {
                Ok(result2) => Ok(result2),
                Err(e2) => {
                    e1.concat(e2);
                    Err(e1)
                }
            },
        }
    }

    fn eat(&self, stream: &mut S) -> Result<(), E> {
        match self.first.eat(stream) {
            Ok(_) => Ok(()),
            Err(mut e1) => match self.second.eat(stream) {
                Ok(_) => Ok(()),
                Err(e2) => {
                    e1.concat(e2);
                    Err(e1)
                }
            },
        }
    }
}

/// An `Attempt` will try to apply a parser to a strem. If it succeeds, it will
/// consume input, if it fails, it will pretend that it consumed no imput.
pub struct Attempt<P>(P);
pub fn attempt<P>(parser: P) -> Attempt<P> {
    Attempt(parser)
}
impl<P, S, R, E, D, Err> Parser<S, R, E, D, Err> for Attempt<P>
where
    S: stream::Stream,
    P: for<'a> Parser<stream::Conjecture<'a, S>, R, E, D, Err>,
    E: ParseError<S::Item, S::Slice, D, Err>,
{
    fn parse(&self, stream: &mut S) -> Result<R, E> {
        stream.conjecture(|conj| self.0.parse(conj))
    }

    fn eat(&self, stream: &mut S) -> Result<(), E> {
        stream.conjecture(|conj| self.0.eat(conj))
    }
}

pub struct Many0<P>(P);
pub fn many0<P>(parser: P) -> Many0<P> {
    Many0(parser)
}
impl<P, S, R, E, D, Err> Parser<S, Vec<R>, E, D, Err> for Many0<P>
where
    P: Parser<S, R, E, D, Err>,
    S: stream::Stream,
    E: ParseError<S::Item, S::Slice, D, Err>,
{
    fn parse(&self, stream: &mut S) -> Result<Vec<R>, E> {
        let mut results = Vec::new();
        loop {
            match self.0.parse(stream) {
                Ok(result) => results.push(result),
                Err(_) => return Ok(results),
            };
        }
    }

    fn eat(&self, stream: &mut S) -> Result<(), E> {
        loop {
            match self.0.parse(stream) {
                Ok(_) => (),
                Err(_) => return Ok(()),
            };
        }
    }
}

pub struct Many1<P>(P);
impl<P, S, R, E, D, Err> Parser<S, Vec<R>, E, D, Err> for Many1<P>
where
    P: Parser<S, R, E, D, Err>,
    S: stream::Stream,
    E: ParseError<S::Item, S::Slice, D, Err>,
{
    fn parse(&self, stream: &mut S) -> Result<Vec<R>, E> {
        let mut results = vec![self.0.parse(stream)?];
        loop {
            match self.0.parse(stream) {
                Ok(result) => results.push(result),
                Err(_) => return Ok(results),
            };
        }
    }

    fn eat(&self, stream: &mut S) -> Result<(), E> {
        self.0.parse(stream)?;
        loop {
            match self.0.parse(stream) {
                Ok(_) => (),
                Err(_) => return Ok(()),
            };
        }
    }
}

pub struct Optional<P>(P);
impl<P, S, R, E, D, Err> Parser<S, Option<R>, E, D, Err> for Optional<P>
where
    P: Parser<S, R, E, D, Err>,
    S: stream::Stream,
    E: ParseError<S::Item, S::Slice, D, Err>,
{
    fn parse(&self, stream: &mut S) -> Result<Option<R>, E> {
        match self.0.parse(stream) {
            Ok(result) => Ok(Some(result)),
            Err(_) => Ok(None),
        }
    }

    fn eat(&self, stream: &mut S) -> Result<(), E> {
        match self.0.eat(stream) {
            Ok(_) => Ok(()),
            Err(_) => Ok(()),
        }
    }
}


#[cfg(test)]
mod test {
    use super::*;
    use error::{FullError, SourcePos};
    use stream::{CachedIterator, Chars, Stream};

    #[test]
    fn char_parses() {
        let parser = single('H');

        let mut string1 = CachedIterator::from("Fello, World!".chars());
        let mut string2 = CachedIterator::from("Hello, World!".chars());

        assert_eq!(parser.parse(&mut string1), Err(()));
        assert_eq!(string1.remove(2), vec!['F', 'e']);

        let result: Result<char, ()> = Ok('H');
        assert_eq!(parser.parse(&mut string2), result);
        assert_eq!(string2.remove(2), vec!['e', 'l']);
    }

    #[test]
    fn label_modifies_error() {
        let char_parser = label(single('H'), "failed with message");
        let chunk_parser = label(chunk("Hello".chars().collect()), "failed again");

        let mut string1 = CachedIterator::from("Fello, World!".chars());
        let mut string2 = CachedIterator::from("Heffo, World!".chars());

        let result: Result<char, FullError<_, _, _, ()>> = Err(FullError::Simple(
            SourcePos::zero(),
            Some(ErrItem::Token('F')),
            std::iter::once(ErrItem::Description("failed with message")).collect(),
        ));
        assert_eq!(char_parser.parse(&mut string1), result);
        assert_eq!(string1.remove(2), vec!['F', 'e']);

        let result: Result<_, FullError<_, _, _, ()>> = Err(FullError::Simple(
            SourcePos::zero(),
            Some(ErrItem::Chunk(vec!['H', 'e', 'f', 'f', 'o'])),
            std::iter::once(ErrItem::Description("failed again")).collect(),
        ));
        assert_eq!(chunk_parser.parse(&mut string2), result);
        assert_eq!(string2.remove(3), vec!['H', 'e', 'f']);
    }

    #[test]
    fn chunk_parses() {
        let parser = chunk("Hello".chars().collect());

        let mut string1 = CachedIterator::from("Hello, World!".chars());
        let mut string2 = CachedIterator::from("Heffo, World!".chars());

        let result: Result<_, ()> = parser.parse(&mut string1);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().into_iter().collect::<String>(), "Hello");
        assert_eq!(string1.remove(3), vec![',', ' ', 'W']);

        let result: Result<_, ()> = parser.parse(&mut string2);
        assert!(result.is_err());
        assert_eq!(string2.remove(3), vec!['H', 'e', 'f']);
    }

    #[test]
    fn chunk_parses_str() {
        let parser = chunk("Hello");

        let mut string1 = Chars::new("Hello, World!");
        let mut string2 = Chars::new("Heffo, World!");

        let result: Result<_, ()> = parser.parse(&mut string1);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), "Hello");
        assert_eq!(string1.remove(3), vec![',', ' ', 'W']);

        let result: Result<_, ()> = parser.parse(&mut string2);
        assert!(result.is_err());
        assert_eq!(string2.remove(3), vec!['H', 'e', 'f']);
    }

    #[test]
    fn attempt_parses() {
        let parser = attempt(chunk("Hello".chars().collect()));

        let mut string1 = CachedIterator::from("Hello, World!".chars());
        let mut string2 = CachedIterator::from("Heffo, World!".chars());

        let result: Result<_, ()> = parser.parse(&mut string2);
        assert!(result.is_err());
        assert_eq!(string2.remove(3), vec!['H', 'e', 'f']);

        let result: Result<_, ()> = parser.parse(&mut string1);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().into_iter().collect::<String>(), "Hello");
        assert_eq!(string1.remove(3), vec![',', ' ', 'W']);
    }

    #[test]
    fn alt_parses() {
        let parser = alt(
            attempt(chunk("Hello".chars().collect())),
            chunk("Heffo".chars().collect()),
        );

        let mut string1 = CachedIterator::from("Hello, World!".chars());
        let mut string2 = CachedIterator::from("Heffo, World!".chars());

        let result: Result<_, ()> = parser.parse(&mut string1);
        assert_eq!(result.unwrap().into_iter().collect::<String>(), "Hello");
        assert_eq!(string1.remove(3), vec![',', ' ', 'W']);

        let result: Result<_, ()> = parser.parse(&mut string2);
        assert_eq!(result.unwrap().into_iter().collect::<String>(), "Heffo");
        assert_eq!(string2.remove(3), vec![',', ' ', 'W']);
    }


    #[test]
    fn simple_grammar() {
        let ws_char = alt(alt(single(' '), single('\t')), single('\n'));

        let ws = many0(ws_char);

        let digit = single('0')
            .alt(single('1'))
            .alt(single('2'))
            .alt(single('3'))
            .alt(single('4'))
            .alt(single('5'))
            .alt(single('6'))
            .alt(single('7'))
            .alt(single('8'))
            .alt(single('9'));

        let sexp = (single('('), ws, digit, ws, single(')'));

        let mut string1 = Chars::new("(     5    \t  )");
        let mut string2 = Chars::new("Heffo, World!");

        let result: Result<_, ()> = sexp.parse(&mut string1);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), ('(', vec![], '5', vec![], ')'));
        assert_eq!(string1.remove(3), vec![',', ' ', 'W']);

        let result: Result<_, ()> = sexp.parse(&mut string2);
        assert!(result.is_err());
        assert_eq!(string2.remove(3), vec!['H', 'e', 'f']);
    }
}