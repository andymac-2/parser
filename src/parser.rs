use crate::stream::Stream;
use std::marker::PhantomData;
use std::cmp::min;

trait Monoid {
    const ZERO: Self;
    fn concat(&mut self, other: Self);
}

trait Parser<T = char, R = (), E = SourcePos, Tok = (), Err = ()>
where
    Self: Sized,
    E: ParseError<Tok, Err>,
{
    fn parse<S>(&self, stream: &mut S) -> Result<R, E>
    where
        S: Stream<Item = T>;

    fn eat<S>(&self, stream: &mut S) -> Result<(), E>
    where
        S: Stream<Item = T>,
    {
        self.parse(stream).map(|_| ())
    }

    fn label(self, token_description: Tok) -> Label<Self, Tok> {
        Label::new(self, token_description)
    }

    fn alt<P>(self, other: P) -> Alt<Self, P>
    where
        P: Parser<T, R, E, Tok, Err>,
    {
        Alt::new(self, other)
    }

    fn attempt(self) -> Attempt<Self> {
        Attempt::new(self)
    }

    fn many0(self) -> Many0<Self> {
        Many0::new(self)
    }

    fn many1(self) -> Many1<Self> {
        Many1::new(self)
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
pub enum ErrorItem<Tok> {
    TokenDescription(Tok),
    EndOfFile,
}
impl<Tok> ErrorItem<Tok> {
    pub fn tok(token: Tok) -> Self {
        ErrorItem::TokenDescription(token)
    }
    pub fn eof() -> Self {
        ErrorItem::EndOfFile
    }
}

pub trait ParseError<Tok, Err>: Monoid {
    fn simple_error (position: SourcePos, unexpected: Option<ErrorItem<Tok>>, expected: Vec<ErrorItem<Tok>>) -> Self;
    fn fancy_error (position: SourcePos, error: Err) -> Self;
    fn get_position (&self) -> SourcePos;
}

#[derive(Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct SourcePos(pub usize);
impl Monoid for SourcePos {
    const ZERO: SourcePos = SourcePos(0);
    fn concat(&mut self, other: Self) {
        self.0 = min(self.0, other.0);
    }
}
impl<Tok, Err> ParseError<Tok, Err> for SourcePos {
    fn simple_error (position: SourcePos, _unexpected: Option<ErrorItem<Tok>>, _expected: Vec<ErrorItem<Tok>>) -> Self {
        position
    }
    fn fancy_error (position: SourcePos, _error: Err) -> Self {
        position
    }
    fn get_position (&self) -> SourcePos {
        self.clone()
    }
}

/// A parser which never suceeds
#[derive(Debug, Clone)]
struct Failure<Err> {
    error: Err,
}
impl<Err> Failure<Err> {
    fn new(error: Err) -> Self {
        Failure {
            error: error,
        }
    }
}
impl<T, R, E, Err, Tok> Parser<T, R, E, Tok, Err> for Failure<Err>
where
    E: ParseError<Tok, Err>,
    Err: Clone,
{
    fn parse<S>(&self, stream: &mut S) -> Result<R, E>
    where
        S: Stream<Item = T>,
    {
        Err(E::fancy_error(stream.get_position(), self.error.clone()))
    }
    fn eat<S>(&self, stream: &mut S) -> Result<(), E>
    where
        S: Stream<Item = T>,
    {
        Err(E::fancy_error(stream.get_position(), self.error.clone()))
    }
}

#[derive(Debug, Clone)]
struct Label<P, Tok> {
    parser: P,
    token_description: Tok,
}
impl<P, Tok> Label<P, Tok> {
    fn new(parser: P, token_description: Tok) -> Self {
        Label {
            parser: parser,
            token_description: token_description,
        }
    }
}
impl<P, T, R, E, Tok, Err> Parser<T, R, E, Tok, Err> for Label<P, Tok>
where
    P: Parser<T, R, E, Tok, Err>,
    E: ParseError<Tok, Err>,
    Tok: Clone,
{
    fn parse<S>(&self, stream: &mut S) -> Result<R, E>
    where
        S: Stream<Item = T>,
    {
        self.parser.parse(stream).map_err(|err| {
            let item = ErrorItem::tok(self.token_description.clone());
            err.concat(E::simple_error(SourcePos::ZERO, None, vec![item]));
            err
        })
    }
    fn eat<S>(&self, stream: &mut S) -> Result<(), E>
    where
        S: Stream<Item = T>,
    {
        self.parser.eat(stream).map_err(|err| {
            let item = ErrorItem::tok(self.token_description.clone());
            err.concat(E::simple_error(SourcePos::ZERO, None, vec![item]));
            err
        })
    }
}


/// The `Char` parser will try to match a single token of input. It will not
/// consume it's input if it fails.
struct Char<T>(T);
impl<T> Char<T> {
    fn new(token: T) -> Self {
        Char(token)
    }
}
impl<T, E, Tok, Err> Parser<T, T, E, Tok, Err> for Char<T>
where
    Tok: From<T>,
    E: ParseError<Tok, Err>,
    T: PartialEq + Clone + std::fmt::Debug,
{
    fn parse<S>(&self, stream: &mut S) -> Result<T, E>
    where
        S: Stream<Item = T>,
    {
        if let Some(token) = stream.peek() {
            if token == self.0 {
                stream.advance();
                Ok(token.clone())
            }
            else {
                let pos = stream.get_position();
                let unexpected = Some(ErrorItem::tok(Tok::from(token.clone())));
                let expected = vec![ErrorItem::tok(Tok::from(self.0.clone()))];
                let error = E::simple_error(pos, unexpected, expected);
                Err(error)
            }
        }
        else {
            let pos = stream.get_position();
            let unexpected = Some(ErrorItem::eof());
            let expected = vec![ErrorItem::tok(Tok::from(self.0.clone()))];
            let error = E::simple_error(pos, unexpected, expected);
            Err(error)
        }
    }

    fn eat<S>(&self, stream: &mut S) -> Result<(), E>
    where
        S: Stream<Item = T>,
    {
        if let Some(token) = stream.peek() {
            if token == self.0 {
                stream.advance();
                Ok(())
            }
            else {
                let pos = stream.get_position();
                let unexpected = Some(ErrorItem::tok(Tok::from(token.clone())));
                let expected = vec![ErrorItem::tok(Tok::from(self.0.clone()))];
                let error = E::simple_error(pos, unexpected, expected);
                Err(error)
            }
        }
        else {
            let pos = stream.get_position();
            let unexpected = Some(ErrorItem::eof());
            let expected = vec![ErrorItem::tok(Tok::from(self.0.clone()))];
            let error = E::simple_error(pos, unexpected, expected);
            Err(error)
        }
    }
}

struct Chunk<C>(C);
impl<T, C, E, Tok, Err> Parser<T, C, E, Tok, Err> for Chunk<C>
where
    E: ParseError<Tok, Err>,
    T: PartialEq,
    C: IntoIterator<Item = T> + Clone,
{
    fn parse<S>(&self, stream: &mut S) -> Result<C, E>
    where
        S: Stream<Item = T>,
    {
        let mut chunk = self.0.clone().into_iter();
        loop {
            if let Some(chunk_char) = chunk.next() {
                if let Some(stream_char) = stream.next() {
                    if stream_char != chunk_char {
                        return Err(());
                    }
                } else {
                    return Err(());
                }
            } else {
                return Ok(self.0.clone());
            }
        }
    }

    fn eat<S>(&self, stream: &mut S) -> Result<(), E>
    where
        S: Stream<Item = T>,
    {
        let mut chunk = self.0.clone().into_iter();
        loop {
            if let Some(chunk_char) = chunk.next() {
                if let Some(stream_char) = stream.next() {
                    if stream_char != chunk_char {
                        return Err(());
                    }
                } else {
                    return Err(());
                }
            } else {
                return Ok(());
            }
        }
    }
}

#[derive(Debug, Clone)]
struct Alt<P, Q> {
    first: P,
    second: Q,
}
impl<P, Q> Alt<P, Q> {
    fn new(first: P, second: Q) -> Self {
        Alt {
            first: first,
            second: second,
        }
    }
}
impl<P, Q, T, R, E, Tok, Err> Parser<T, R, E, Tok, Err> for Alt<P, Q>
where
    P: Parser<T, R, E, Tok, Err>,
    Q: Parser<T, R, E, Tok, Err>,
    E: ParseError<Tok, Err>,
{
    fn parse<S>(&self, stream: &mut S) -> Result<R, E>
    where
        S: Stream<Item = T>,
    {
        match self.first.parse(stream) {
            Ok(result) => Ok(result),
            Err(_) => self.second.parse(stream),
        }
    }

    fn eat<S>(&self, stream: &mut S) -> Result<(), E>
    where
        S: Stream<Item = T>,
    {
        match self.first.parse(stream) {
            Ok(_) => Ok(()),
            Err(_) => self.second.parse(stream).map(|_| ()),
        }
    }
}

/// An `Attempt` will try to apply a parser to a strem. If it succeeds, it will
/// consume input, if it fails, it will pretend that it consumed no imput.
struct Attempt<P>(P);
impl<P> Attempt<P> {
    fn new(parser: P) -> Self {
        Attempt(parser)
    }
}
impl<P, T, R, E, Tok, Err> Parser<T, R, E, Tok, Err> for Attempt<P>
where
    P: Parser<T, R, E, Tok, Err>,
    E: ParseError<Tok, Err>,
{
    fn parse<S>(&self, stream: &mut S) -> Result<R, E>
    where
        S: Stream<Item = T>,
    {
        stream.conjecture(|conj| self.0.parse(conj))
    }

    fn eat<S>(&self, stream: &mut S) -> Result<(), E>
    where
        S: Stream<Item = T>,
    {
        stream.conjecture(|conj| self.0.eat(conj))
    }
}

struct Many0<P>(P);
impl<P> Many0<P> {
    fn new(parser: P) -> Self {
        Many0(parser)
    }
}
impl<P, T, R, E, Tok, Err> Parser<T, Vec<R>, E, Tok, Err> for Many0<P>
where
    P: Parser<T, R, E, Tok, Err>,
    E: ParseError<Tok, Err>,
{
    fn parse<S>(&self, stream: &mut S) -> Result<Vec<R>, E>
    where
        S: Stream<Item = T>,
    {
        let mut results = Vec::new();
        loop {
            match self.0.parse(stream) {
                Ok(result) => results.push(result),
                Err(_) => return Ok(results),
            };
        }
    }

    fn eat<S>(&self, stream: &mut S) -> Result<(), E>
    where
        S: Stream<Item = T>,
    {
        loop {
            match self.0.parse(stream) {
                Ok(_) => (),
                Err(_) => return Ok(()),
            };
        }
    }
}

struct Many1<P>(P);
impl<P> Many1<P> {
    fn new(parser: P) -> Self {
        Many1(parser)
    }
}
impl<P, T, R, E, Tok, Err> Parser<T, Vec<R>, E, Tok, Err> for Many1<P>
where
    P: Parser<T, R, E, Tok, Err>,
    E: ParseError<Tok, Err>,
{
    fn parse<S>(&self, stream: &mut S) -> Result<Vec<R>, E>
    where
        S: Stream<Item = T>,
    {
        let mut results = vec![self.0.parse(stream)?];
        loop {
            match self.0.parse(stream) {
                Ok(result) => results.push(result),
                Err(_) => return Ok(results),
            };
        }
    }

    fn eat<S>(&self, stream: &mut S) -> Result<(), E>
    where
        S: Stream<Item = T>,
    {
        self.0.parse(stream)?;
        loop {
            match self.0.parse(stream) {
                Ok(_) => (),
                Err(_) => return Ok(()),
            };
        }
    }
}

struct Optional<P>(P);
impl<P> Optional<P> {
    fn new(parser: P) -> Self {
        Optional(parser)
    }
}
impl<P, T, R, E, Tok, Err> Parser<T, Option<R>, E, Tok, Err> for Optional<P>
where
    P: Parser<T, R, E, Tok, Err>,
    E: ParseError<Tok, Err>,
{
    fn parse<S>(&self, stream: &mut S) -> Result<Option<R>, E>
    where
        S: Stream<Item = T>,
    {
        match self.0.parse(stream) {
            Ok(result) => Ok(Some(result)),
            Err(_) => Ok(None),
        }
    }

    fn eat<S>(&self, stream: &mut S) -> Result<(), E>
    where
        S: Stream<Item = T>,
    {
        match self.0.eat(stream) {
            Ok(_) => Ok(()),
            Err(_) => Ok(()),
        }
    }
}


#[cfg(test)]
mod test {
    use super::Char;
    use super::Chunk;
    use super::Parser;
    use crate::stream::{CachedIterator, Stream};

    #[test]
    fn char_parses() {
        let parser = Char::new('H');

        let mut string1 = CachedIterator::from("Fello, World!".chars());
        let mut string2 = CachedIterator::from("Hello, World!".chars());

        assert_eq!(parser.parse(&mut string1), Err(()));
        assert_eq!(string1.remove(2), vec!['F', 'e']);

        assert_eq!(parser.parse(&mut string2), Ok('H'));
        assert_eq!(string2.remove(2), vec!['e', 'l']);
    }

    #[test]
    fn label_modifies_error() {
        let char_parser = Char::new('H').label("failed with message");
        let chunk_parser = Chunk("Hello".chars()).label("failed again");

        let mut string1 = CachedIterator::from("Fello, World!".chars());
        let mut string2 = CachedIterator::from("Heffo, World!".chars());

        assert_eq!(char_parser.parse(&mut string1), Err("failed with message"));
        assert_eq!(string1.remove(2), vec!['F', 'e']);

        assert_eq!(
            chunk_parser.parse(&mut string2).unwrap_err(),
            "failed again"
        );
        assert_eq!(string2.remove(3), vec!['f', 'o', ',']);
    }

    #[test]
    fn chunk_parses() {
        let parser = Chunk("Hello".chars());

        let mut string1 = CachedIterator::from("Hello, World!".chars());
        let mut string2 = CachedIterator::from("Heffo, World!".chars());

        let result = parser.parse(&mut string1);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().collect::<String>(), "Hello");
        assert_eq!(string1.remove(3), vec![',', ' ', 'W']);

        let result = parser.parse(&mut string2);
        assert!(result.is_err());
        assert_eq!(string2.remove(3), vec!['f', 'o', ',']);
    }

    #[test]
    fn attempt_parses() {
        let parser = Chunk("Hello".chars()).attempt();

        let mut string1 = CachedIterator::from("Hello, World!".chars());
        let mut string2 = CachedIterator::from("Heffo, World!".chars());

        let result = parser.parse(&mut string2);
        assert!(result.is_err());
        assert_eq!(string2.remove(3), vec!['H', 'e', 'f']);

        let result = parser.parse(&mut string1);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().collect::<String>(), "Hello");
        assert_eq!(string1.remove(3), vec![',', ' ', 'W']);
    }

    #[test]
    fn alt_parses() {
        let parser = Chunk("Hello".chars()).attempt().alt(Chunk("Heffo".chars()));

        let mut string1 = CachedIterator::from("Hello, World!".chars());
        let mut string2 = CachedIterator::from("Heffo, World!".chars());

        let result = parser.parse(&mut string1);
        assert_eq!(result.unwrap().collect::<String>(), "Hello");
        assert_eq!(string1.remove(3), vec![',', ' ', 'W']);

        let result = parser.parse(&mut string2);
        assert_eq!(result.unwrap().collect::<String>(), "Heffo");
        assert_eq!(string2.remove(3), vec![',', ' ', 'W']);
    }
}