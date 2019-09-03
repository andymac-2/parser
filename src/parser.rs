
pub mod alternate;
pub mod bracketed;
pub mod error;
pub mod map;
pub mod multi;
pub mod one_of;
pub mod sequence;
pub mod stream;
pub mod token;

pub use alternate::Alt;
pub use bracketed::{between, first, second, Between, First, Second};
pub use map::{map, Map};
pub use multi::{count, many, many1, sep_by, Count, Many0, Many1, SepBy};
pub use one_of::{one_of, OneOf};
pub use token::{satisfy, token, Token};

use std::collections::HashSet;
use std::hash::Hash;

use alternate::Alt2;
use error::{ErrItem, ParseError};
use stream::SliceLen;


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

pub trait Parser<S, E = (), D = (), Err = ()>
where
    S: stream::Stream,
    E: error::ParseError<S::Item, S::Slice, D, Err>,
{
    type Output;
    fn parse(&self, stream: &mut S) -> Result<Self::Output, E>;
    fn parse_qualified(&self, stream: &mut S, _err: &E) -> PResult<Self::Output, E> {
        self.parse(stream)
    }

    fn eat(&self, stream: &mut S) -> Result<(), E> {
        self.parse(stream).map(|_| ())
    }
    fn eat_qualified(&self, stream: &mut S, _err: &E) -> PResult<(), E> {
        self.eat(stream)
    }
}

pub trait Combinator
where
    Self: Sized,
{
    fn label<D>(self, token_description: D) -> Label<Self, D>
    where
        Label<Self, D>: Combinator,
    {
        Label {
            parser: self,
            token_description: token_description,
        }
    }
    fn attempt<R>(self) -> Attempt<Self, R>
    where
        Attempt<Self, R>: Combinator,
    {
        attempt(self)
    }
    fn many(self) -> Many0<Self>
    where
        Many0<Self>: Combinator,
    {
        many(self)
    }
    fn many1(self) -> Many1<Self>
    where
        Many1<Self>: Combinator,
    {
        many1(self)
    }
    fn count(self, num: usize) -> Count<Self>
    where
        Count<Self>: Combinator,
    {
        count(self, num)
    }
    fn optional(self) -> Optional<Self>
    where
        Optional<Self>: Combinator,
    {
        Optional(self)
    }
    fn void(self) -> Void<Self>
    where
        Void<Self>: Combinator,
    {
        Void { parser: self }
    }
    fn value<R>(self, value: R) -> Value<Self, R>
    where
        Value<Self, R>: Combinator,
    {
        Value {
            parser: self,
            result: value,
        }
    }
    fn first<P1>(self, other: P1) -> First<Self, P1>
    where
        P1: Combinator,
        First<Self, P1>: Combinator,
    {
        first(self, other)
    }
    fn second<P1: Combinator>(self, other: P1) -> Second<Self, P1>
    where
        P1: Combinator,
        Second<Self, P1>: Combinator,
    {
        second(self, other)
    }
    fn sep_by<P1>(self, delimiter: P1) -> SepBy<Self, P1>
    where
        P1: Combinator,
        SepBy<Self, P1>: Combinator,
    {
        sep_by(self, delimiter)
    }
    fn map<F, R>(self, func: F) -> Map<Self, F, R>
    where
        Map<Self, F, R>: Combinator,
    {
        map(self, func)
    }
    fn alt<P1>(self, other: P1) -> Alt2<Self, P1>
    where
        P1: Combinator,
        Alt2<Self, P1>: Combinator,
    {
        alternate::alt2(self, other)
    }
    fn then<P1>(self, other: P1) -> (Self, P1)
    where
        P1: Combinator,
        (Self, P1): Combinator,
    {
        (self, other)
    }
    fn between<L, R>(self, left: L, right: R) -> Between<L, Self, R>
    where
        L: Combinator,
        R: Combinator,
        Between<L, Self, R>: Combinator,
    {
        between(self, left, right)
    }
}

impl<'p, P> Combinator for &'p P {}
impl<'p, P, S, E, D, Err> Parser<S, E, D, Err> for &'p P
where
    P: Parser<S, E, D, Err>,
    S: stream::Stream,
    E: error::ParseError<S::Item, S::Slice, D, Err>,
{
    type Output = P::Output;
    fn parse(&self, stream: &mut S) -> Result<Self::Output, E> {
        (*self).parse(stream)
    }
    fn eat(&self, stream: &mut S) -> Result<(), E> {
        (*self).eat(stream)
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
pub struct Func<F, R> {
    func: F,
    _output: std::marker::PhantomData<*const R>,
}
impl<F, R> Combinator for Func<F, R> {}
pub fn func<F, S, R, E, D, Err>(func: F) -> Func<F, R>
where
    F: Fn(&mut S) -> Result<R, E>,
    S: stream::Stream,
    E: ParseError<S::Item, S::Slice, D, Err>,
{
    Func {
        func: func,
        _output: std::marker::PhantomData,
    }
}
impl<F, R, S, E, D, Err> Parser<S, E, D, Err> for Func<F, R>
where
    F: Fn(&mut S) -> Result<R, E>,
    Err: Clone,
    S: stream::Stream,
    E: ParseError<S::Item, S::Slice, D, Err>,
{
    type Output = R;
    fn parse(&self, stream: &mut S) -> Result<Self::Output, E> {
        (self.func)(stream)
    }
    fn eat(&self, stream: &mut S) -> Result<(), E> {
        match (self.func)(stream) {
            Ok(_) => Ok(()),
            Err(e) => Err(e),
        }
    }
}

/// A parser which never suceeds
#[derive(Debug, Clone)]
pub struct Failure<Err> {
    error: Err,
}
impl<Err> Combinator for Failure<Err> {}
pub fn failure<Err>(error: Err) -> Failure<Err> {
    Failure { error: error }
}
impl<S, E, D, Err> Parser<S, E, D, Err> for Failure<Err>
where
    Err: Clone,
    S: stream::Stream,
    E: ParseError<S::Item, S::Slice, D, Err>,
{
    type Output = ();
    fn parse(&self, stream: &mut S) -> Result<Self::Output, E> {
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
impl<P, D> Combinator for Label<P, D> {}
pub fn label<P, D>(parser: P, token_description: D) -> Label<P, D> {
    Label {
        parser: parser,
        token_description: token_description,
    }
}
impl<P, S, E, D, Err> Parser<S, E, D, Err> for Label<P, D>
where
    P: Parser<S, E, D, Err>,
    S: stream::Stream,
    D: Eq + Hash + Clone,
    E: ParseError<S::Item, S::Slice, D, Err>,
{
    type Output = P::Output;
    fn parse(&self, stream: &mut S) -> Result<Self::Output, E> {
        self.parser.parse(stream).map_err(|mut err: E| {
            let desc: D = self.token_description.clone();
            err.description(desc);
            err
        })
    }
    fn eat(&self, stream: &mut S) -> Result<(), E> {
        self.parser.eat(stream).map_err(|mut err| {
            let desc: D = self.token_description.clone();
            err.description(desc);
            err
        })
    }
}


/// The `Char` parser will try to match a single token of input. It will not
/// consume it's input if it fails.
#[derive(Debug, Clone, Hash, Eq, Ord, PartialOrd, PartialEq)]
pub struct Char<T>(T);
impl<T> Combinator for Char<T> {}
pub fn single<T>(token: T) -> Char<T> {
    Char(token)
}
impl<S, E, D, Err> Parser<S, E, D, Err> for Char<S::Item>
where
    S: stream::Stream,
    S::Slice: Eq + Hash,
    S::Item: Eq + Hash + Clone,
    E: ParseError<S::Item, S::Slice, D, Err>,
    D: Eq + Hash,
{
    type Output = S::Item;
    fn parse(&self, stream: &mut S) -> Result<Self::Output, E> {
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
impl<C> Combinator for Chunk<C> {}
pub fn chunk<C>(chunk: C) -> Chunk<C> {
    Chunk(chunk)
}
impl<S, E, D, Err> Parser<S, E, D, Err> for Chunk<S::Slice>
where
    S: stream::Stream,
    S::Item: Eq + Hash,
    S::Slice: Clone + Eq + Hash + PartialEq + stream::SliceLen,
    E: ParseError<S::Item, S::Slice, D, Err>,
    D: Eq + Hash,
{
    type Output = S::Slice;
    fn parse(&self, stream: &mut S) -> Result<Self::Output, E> {
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

/// An `Attempt` will try to apply a parser to a strem. If it succeeds, it will
/// consume input, if it fails, it will pretend that it consumed no imput.
#[derive(Debug, Clone, Hash, Eq, Ord, PartialOrd, PartialEq)]
pub struct Attempt<P, R> {
    parser: P,
    _result: std::marker::PhantomData<*const R>,
}
impl<P, R> Combinator for Attempt<P, R> {}
pub fn attempt<P, R>(parser: P) -> Attempt<P, R> {
    Attempt {
        parser: parser,
        _result: std::marker::PhantomData,
    }
}
impl<P, R, S, E, D, Err> Parser<S, E, D, Err> for Attempt<P, R>
where
    S: stream::Stream,
    P: for<'a> Parser<stream::Conjecture<'a, S>, E, D, Err, Output = R>,
    E: ParseError<S::Item, S::Slice, D, Err>,
{
    type Output = R;
    fn parse(&self, stream: &mut S) -> Result<Self::Output, E> {
        stream.conjecture(|conj| self.parser.parse(conj))
    }

    fn eat(&self, stream: &mut S) -> Result<(), E> {
        stream.conjecture(|conj| self.parser.eat(conj))
    }
}

#[derive(Debug, Clone, Hash, Eq, Ord, PartialOrd, PartialEq)]
pub struct Optional<P>(P);
impl<P> Combinator for Optional<P> {}
impl<P, S, E, D, Err> Parser<S, E, D, Err> for Optional<P>
where
    P: Parser<S, E, D, Err>,
    S: stream::Stream,
    E: ParseError<S::Item, S::Slice, D, Err>,
{
    type Output = Option<P::Output>;
    fn parse(&self, stream: &mut S) -> Result<Self::Output, E> {
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

#[derive(Debug, Clone, Hash, Eq, Ord, PartialOrd, PartialEq)]
pub struct Void<P> {
    parser: P,
}
impl<P> Combinator for Void<P> {}
pub fn void<P>(parser: P) -> Void<P> {
    Void { parser: parser }
}
impl<P, S, E, D, Err> Parser<S, E, D, Err> for Void<P>
where
    P: Parser<S, E, D, Err>,
    S: stream::Stream,
    E: ParseError<S::Item, S::Slice, D, Err>,
{
    type Output = ();
    fn parse(&self, stream: &mut S) -> Result<(), E> {
        self.parser.eat(stream)
    }

    fn eat(&self, stream: &mut S) -> Result<(), E> {
        self.parser.eat(stream)
    }
}


#[derive(Debug, Clone, Hash, Eq, Ord, PartialOrd, PartialEq)]
pub struct Value<P, R> {
    parser: P,
    result: R,
}
impl<P, R> Combinator for Value<P, R> {}
pub fn value<P, R>(parser: P, result: R) -> Value<P, R> {
    Value {
        parser: parser,
        result: result,
    }
}
impl<P, S, R, E, D, Err> Parser<S, E, D, Err> for Value<P, R>
where
    P: Parser<S, E, D, Err>,
    R: Clone,
    S: stream::Stream,
    E: ParseError<S::Item, S::Slice, D, Err>,
{
    type Output = R;
    fn parse(&self, stream: &mut S) -> Result<Self::Output, E> {
        self.parser.eat(stream)?;
        Ok(self.result.clone())
    }

    fn eat(&self, stream: &mut S) -> Result<(), E> {
        self.parser.eat(stream)
    }
}

impl<S, R, E, D, Err> Parser<S, E, D, Err> for fn(&mut S) -> Result<R, E>
where
    S: stream::Stream,
    E: ParseError<S::Item, S::Slice, D, Err>,
{
    type Output = R;
    fn parse(&self, stream: &mut S) -> Result<Self::Output, E> {
        self(stream)
    }
    fn eat(&self, stream: &mut S) -> Result<(), E> {
        match self(stream) {
            Ok(_) => Ok(()),
            Err(e) => Err(e),
        }
    }
}

#[cfg(test)]
mod test {

    use super::error::{FullError, SourcePos};
    use super::stream::{CachedIterator, Chars, Stream};
    use super::*;
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
        let parser =
            Alt | attempt(chunk("Hello".chars().collect())) | chunk("Heffo".chars().collect());

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
        let ws_char = Alt | single(' ') | single('\t') | single('\n');

        let ws = many(ws_char).void();

        let digit = Alt
            | single('0')
            | single('1')
            | single('2')
            | single('3')
            | single('4')
            | single('5')
            | single('6')
            | single('7')
            | single('8')
            | single('9');

        let sexp = (single('('), ws.clone(), digit, ws, single(')'));

        let mut string1 = Chars::new("(     5    \t  )");
        let mut string2 = Chars::new("Heffo, World!");

        let result: Result<_, ()> = sexp.parse(&mut string1);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), ('(', (), '5', (), ')'));
        assert_eq!(string1.remove(3), vec![]);

        let result: Result<_, ()> = sexp.parse(&mut string2);
        assert!(result.is_err());
        assert_eq!(string2.remove(3), vec!['H', 'e', 'f']);
    }

    use std::collections::HashMap;
    #[derive(Debug, Clone, PartialEq)]
    enum JSON {
        JObject(HashMap<String, JSON>),
        JArray(Vec<JSON>),
        JString(String),
        JNumber(f64),
        JBool(bool),
        JNull,
    }

    fn parse_json<S, E>(stream: &mut S) -> Result<JSON, E>
    where
        S: stream::Stream<Item = char, Slice = &'static str>,
        E: error::ParseError<S::Item, S::Slice, (), ()>,
    {
        let ws = many(Alt | single(' ') | single('\n') | single('\t') | single('\r')).void();

        let null_p = chunk("null").value(JSON::JNull);
        let true_p = chunk("true").value(JSON::JBool(true));
        let false_p = chunk("false").value(JSON::JBool(false));

        let normal_char = satisfy(|c| *c >= ' ' && *c <= '\u{10FFF}' && *c != '\"' && *c != '\\');
        let unicode = single('u')
            .second(satisfy(|c: &char| c.is_ascii_hexdigit()).count(4))
            .map(|chars: Vec<char>| {
                if let &[a, b, c, d] = chars.as_slice() {
                    std::char::from_u32(
                        a.to_digit(16).expect("Hex digit") * 0x1000
                            + b.to_digit(16).expect("Hex digit") * 0x0100
                            + c.to_digit(16).expect("Hex digit") * 0x0010
                            + d.to_digit(16).expect("Hex digit") * 0x0001,
                    )
                    .expect("Valid char")
                } else {
                    unreachable!()
                }
            });
        let escape_char = one_of(&['\"', '\\', '/', 'b', 'f', 'n', 'r', 't']).map(|c| match c {
            'b' => '\x08',
            'f' => '\x0F',
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            any_c => any_c,
        });
        let escaped = single('\\').second(escape_char.alt(unicode));
        let character = Alt | normal_char | escaped;

        let raw_string = many(character)
            .between(single('"'), single('"'))
            .map(|chars: Vec<_>| chars.into_iter().collect());
        let string_p = (&raw_string).map(JSON::JString);

        let array_p = func(parse_json)
            .sep_by(single(','))
            .between(single('['), single(']'))
            .map(JSON::JArray);

        let object_key = (&raw_string).between(&ws, (&ws, single(':')));

        let object_entry = (object_key, func(parse_json));

        let object_p = object_entry
            .sep_by(single(','))
            .between(single('{'), single('}'))
            .map(|entries: Vec<(String, JSON)>| JSON::JObject(entries.into_iter().collect()));

        let value = Alt | null_p | true_p | false_p | string_p | array_p | object_p;

        value.between(&ws, &ws).parse(stream)
    }

    #[test]
    fn more_complex_grammar() {
        let result: Result<_, ()> = Ok(JSON::JBool(false));
        assert_eq!(parse_json(&mut Chars::new("false")), result);

        let result: Result<_, ()> = Ok(JSON::JBool(true));
        assert_eq!(parse_json(&mut Chars::new("true")), result);

        let result: Result<_, ()> = Ok(JSON::JNull);
        assert_eq!(parse_json(&mut Chars::new("null")), result);

        let result: Result<_, ()> = Ok(JSON::JString("Hello, World!".to_string()));
        assert_eq!(parse_json(&mut Chars::new("\"Hello, World!\"")), result);

        let result: Result<_, ()> = Ok(JSON::JArray(vec![
            JSON::JNull,
            JSON::JBool(true),
            JSON::JBool(false),
            JSON::JString("Hello".to_string()),
        ]));
        assert_eq!(
            parse_json(&mut Chars::new("[ null, true, false, \t\n \"Hello\"   ]")),
            result
        );
    }
}