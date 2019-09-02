use super::{Combinator, Parser, stream, error};

#[derive(Debug, Clone, Hash, Eq, Ord, PartialOrd, PartialEq)]
pub struct First<P0, P1> {
    first: P0,
    second: P1,
}
impl<P0, P1> Combinator for First<P0, P1> {}
pub fn first<P0, P1>(first: P0, second: P1) -> First<P0, P1> {
    First {
        first: first,
        second: second,
    }
}
impl<P0, P1, S, E, D, Err> Parser<S, E, D, Err> for First<P0, P1>
where
    P0: Parser<S, E, D, Err>,
    P1: Parser<S, E, D, Err>,
    S: stream::Stream,
    E: error::ParseError<S::Item, S::Slice, D, Err>,
{
    type Output = P0::Output;
    fn parse(&self, stream: &mut S) -> Result<Self::Output, E> {
        let result = self.first.parse(stream)?;
        self.second.eat(stream)?;
        Ok(result)
    }

    fn eat(&self, stream: &mut S) -> Result<(), E> {
        self.first.eat(stream)?;
        self.second.eat(stream)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Hash, Eq, Ord, PartialOrd, PartialEq)]
pub struct Second<P0, P1> {
    first: P0,
    second: P1,
}
impl<P0, P1> Combinator for Second<P0, P1> {}
pub fn second<P0, P1>(first: P0, second: P1) -> Second<P0, P1> {
    Second {
        first: first,
        second: second,
    }
}
impl<P0, P1, S, E, D, Err> Parser<S, E, D, Err> for Second<P0, P1>
where
    P0: Parser<S, E, D, Err>,
    P1: Parser<S, E, D, Err>,
    S: stream::Stream,
    E: error::ParseError<S::Item, S::Slice, D, Err>,
{
    type Output = P1::Output;
    fn parse(&self, stream: &mut S) -> Result<Self::Output, E> {
        self.first.eat(stream)?;
        self.second.parse(stream)
    }

    fn eat(&self, stream: &mut S) -> Result<(), E> {
        self.first.eat(stream)?;
        self.second.eat(stream)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Hash, Eq, Ord, PartialOrd, PartialEq)]
pub struct Between<L, P, R> {
    parser: P,
    left_delim: L,
    right_delim: R,
}
impl<L, P, R> Combinator for Between<L, P, R> {}
pub fn between<L, P, R>(parser: P, left: L, right: R) -> Between<L, P, R> {
    Between {
        parser: parser,
        left_delim: left,
        right_delim: right,
    }
}
impl<L, P, R, S, E, D, Err> Parser<S, E, D, Err> for Between<L, P, R>
where
    P: Parser<S, E, D, Err>,
    L: Parser<S, E, D, Err>,
    R: Parser<S, E, D, Err>,
    S: stream::Stream,
    E: error::ParseError<S::Item, S::Slice, D, Err>,
{
    type Output = P::Output;
    fn parse(&self, stream: &mut S) -> Result<Self::Output, E> {
        self.left_delim.eat(stream)?;
        let result = self.parser.parse(stream)?;
        self.right_delim.eat(stream)?;
        Ok(result)
    }

    fn eat(&self, stream: &mut S) -> Result<(), E> {
        self.left_delim.eat(stream)?;
        self.parser.eat(stream)?;
        self.right_delim.eat(stream)?;
        Ok(())
    }
}
