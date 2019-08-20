#[feature(trace_macros)]
trace_macros!(true);

use super::{error, stream, Parser};

macro_rules! parse_multi {
    ( $stream:ident; $($parser:ident $result:ident),* ) => {
        let ( $(ref $parser, )* ) = self;
        $(
            let $result = $parser.parse($stream)?;
        )*
        Ok(( $($result, )* ))
    }
}

macro_rules! eat_multi {
    ( $stream:ident; $($result:ident),* ) => {
        let ( $(ref $parser, )* ) = self;
        $(
            let $result = $parser.eat($stream)?;
        )*
        Ok(())
    }
}

macro_rules! impl_multi {
    ( $stream:ident: $($parser_t:ty, $result_t:ty, $parser:ident, $result:ident);* ) => {
        impl < $($parser_t, )* $($result_t, )* S, E, D, Err>
            Parser <S, ( $($result_t, )* ), e, D, Err>
            for ( $($parser_t, )* )
        where
            S: stream::Stream,
            E: error::ParseError<S::Item, S::Slice, D, Err>,
            $(
                $parser_t: Parser<S, $result_t, E, D, R>,
            )*
        {
            fn parse(&self, stream: &mut S) -> Result<R1, E> {
                parse_multi!( S; $($parser $result),* )
            }
            fn eat(&self, stream: &mut S) -> Result<(), E> {
                eat_multi!( S; $($parser $result),* )
            }
        }
    }
}

impl<S, E, D, Err> Parser<S, (), E, D, Err> for ()
where
    S: stream::Stream,
    E: error::ParseError<S::Item, S::Slice, D, Err>,
{
    fn parse(&self, _stream: &mut S) -> Result<(), E> {
        Ok(())
    }
    fn eat(&self, _stream: &mut S) -> Result<(), E> {
        Ok(())
    }
}
impl<S, R1, E, D, Err, P1> Parser<S, (R1), E, D, Err> for (P1,)
where
    S: stream::Stream,
    E: error::ParseError<S::Item, S::Slice, D, Err>,
    P1: Parser<S, R1, E, D, Err>,
{
    fn parse(&self, stream: &mut S) -> Result<R1, E> {
        self.0.parse(stream)
    }
    fn eat(&self, stream: &mut S) -> Result<(), E> {
        self.0.eat(stream)
    }
}
impl<S, R0, R1, E, D, Err, P0, P1> Parser<S, (R0, R1), E, D, Err> for (P0, P1)
where
    S: stream::Stream,
    E: error::ParseError<S::Item, S::Slice, D, Err>,
    P0: Parser<S, R0, E, D, Err>,
    P1: Parser<S, R1, E, D, Err>,
{
    fn parse(&self, stream: &mut S) -> Result<(R0, R1), E> {
        let (ref p0, ref p1) = self;
        let r0 = p0.parse(stream)?;
        let r1 = p1.parse(stream)?;
        Ok((r0, r1))
    }
    fn eat(&self, stream: &mut S) -> Result<(), E> {
        self.0.eat(stream)?;
        self.1.eat(stream)?;
        Ok(())
    }
}