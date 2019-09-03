use super::{error, stream, Combinator, Parser};

#[derive(Debug, Clone, Hash, Eq, Ord, PartialOrd, PartialEq)]
pub struct Many0<P>(P);
impl<P> Combinator for Many0<P> {}
pub fn many0<P: Combinator>(parser: P) -> Many0<P> {
    Many0(parser)
}
pub fn many<P: Combinator>(parser: P) -> Many0<P> {
    Many0(parser)
}
impl<P, S, E, D, Err> Parser<S, E, D, Err> for Many0<P>
where
    P: Parser<S, E, D, Err>,
    S: stream::Stream,
    E: error::ParseError<S::Item, S::Slice, D, Err>,
{
    type Output = Vec<P::Output>;
    fn parse(&self, stream: &mut S) -> Result<Self::Output, E> {
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
            match self.0.eat(stream) {
                Ok(_) => (),
                Err(_) => return Ok(()),
            };
        }
    }
}

#[derive(Debug, Clone, Hash, Eq, Ord, PartialOrd, PartialEq)]
pub struct Many1<P>(P);
pub fn many1<P: Combinator>(parser: P) -> Many1<P> {
    Many1(parser)
}
impl<P> Combinator for Many1<P> {}
impl<P, S, E, D, Err> Parser<S, E, D, Err> for Many1<P>
where
    P: Parser<S, E, D, Err>,
    S: stream::Stream,
    E: error::ParseError<S::Item, S::Slice, D, Err>,
{
    type Output = Vec<P::Output>;
    fn parse(&self, stream: &mut S) -> Result<Self::Output, E> {
        let mut results = vec![self.0.parse(stream)?];
        loop {
            match self.0.parse(stream) {
                Ok(result) => results.push(result),
                Err(_) => return Ok(results),
            };
        }
    }

    fn eat(&self, stream: &mut S) -> Result<(), E> {
        self.0.eat(stream)?;
        loop {
            match self.0.parse(stream) {
                Ok(_) => (),
                Err(_) => return Ok(()),
            };
        }
    }
}

#[derive(Debug, Clone, Hash, Eq, Ord, PartialOrd, PartialEq)]
pub struct SepBy<P0, P1> {
    parser: P0,
    delimiter: P1,
}
impl<P0, P1> Combinator for SepBy<P0, P1> {}
pub fn sep_by<P0: Combinator, P1: Combinator>(parser: P0, delimiter: P1) -> SepBy<P0, P1> {
    SepBy {
        parser: parser,
        delimiter: delimiter,
    }
}
impl<P0, P1, S, E, D, Err> Parser<S, E, D, Err> for SepBy<P0, P1>
where
    P0: Parser<S, E, D, Err>,
    P1: Parser<S, E, D, Err>,
    S: stream::Stream,
    E: error::ParseError<S::Item, S::Slice, D, Err>,
{
    type Output = Vec<P0::Output>;
    fn parse(&self, stream: &mut S) -> Result<Self::Output, E> {
        let mut results = Vec::new();
        loop {
            match self.parser.parse(stream) {
                Ok(result) => results.push(result),
                Err(_) => return Ok(results),
            };

            match self.delimiter.eat(stream) {
                Ok(_) => (),
                Err(_) => return Ok(results),
            };
        }
    }

    fn eat(&self, stream: &mut S) -> Result<(), E> {
        loop {
            match self.parser.eat(stream) {
                Ok(_) => (),
                Err(_) => return Ok(()),
            };

            match self.delimiter.eat(stream) {
                Ok(_result) => (),
                Err(_) => return Ok(()),
            };
        }
    }
}

#[derive(Debug, Clone, Hash, Eq, Ord, PartialOrd, PartialEq)]
pub struct Count<P> {
    count: usize,
    parser: P,
}
pub fn count<P>(parser: P, num: usize) -> Count<P> {
    Count {
        count: num,
        parser: parser,
    }
}
impl<P> Combinator for Count<P> {}
impl<P, S, E, D, Err> Parser<S, E, D, Err> for Count<P>
where
    P: Parser<S, E, D, Err>,
    S: stream::Stream,
    E: error::ParseError<S::Item, S::Slice, D, Err>,
{
    type Output = Vec<P::Output>;
    fn parse(&self, stream: &mut S) -> Result<Self::Output, E> {
        let mut results = Vec::new();
        for _ in 0..self.count {
            results.push(self.parser.parse(stream)?);
        }
        Ok(results)
    }

    fn eat(&self, stream: &mut S) -> Result<(), E> {
        for _ in 0..self.count {
            self.parser.parse(stream)?;
        }
        Ok(())
    }
}