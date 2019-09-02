use super::{error, stream, Parser, Combinator};

#[derive(Debug, Clone, Hash, Eq, Ord, PartialOrd, PartialEq)]
pub struct Map<P, F, R> {
    parser: P,
    func: F,
    _result: std::marker::PhantomData<*const R>,
}
impl<P, F, R> Combinator for Map<P, F, R> {}
pub fn map<P, F, R>(parser: P, func: F) -> Map<P, F, R> {
    Map {
        parser: parser,
        func: func,
        _result: std::marker::PhantomData,
    }
}
impl<P, F, R, S, E, D, Err> Parser<S, E, D, Err> for Map<P, F, R>
where
    P: Parser<S, E, D, Err>,
    F: Fn(P::Output) -> R,
    S: stream::Stream,
    E: error::ParseError<S::Item, S::Slice, D, Err>,
{
    type Output = R;
    fn parse(&self, stream: &mut S) -> Result<Self::Output, E> {
        self.parser.parse(stream).map(&self.func)
    }
    fn eat(&self, stream: &mut S) -> Result<(), E> {
        self.parser.eat(stream)
    }
}