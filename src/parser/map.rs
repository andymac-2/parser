use super::{error, stream, Parser};

#[derive(Debug, Clone, Hash, Eq, Ord, PartialOrd, PartialEq)]
pub struct Map<P, F, R> {
    parser: P,
    func: F,
    _parser_result: std::marker::PhantomData<*const R>,
}
pub fn map<P, F, R>(parser: P, func: F) -> Map<P, F, R> {
    Map {
        parser: parser,
        func: func,
        _parser_result: std::marker::PhantomData,
    }
}
impl<P, F, Rin, Rout, S, E, D, Err> Parser<S, Rout, E, D, Err> for Map<P, F, Rin>
where
    P: Parser<S, Rin, E, D, Err>,
    F: Fn(Rin) -> Rout,
    S: stream::Stream,
    E: error::ParseError<S::Item, S::Slice, D, Err>,
{
    fn parse(&self, stream: &mut S) -> Result<Rout, E> {
        self.parser.parse(stream).map(&self.func)
    }
    fn eat(&self, stream: &mut S) -> Result<(), E> {
        self.parser.eat(stream)
    }
}