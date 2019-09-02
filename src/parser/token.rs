use super::{error, stream, Parser, Combinator};

use std::collections::HashSet;
use std::hash;

#[derive(Debug, Clone)]
pub struct Token<F, T, C, D> 
where
    T: Eq + hash::Hash,
    C: Eq + hash::Hash,
    D: Eq + hash::Hash,
    F: Fn(&T) -> bool,
{
    func: F,
    expected: HashSet<error::ErrItem<T, C, D>>
}
impl<F, T, C, D> Combinator for Token<F, T, C, D> 
where
    T: Eq + hash::Hash,
    C: Eq + hash::Hash,
    D: Eq + hash::Hash,
    F: Fn(&T) -> bool,
{}
pub fn token<F, T, C, D>(expected: HashSet<error::ErrItem<T, C, D>>, func: F) -> Token<F, T, C, D> 
where
    T: Eq + hash::Hash,
    C: Eq + hash::Hash,
    D: Eq + hash::Hash,
    F: Fn(&T) -> bool
{
    Token {
        func: func,
        expected: expected,
    }
}
impl<F, S, E, D, Err> Parser<S, E, D, Err> for Token<F, S::Item, S::Slice, D>
where
    D: Eq + hash::Hash + Clone,
    F: Fn(&S::Item) -> bool,
    S: stream::Stream,
    S::Item: Eq + hash::Hash + Clone,
    S::Slice: Eq + hash::Hash + Clone,
    E: error::ParseError<S::Item, S::Slice, D, Err>,
{
    type Output = S::Item;
    fn parse(&self, stream: &mut S) -> Result<Self::Output, E> {
        if let Some(token) = stream.peek() {
            if (self.func)(&token) {
                stream.advance();
                Ok(token.clone())
            } else {
                let pos = stream.get_position();
                let unexpected = Some(error::ErrItem::Token(token.clone()));
                let expected = self.expected.clone();
                Err(E::simple_error(pos, unexpected, expected))
            }
        } else {
            let pos = stream.get_position();
            let unexpected = Some(error::ErrItem::Eof);
            let expected = self.expected.clone();
            Err(E::simple_error(pos, unexpected, expected))
        }
    }
    fn eat(&self, stream: &mut S) -> Result<(), E> {
        if let Some(token) = stream.peek() {
            if (self.func)(&token) {
                stream.advance();
                Ok(())
            } else {
                let pos = stream.get_position();
                let unexpected = Some(error::ErrItem::Token(token.clone()));
                let expected = self.expected.clone();
                Err(E::simple_error(pos, unexpected, expected))
            }
        } else {
            let pos = stream.get_position();
            let unexpected = Some(error::ErrItem::Eof);
            let expected = self.expected.clone();
            Err(E::simple_error(pos, unexpected, expected))
        }
    }
}