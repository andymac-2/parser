use super::{error, stream, Combinator, Parser};

use std::collections::HashSet;
use std::hash;

#[derive(Debug, Clone)]
pub struct OneOf<'a, T>
where
    T: Eq + hash::Hash,
{
    expected: &'a [T],
}
impl<'a, T> Combinator for OneOf<'a, T> where T: Eq + hash::Hash {}
impl<'a, T> OneOf<'a, T>
where

    T: Eq + hash::Hash + Clone,
{
    fn expected_values<C, D>(&self) -> HashSet<error::ErrItem<T, C, D>>
    where
        C: Eq + hash::Hash,
        D: Eq + hash::Hash,
    {
        self.expected
            .iter()
            .map(|c| error::ErrItem::Token(c.clone()))
            .collect()
    }
}
pub fn one_of<'a, T>(expected: &'a [T]) -> OneOf<'a, T>
where
    T: Eq + hash::Hash,
{
    OneOf { expected: expected }
}
impl<'a, S, E, D, Err> Parser<S, E, D, Err> for OneOf<'a, S::Item>
where
    S: stream::Stream,
    S::Item: Eq + hash::Hash + Clone,
    S::Slice: Eq + hash::Hash + Clone,
    E: error::ParseError<S::Item, S::Slice, D, Err>,
    D: Eq + hash::Hash,
{
    type Output = S::Item;
    fn parse(&self, stream: &mut S) -> Result<Self::Output, E> {
        if let Some(token) = stream.peek() {
            if self.expected.contains(&token) {
                stream.advance();
                Ok(token.clone())
            } else {
                let pos = stream.get_position();
                let unexpected = Some(error::ErrItem::Token(token.clone()));
                let expected = self.expected_values();
                Err(E::simple_error(pos, unexpected, expected))
            }
        } else {
            let pos = stream.get_position();
            let unexpected = Some(error::ErrItem::Eof);
            let expected = self.expected_values();
            Err(E::simple_error(pos, unexpected, expected))
        }
    }
    fn eat(&self, stream: &mut S) -> Result<(), E> {
        if let Some(token) = stream.peek() {
            if self.expected.contains(&token) {
                stream.advance();
                Ok(())
            } else {
                let pos = stream.get_position();
                let unexpected = Some(error::ErrItem::Token(token.clone()));
                let expected = self.expected_values();
                Err(E::simple_error(pos, unexpected, expected))
            }
        } else {
            let pos = stream.get_position();
            let unexpected = Some(error::ErrItem::Eof);
            let expected = self.expected_values();
            Err(E::simple_error(pos, unexpected, expected))
        }
    }
}