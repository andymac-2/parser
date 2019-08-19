use std::collections::VecDeque;

use super::parser::SourcePos;

type SResult<R = (), E = ()> = Result<R, E>;

pub trait SliceLen {
    fn length(&self) -> usize;
}
impl<T> SliceLen for Vec<T> {
    fn length(&self) -> usize {
        self.len()
    }
}
impl<T> SliceLen for &[T] {
    fn length(&self) -> usize {
        self.len()
    }
}
impl SliceLen for &str {
    fn length(&self) -> usize {
        self.chars().count()
    }
}
impl<T> SliceLen for std::iter::Empty<T> {
    fn length(&self) -> usize {
        0
    }
}

/// The trait `Stream` is a series of `Item`s that allows us to look ahead
/// without actually consuming any input.
///
/// Note that, while we can look ahead in the stream without consuming any
/// input, we can't actually rewind the stream.
pub trait Stream: Iterator {
    type Slice: SliceLen;

    fn get_position(&self) -> SourcePos;

    fn index(&mut self, n: usize) -> Option<Self::Item>;

    /// Obtain a the next element of the struct without actually consuming it.
    /// It is expected that every time you call this function, it will return
    /// the same item
    fn peek(&mut self) -> Option<Self::Item> {
        self.index(0)
    }

    fn view(&mut self, lower: usize, upper: usize) -> Option<Self::Slice>;

    /// Try to look ahead `n` elements without consuming any input. The exact
    /// type of the iterator depends on the implementation.
    fn lookahead(&mut self, n: usize) -> Option<Self::Slice> {
        self.view(0, n)
    }

    fn conjecture<F, R, E>(&mut self, func: F) -> SResult<R, E>
    where
        Self: Sized,
        F: for<'a> FnOnce(&mut Conjecture<'a, Self>) -> SResult<R, E>,
    {
        let mut conjecture = Conjecture::new(self);
        let result = func(&mut conjecture);

        if result.is_ok() {
            conjecture.apply()
        }

        result
    }


    /// Try to cache `n` elements in memory. For structures wehre more than `n`
    /// elements are already completely generated in memory, this should be a
    /// no-op. For iterators or other generators, it is expected that `cache`
    /// will produce the values and store them in memory.
    ///
    /// The default implementation for `cache` is a no-op.
    fn cache(&mut self, _n: usize) {}

    /// Advance one token in the stream without returning it.
    fn advance(&mut self) {
        self.next();
    }

    /// Advance n tokens in the stream and consume nothing.
    fn seek(&mut self, n: usize) {
        for _ in 0..n {
            self.advance();
        }
    }

    /// Try take n `Item`s from the iterator, and return a vector of the
    /// results. If the end of the stream is reached, it returns all of the
    /// remaining `Item`s.
    fn remove(&mut self, n: usize) -> Vec<Self::Item> {
        let mut results = Vec::with_capacity(n);
        for _ in 0..n {
            match self.next() {
                Some(elem) => results.push(elem),
                None => return results,
            }
        }
        results
    }

    fn default_next(&mut self) -> Option<Self::Item> {
        let result = self.peek();
        self.advance();
        result
    }
}


#[derive(Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Conjecture<'c, S> {
    position: usize,
    underlying: &'c mut S,
}
impl<'c, S> Conjecture<'c, S> {
    fn new(underlying: &'c mut S) -> Self {
        Conjecture {
            position: 0,
            underlying: underlying,
        }
    }
}
impl<'c, S> Conjecture<'c, S>
where
    S: Stream,
{
    fn apply(self) {
        self.underlying.seek(self.position)
    }
}
impl<'c, S> Iterator for Conjecture<'c, S>
where
    S: Stream,
{
    type Item = S::Item;

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.peek();
        if result.is_some() {
            self.position += 1;
        }
        result
    }
}
impl<'c, S> Stream for Conjecture<'c, S>
where
    S: Stream,
{
    type Slice = S::Slice;
    fn get_position(&self) -> SourcePos {
        SourcePos(self.position + self.underlying.get_position().0)
    }
    fn index(&mut self, n: usize) -> Option<Self::Item> {
        self.underlying.index(self.position + n)
    }
    fn view(&mut self, lower: usize, upper: usize) -> Option<Self::Slice> {
        let pos = self.position;
        self.underlying.view(lower + pos, upper + pos)
    }
    fn cache(&mut self, n: usize) {
        self.underlying.cache(n + self.position)
    }
    fn advance(&mut self) {
        self.position += 1;
    }
    fn seek(&mut self, n: usize) {
        self.position += n;
    }
}

/// A default implementation to derive a `Stream` from any `Iterator`. This can
/// be a default implementation for a `Stream` if you like, however, there may be
/// more efficient implementations for some types.
#[derive(Debug, Clone)]
pub struct CachedIterator<I>
where
    I: Iterator,
{
    iterator: I,
    cache: VecDeque<I::Item>,
    position: usize,
}
impl<I> CachedIterator<I>
where
    I: Iterator,
{
    pub fn new(iterator: I) -> Self {
        CachedIterator {
            iterator: iterator,
            cache: VecDeque::new(),
            position: 0,
        }
    }

    /// Cache an additional number of elements from the internal iterator. The
    /// stream acts as if no elements have been consumed.
    fn cache_additional(&mut self, additional: usize) {
        self.cache.reserve(additional);
        for _ in 0..additional {
            match self.iterator.next() {
                Some(elem) => self.cache.push_back(elem),
                None => return,
            }
        }
    }
}
impl<I> From<I> for CachedIterator<I>
where
    I: Iterator,
{
    fn from(iterator: I) -> Self {
        CachedIterator::new(iterator)
    }
}
impl<I> Iterator for CachedIterator<I>
where
    I: Iterator,
{
    type Item = I::Item;
    fn next(&mut self) -> Option<Self::Item> {
        match self.cache.pop_front() {
            Some(elem) => {
                self.position += 1;
                Some(elem)
            }
            None => {
                let result = self.iterator.next();
                if result.is_some() {
                    self.position += 1;
                }
                result
            }
        }
    }
}
impl<I> Stream for CachedIterator<I>
where
    I: Iterator,
    I::Item: Clone,
{
    type Slice = Vec<Self::Item>;
    fn get_position(&self) -> SourcePos {
        SourcePos(self.position)
    }
    fn index(&mut self, n: usize) -> Option<Self::Item> {
        self.cache(n + 1);
        self.cache.get(n).cloned()
    }
    fn view<'a>(&mut self, lower: usize, upper: usize) -> Option<Self::Slice> {
        self.cache(upper);
        let mut view = Vec::with_capacity(upper - lower);
        for i in lower..upper {
            match self.cache.get(i) {
                Some(elem) => view.push(elem.clone()),
                None => return None,
            }
        }
        Some(view)
    }
    fn cache(&mut self, n: usize) {
        let length = self.cache.len();
        if n > length {
            self.cache_additional(n - length);
        }
    }
}

pub struct Slice<'a, T> {
    position: usize,
    slice: std::slice::Iter<'a, T>,
}
impl<'a, T> Slice<'a, T> {
    pub fn new(slice: &'a [T]) -> Self {
        Slice {
            position: 0,
            slice: slice.iter(),
        }
    }
}
impl<'a, T> Iterator for Slice<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        let result = self.slice.next();
        if result.is_some() {
            self.position += 1;
        }
        result
    }
}
impl<'a, T> Stream for Slice<'a, T> {
    type Slice = &'a [T];
    fn get_position(&self) -> SourcePos {
        SourcePos(self.position)
    }
    fn index(&mut self, n: usize) -> Option<Self::Item> {
        self.slice.as_slice().get(n)
    }
    fn peek(&mut self) -> Option<Self::Item> {
        self.slice.as_slice().first()
    }
    fn view(&mut self, lower: usize, upper: usize) -> Option<Self::Slice> {
        self.slice.as_slice().get(lower..upper)
    }
    fn seek(&mut self, n: usize) {
        if n >= 1 {
            self.position += n;
            self.slice.nth(n - 1);
        }
    }
}
pub struct Chars<'a> {
    position: usize,
    string: std::str::Chars<'a>,
}
impl<'a> Chars<'a> {
    pub fn new(string: &'a str) -> Self {
        Chars {
            position: 0,
            string: string.chars(),
        }
    }
}
impl<'a> Iterator for Chars<'a> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        let result = self.string.next();
        if result.is_some() {
            self.position += 1;
        }
        result
    }
}
impl<'a> Stream for Chars<'a> {
    type Slice = &'a str;
    fn get_position(&self) -> SourcePos {
        SourcePos(self.position)
    }
    fn index(&mut self, n: usize) -> Option<Self::Item> {
        self.string.as_str().chars().nth(n)
    }
    fn peek(&mut self) -> Option<Self::Item> {
        self.string.as_str().chars().next()
    }
    fn view(&mut self, lower: usize, upper: usize) -> Option<Self::Slice> {
        if upper == lower {
            return Some("");
        }
        let string = self.string.as_str();
        let mut indices = string.char_indices();
        let lower_byte = indices.nth(lower);
        let upper_byte = indices.nth(upper - lower - 1);

        match (lower_byte, upper_byte) {
            (Some(lb), Some(ub)) => Some(&string[lb.0..ub.0]),
            _ => None,
        }
    }
    fn lookahead(&mut self, n: usize) -> Option<Self::Slice> {
        let string = self.string.as_str();
        let index = string.char_indices().nth(n);

        match index {
            Some(ix) => Some(&string[..ix.0]),
            _ => None,
        }
    }
    fn seek(&mut self, n: usize) {
        if n >= 1 {
            self.position += n;
            self.string.nth(n - 1);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::SourcePos;
    use std::iter;

    /// This is a test to see if we can derive an `Iterator` instance using only
    /// Stream methods. If it compiles, it succeeds.
    #[derive(Clone)]
    struct TestStream();
    impl Iterator for TestStream {
        type Item = ();
        fn next(&mut self) -> Option<Self::Item> {
            self.default_next()
        }
    }
    impl Stream for TestStream {
        type Slice = iter::Empty<()>;
        fn get_position(&self) -> SourcePos {
            SourcePos(0)
        }
        fn index(&mut self, _n: usize) -> Option<Self::Item> {
            None
        }
        fn view<'a>(&mut self, _lower: usize, _upper: usize) -> Option<Self::Slice> {
            Some(iter::empty())
        }
    }

    #[test]
    fn iterator_stream_takes() {
        let mut stream = CachedIterator::new(0..10);

        assert_eq!(stream.remove(5), vec![0, 1, 2, 3, 4]);
        assert_eq!(stream.lookahead(5), Some(vec![5, 6, 7, 8, 9]));
        assert_eq!(stream.remove(5), vec![5, 6, 7, 8, 9]);
        assert_eq!(stream.lookahead(5), None);
        assert_eq!(stream.remove(2), vec![]);
    }

    #[test]
    fn zero_slice_has_zero_length() {
        let mut stream = Chars::new("Hello, World!");
        let zero_slice = stream.view(5, 5);
        assert_eq!(zero_slice, Some(""));
        assert_eq!(zero_slice.unwrap().length(), 0);

        let mut stream = Slice::new(&[1, 2, 3, 4, 5, 6, 7]);
        let zero_slice = stream.view(5, 5);
        assert_eq!(zero_slice, Some(&[][..]));
        assert_eq!(zero_slice.unwrap().length(), 0);
    }

    #[test]
    fn one_slice_has_length_one() {
        let mut stream = Chars::new("Hello, World!");
        let zero_slice = stream.view(4, 5);
        assert_eq!(zero_slice, Some("o"));
        assert_eq!(zero_slice.unwrap().length(), 1);

        let mut stream = Slice::new(&[1, 2, 3, 4, 5, 6, 7]);
        let zero_slice = stream.view(4, 5);
        assert_eq!(zero_slice, Some(&[5][..]));
        assert_eq!(zero_slice.unwrap().length(), 1);
    }

    #[test]
    fn char_stream() {
        let mut stream = Chars::new("Hello, World!");

        let slice = stream.view(3, 8);
        assert_eq!(slice, Some("lo, W"));
        assert_eq!(slice.unwrap().length(), 5);
        assert_eq!(stream.get_position(), SourcePos(0));
        assert_eq!(stream.peek(), Some('H'));
        assert_eq!(stream.lookahead(6), Some("Hello,"));

        stream.advance();
        assert_eq!(stream.get_position(), SourcePos(1));
        assert_eq!(stream.peek(), Some('e'));

        stream.seek(4);
        assert_eq!(stream.get_position(), SourcePos(5));
        assert_eq!(stream.peek(), Some(','));
    }

    #[test]
    fn slice_stream() {
        let mut stream = Slice::new(&[1, 2, 3, 4, 5, 6, 7, 8, 9, 0]);

        let slice = stream.view(3, 8);
        assert_eq!(slice, Some(&[4, 5, 6, 7, 8][..]));
        assert_eq!(slice.unwrap().length(), 5);
        assert_eq!(stream.get_position(), SourcePos(0));
        assert_eq!(stream.peek(), Some(&1));
        assert_eq!(stream.lookahead(6), Some(&[1, 2, 3, 4, 5, 6][..]));

        stream.advance();
        assert_eq!(stream.get_position(), SourcePos(1));
        assert_eq!(stream.peek(), Some(&2));

        stream.seek(4);
        assert_eq!(stream.get_position(), SourcePos(5));
        assert_eq!(stream.peek(), Some(&6));
    }

    #[test]
    fn iterator_stream_finishes() {
        let mut stream = CachedIterator::new(0..10);

        assert_eq!(stream.remove(10), vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
        assert_eq!(stream.lookahead(5), None);
        assert_eq!(stream.remove(2), vec![]);
    }

    #[test]
    fn stream_from_iterator() {
        let mut stream = CachedIterator::from(0..10);
        assert_eq!(stream.lookahead(3), Some(vec![0, 1, 2]));
        assert_eq!(stream.remove(1), vec![0]);
        assert_eq!(stream.lookahead(4), Some(vec![1, 2, 3, 4]));
        assert_eq!(stream.remove(4), vec![1, 2, 3, 4]);
    }

    #[test]
    fn iterator_into_stream() {
        let mut stream: CachedIterator<_> = (0..10).into();
        assert_eq!(stream.lookahead(3), Some(vec![0, 1, 2]));
        assert_eq!(stream.remove(1), vec![0]);
        assert_eq!(stream.lookahead(4), Some(vec![1, 2, 3, 4]));
        assert_eq!(stream.remove(4), vec![1, 2, 3, 4]);
    }

    #[test]
    fn conjecture_applies_on_success() {
        let mut stream: CachedIterator<_> = (0..10).into();
        assert_eq!(stream.lookahead(3), Some(vec![0, 1, 2]));

        let result = stream.conjecture::<_, (), ()>(|stream| {
            assert_eq!(stream.lookahead(3), Some(vec![0, 1, 2]));
            assert_eq!(stream.remove(1), vec![0]);
            assert_eq!(stream.lookahead(3), Some(vec![1, 2, 3]));

            Ok(())
        });

        assert_eq!(result, Ok(()));
        assert_eq!(stream.lookahead(3), Some(vec![1, 2, 3]));
    }

    #[test]
    fn conjecture_dissolves_on_failure() {
        let mut stream: CachedIterator<_> = (0..10).into();
        assert_eq!(stream.lookahead(3), Some(vec![0, 1, 2]));

        let result = stream.conjecture::<_, (), ()>(|stream| {
            assert_eq!(stream.lookahead(3), Some(vec![0, 1, 2]));
            assert_eq!(stream.remove(1), vec![0]);
            assert_eq!(stream.lookahead(3), Some(vec![1, 2, 3]));

            Err(())
        });

        assert_eq!(result, Err(()));
        assert_eq!(stream.lookahead(3), Some(vec![0, 1, 2]));
    }
}
