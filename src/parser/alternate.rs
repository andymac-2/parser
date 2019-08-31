use super::{error, stream, Parser};

#[derive(Debug, Clone, Hash, Eq, Ord, PartialOrd, PartialEq)]
pub struct Alt;
impl<S, R, E, D, Err> Parser<S, R, E, D, Err> for Alt
where
    S: stream::Stream,
    E: error::ParseError<S::Item, S::Slice, D, Err>,
{
    fn parse(&self, _stream: &mut S) -> Result<R, E> {
        Err(E::zero())
    }
    fn eat(&self, _stream: &mut S) -> Result<(), E> {
        Err(E::zero())
    }
}
impl<P> std::ops::BitOr<P> for Alt {
    type Output = Alt1<P>;
    fn bitor(self, rhs: P) -> Self::Output {
        Alt1(rhs)
    }
}

#[derive(Debug, Clone, Hash, Eq, Ord, PartialOrd, PartialEq)]
pub struct Alt1<P>(P);
impl<P, S, R, E, D, Err> Parser<S, R, E, D, Err> for Alt1<P>
where
    P: Parser<S, R, E, D, Err>,
    S: stream::Stream,
    E: error::ParseError<S::Item, S::Slice, D, Err>,
{
    fn parse(&self, stream: &mut S) -> Result<R, E> {
        self.0.parse(stream)
    }
    fn eat(&self, stream: &mut S) -> Result<(), E> {
        self.0.eat(stream)
    }
}
impl<P0, P1> std::ops::BitOr<P1> for Alt1<P0> {
    type Output = Alt2<P0, P1>;
    fn bitor(self, rhs: P1) -> Self::Output {
        Alt2(self.0, rhs)
    }
}

#[derive(Debug, Clone, Hash, Eq, Ord, PartialOrd, PartialEq)]
pub struct Alt2<P0, P1>(P0, P1);
impl<P0, P1, S, R, E, D, Err> Parser<S, R, E, D, Err> for Alt2<P0, P1>
where
    P0: Parser<S, R, E, D, Err>,
    P1: Parser<S, R, E, D, Err>,
    S: stream::Stream,
    E: error::ParseError<S::Item, S::Slice, D, Err>,
{
    fn parse(&self, stream: &mut S) -> Result<R, E> {
        match self.0.parse(stream) {
            Ok(result) => return Ok(result),
            Err(mut e1) => match self.1.parse(stream) {
                Ok(result) => return Ok(result),
                Err(e2) => {
                    e1.concat(e2);
                    Err(e1)
                }
            }
        }
    }
    fn eat(&self, stream: &mut S) -> Result<(), E> {
        match self.0.eat(stream) {
            Ok(_) => return Ok(()),
            Err(mut e1) => match self.1.eat(stream) {
                Ok(_) => return Ok(()),
                Err(e2) => {
                    e1.concat(e2);
                    Err(e1)
                }
            }
        }
    }
}
impl<P0, P1, P2> std::ops::BitOr<P2> for Alt2<P0, P1> {
    type Output = Alt2<Self, P2>;
    fn bitor(self, rhs: P2) -> Self::Output {
        Alt2(self, rhs)
    }
}

macro_rules! impl_bitor {
    ( $alt_t:ident, $next_t:ident; $( $parser_t:ident, $parser:ident );* ) => {
        impl<$( $parser_t , )* PN> std::ops::BitOr<PN> for $alt_t<$( $parser_t , )*> {
            type Output = $next_t<$( $parser_t , )* PN>;
            fn bitor (self, rhs: PN) -> Self::Output {
                let $alt_t( $($parser, )* ) = self;
                $next_t($($parser, )* rhs)
            }
        }
    }
}

macro_rules! impl_multi {
    ( $alt_t:ident; $( $parser_t:ident, $parser:ident );* ) => {

        #[derive(Debug, Clone, Hash, Eq, Ord, PartialOrd, PartialEq)]
        pub struct $alt_t<$( $parser_t , )*>($( $parser_t , )*);

        impl < $( $parser_t , )* S, R, E, D, Err> Parser <S, R, E, D, Err> for
            $alt_t<$( $parser_t , )*>
        where
            S: stream::Stream,
            E: error::ParseError<S::Item, S::Slice, D, Err>,
            $(
                $parser_t: Parser<S, R, E, D, Err>,
            )*
        {
            fn parse(&self, stream: &mut S) -> Result<R, E> {
                let mut err = E::zero();
                let $alt_t( $(ref $parser, )* ) = self;

                $(
                    match $parser.parse(stream) {
                        Ok(result) => return Ok(result),
                        Err(e_new) => err.concat(e_new),
                    }
                )*

                Err(err)
            }

            fn eat(&self, stream: &mut S) -> Result<(), E> {
                let mut err = E::zero();
                let $alt_t( $(ref $parser, )* ) = self;

                $(
                    match $parser.eat(stream) {
                        Ok(_) => return Ok(()),
                        Err(e_new) => err.concat(e_new),
                    }
                )*

                Err(err)
            }
        }
    }
}