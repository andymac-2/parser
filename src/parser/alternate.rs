use super::{error, stream, Parser};

struct Alt();
struct Alt1<P1>(P1);
struct Alt2<P1, P2>(P1, P2);


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

macro_rules! impl_multi {
    ( $alt_t:ident; $( $parser_t:ident, $parser:ident );* ) => {
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

impl_multi!( Alt1;
    P0, p0);
impl_multi!( Alt2;
    P0, p0;
    P1, p1);