use super::{error, stream, Parser};

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

impl_multi!( Alt1;
    P0, p0);
impl_bitor!( Alt1, Alt2;
    P0, p0);
impl_multi!( Alt2;
    P0, p0;
    P1, p1);
impl_bitor!( Alt2, Alt3;
    P0, p0;
    P1, p1);
impl_multi!( Alt3;
    P0, p0;
    P1, p1;
    P2, p2);
impl_bitor!( Alt3, Alt4;
    P0, p0;
    P1, p1;
    P2, p2);
impl_multi!( Alt4;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3);
impl_bitor!( Alt4, Alt5;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3);
impl_multi!( Alt5;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3;
    P4, p4);
impl_bitor!( Alt5, Alt6;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3;
    P4, p4);
impl_multi!( Alt6;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3;
    P4, p4;
    P5, p5);
impl_bitor!( Alt6, Alt7;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3;
    P4, p4;
    P5, p5);
impl_multi!( Alt7;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3;
    P4, p4;
    P5, p5;
    P6, p6);
impl_bitor!( Alt7, Alt8;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3;
    P4, p4;
    P5, p5;
    P6, p6);
impl_multi!( Alt8;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3;
    P4, p4;
    P5, p5;
    P6, p6;
    P7, p7);
impl_bitor!( Alt8, Alt9;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3;
    P4, p4;
    P5, p5;
    P6, p6;
    P7, p7);
impl_multi!( Alt9;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3;
    P4, p4;
    P5, p5;
    P6, p6;
    P7, p7;
    P8, p8);
impl_bitor!( Alt9, Alt10;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3;
    P4, p4;
    P5, p5;
    P6, p6;
    P7, p7;
    P8, p8);
impl_multi!( Alt10;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3;
    P4, p4;
    P5, p5;
    P6, p6;
    P7, p7;
    P8, p8;
    P9, p9);
impl_bitor!( Alt10, Alt11;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3;
    P4, p4;
    P5, p5;
    P6, p6;
    P7, p7;
    P8, p8;
    P9, p9);
impl_multi!( Alt11;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3;
    P4, p4;
    P5, p5;
    P6, p6;
    P7, p7;
    P8, p8;
    P9, p9;
    P10, p10);
impl_bitor!( Alt11, Alt12;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3;
    P4, p4;
    P5, p5;
    P6, p6;
    P7, p7;
    P8, p8;
    P9, p9;
    P10, p10);
impl_multi!( Alt12;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3;
    P4, p4;
    P5, p5;
    P6, p6;
    P7, p7;
    P8, p8;
    P9, p9;
    P10, p10;
    P11, p11);
impl_bitor!( Alt12, Alt13;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3;
    P4, p4;
    P5, p5;
    P6, p6;
    P7, p7;
    P8, p8;
    P9, p9;
    P10, p10;
    P11, p11);
impl_multi!( Alt13;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3;
    P4, p4;
    P5, p5;
    P6, p6;
    P7, p7;
    P8, p8;
    P9, p9;
    P10, p10;
    P11, p11;
    P12, p12);
impl_bitor!( Alt13, Alt14;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3;
    P4, p4;
    P5, p5;
    P6, p6;
    P7, p7;
    P8, p8;
    P9, p9;
    P10, p10;
    P11, p11;
    P12, p12);
impl_multi!( Alt14;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3;
    P4, p4;
    P5, p5;
    P6, p6;
    P7, p7;
    P8, p8;
    P9, p9;
    P10, p10;
    P11, p11;
    P12, p12;
    P13, p13);
impl_bitor!( Alt14, Alt15;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3;
    P4, p4;
    P5, p5;
    P6, p6;
    P7, p7;
    P8, p8;
    P9, p9;
    P10, p10;
    P11, p11;
    P12, p12;
    P13, p13);
impl_multi!( Alt15;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3;
    P4, p4;
    P5, p5;
    P6, p6;
    P7, p7;
    P8, p8;
    P9, p9;
    P10, p10;
    P11, p11;
    P12, p12;
    P13, p13;
    P14, p14);
impl_bitor!( Alt15, Alt16;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3;
    P4, p4;
    P5, p5;
    P6, p6;
    P7, p7;
    P8, p8;
    P9, p9;
    P10, p10;
    P11, p11;
    P12, p12;
    P13, p13;
    P14, p14);
impl_multi!( Alt16;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3;
    P4, p4;
    P5, p5;
    P6, p6;
    P7, p7;
    P8, p8;
    P9, p9;
    P10, p10;
    P11, p11;
    P12, p12;
    P13, p13;
    P14, p14;
    P15, p15);
impl_bitor!( Alt16, Alt17;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3;
    P4, p4;
    P5, p5;
    P6, p6;
    P7, p7;
    P8, p8;
    P9, p9;
    P10, p10;
    P11, p11;
    P12, p12;
    P13, p13;
    P14, p14;
    P15, p15);
impl_multi!( Alt17;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3;
    P4, p4;
    P5, p5;
    P6, p6;
    P7, p7;
    P8, p8;
    P9, p9;
    P10, p10;
    P11, p11;
    P12, p12;
    P13, p13;
    P14, p14;
    P15, p15;
    P16, p16);
impl_bitor!( Alt17, Alt18;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3;
    P4, p4;
    P5, p5;
    P6, p6;
    P7, p7;
    P8, p8;
    P9, p9;
    P10, p10;
    P11, p11;
    P12, p12;
    P13, p13;
    P14, p14;
    P15, p15;
    P16, p16);
impl_multi!( Alt18;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3;
    P4, p4;
    P5, p5;
    P6, p6;
    P7, p7;
    P8, p8;
    P9, p9;
    P10, p10;
    P11, p11;
    P12, p12;
    P13, p13;
    P14, p14;
    P15, p15;
    P16, p16;
    P17, p17);
impl_bitor!( Alt18, Alt19;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3;
    P4, p4;
    P5, p5;
    P6, p6;
    P7, p7;
    P8, p8;
    P9, p9;
    P10, p10;
    P11, p11;
    P12, p12;
    P13, p13;
    P14, p14;
    P15, p15;
    P16, p16;
    P17, p17);
impl_multi!( Alt19;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3;
    P4, p4;
    P5, p5;
    P6, p6;
    P7, p7;
    P8, p8;
    P9, p9;
    P10, p10;
    P11, p11;
    P12, p12;
    P13, p13;
    P14, p14;
    P15, p15;
    P16, p16;
    P17, p17;
    P18, p18);
impl_bitor!( Alt19, Alt20;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3;
    P4, p4;
    P5, p5;
    P6, p6;
    P7, p7;
    P8, p8;
    P9, p9;
    P10, p10;
    P11, p11;
    P12, p12;
    P13, p13;
    P14, p14;
    P15, p15;
    P16, p16;
    P17, p17;
    P18, p18);
impl_multi!( Alt20;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3;
    P4, p4;
    P5, p5;
    P6, p6;
    P7, p7;
    P8, p8;
    P9, p9;
    P10, p10;
    P11, p11;
    P12, p12;
    P13, p13;
    P14, p14;
    P15, p15;
    P16, p16;
    P17, p17;
    P18, p18;
    P19, p19);
impl_bitor!( Alt20, Alt21;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3;
    P4, p4;
    P5, p5;
    P6, p6;
    P7, p7;
    P8, p8;
    P9, p9;
    P10, p10;
    P11, p11;
    P12, p12;
    P13, p13;
    P14, p14;
    P15, p15;
    P16, p16;
    P17, p17;
    P18, p18;
    P19, p19);
impl_multi!( Alt21;
    P0, p0;
    P1, p1;
    P2, p2;
    P3, p3;
    P4, p4;
    P5, p5;
    P6, p6;
    P7, p7;
    P8, p8;
    P9, p9;
    P10, p10;
    P11, p11;
    P12, p12;
    P13, p13;
    P14, p14;
    P15, p15;
    P16, p16;
    P17, p17;
    P18, p18;
    P19, p19;
    P20, p20);