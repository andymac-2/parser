use super::{error, stream, Parser};

macro_rules! parse_multi {
    ( $this:ident $stream:ident; $($parser:ident $result:ident),* ) => {
        {
            let ( $(ref $parser, )* ) = $this;
            $(
                let $result = $parser.parse($stream)?;
            )*
            Ok(( $($result, )* ))
        }
    }
}

macro_rules! eat_multi {
    ( $this:ident $stream:ident; $($parser:ident),* ) => {
        {
            let ( $(ref $parser, )* ) = $this;
            $(
                let _ = $parser.eat($stream)?;
            )*
            Ok(())
        }
    }
}

macro_rules! impl_multi {
    ( $($parser_t:ident, $result_t:ident, $parser:ident, $result:ident);* ) => {
        impl < $( $parser_t , )* $( $result_t , )* S, E, D, Err>
            Parser <S, ( $($result_t, )* ), E, D, Err>
            for ( $($parser_t, )* )
        where
            S: stream::Stream,
            E: error::ParseError<S::Item, S::Slice, D, Err>,
            $(
                $parser_t: Parser<S, $result_t, E, D, Err>,
            )*
        {
            fn parse(&self, stream: &mut S) -> Result<( $($result_t, )* ), E> {
                parse_multi!(self stream; $($parser $result),* )
            }
            fn eat(&self, stream: &mut S) -> Result<(), E> {
                eat_multi!(self stream; $($parser),* )
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

impl_multi!(
    P0, R0, p0, r0);
impl_multi!(
    P0, R0, p0, r0;
    P1, R1, p1, r1);
impl_multi!(
    P0, R0, p0, r0;
    P1, R1, p1, r1;
    P2, R2, p2, r2);
impl_multi!(
    P0, R0, p0, r0;
    P1, R1, p1, r1;
    P2, R2, p2, r2;
    P3, R3, p3, r3);
impl_multi!(
    P0, R0, p0, r0;
    P1, R1, p1, r1;
    P2, R2, p2, r2;
    P3, R3, p3, r3;
    P4, R4, p4, r4);
impl_multi!(
    P0, R0, p0, r0;
    P1, R1, p1, r1;
    P2, R2, p2, r2;
    P3, R3, p3, r3;
    P4, R4, p4, r4;
    P5, R5, p5, r5);
impl_multi!(
    P0, R0, p0, r0;
    P1, R1, p1, r1;
    P2, R2, p2, r2;
    P3, R3, p3, r3;
    P4, R4, p4, r4;
    P5, R5, p5, r5;
    P6, R6, p6, r6);
impl_multi!(
    P0, R0, p0, r0;
    P1, R1, p1, r1;
    P2, R2, p2, r2;
    P3, R3, p3, r3;
    P4, R4, p4, r4;
    P5, R5, p5, r5;
    P6, R6, p6, r6;
    P7, R7, p7, r7);
impl_multi!(
    P0, R0, p0, r0;
    P1, R1, p1, r1;
    P2, R2, p2, r2;
    P3, R3, p3, r3;
    P4, R4, p4, r4;
    P5, R5, p5, r5;
    P6, R6, p6, r6;
    P7, R7, p7, r7;
    P8, R8, p8, r8);
impl_multi!(
    P0, R0, p0, r0;
    P1, R1, p1, r1;
    P2, R2, p2, r2;
    P3, R3, p3, r3;
    P4, R4, p4, r4;
    P5, R5, p5, r5;
    P6, R6, p6, r6;
    P7, R7, p7, r7;
    P8, R8, p8, r8;
    P9, R9, p9, r9);
impl_multi!(
    P0, R0, p0, r0;
    P1, R1, p1, r1;
    P2, R2, p2, r2;
    P3, R3, p3, r3;
    P4, R4, p4, r4;
    P5, R5, p5, r5;
    P6, R6, p6, r6;
    P7, R7, p7, r7;
    P8, R8, p8, r8;
    P9, R9, p9, r9;
    P10, R10, p10, r10);
impl_multi!(
    P0, R0, p0, r0;
    P1, R1, p1, r1;
    P2, R2, p2, r2;
    P3, R3, p3, r3;
    P4, R4, p4, r4;
    P5, R5, p5, r5;
    P6, R6, p6, r6;
    P7, R7, p7, r7;
    P8, R8, p8, r8;
    P9, R9, p9, r9;
    P10, R10, p10, r10;
    P11, R11, p11, r11);
impl_multi!(
    P0, R0, p0, r0;
    P1, R1, p1, r1;
    P2, R2, p2, r2;
    P3, R3, p3, r3;
    P4, R4, p4, r4;
    P5, R5, p5, r5;
    P6, R6, p6, r6;
    P7, R7, p7, r7;
    P8, R8, p8, r8;
    P9, R9, p9, r9;
    P10, R10, p10, r10;
    P11, R11, p11, r11;
    P12, R12, p12, r12);
impl_multi!(
    P0, R0, p0, r0;
    P1, R1, p1, r1;
    P2, R2, p2, r2;
    P3, R3, p3, r3;
    P4, R4, p4, r4;
    P5, R5, p5, r5;
    P6, R6, p6, r6;
    P7, R7, p7, r7;
    P8, R8, p8, r8;
    P9, R9, p9, r9;
    P10, R10, p10, r10;
    P11, R11, p11, r11;
    P12, R12, p12, r12;
    P13, R13, p13, r13);
impl_multi!(
    P0, R0, p0, r0;
    P1, R1, p1, r1;
    P2, R2, p2, r2;
    P3, R3, p3, r3;
    P4, R4, p4, r4;
    P5, R5, p5, r5;
    P6, R6, p6, r6;
    P7, R7, p7, r7;
    P8, R8, p8, r8;
    P9, R9, p9, r9;
    P10, R10, p10, r10;
    P11, R11, p11, r11;
    P12, R12, p12, r12;
    P13, R13, p13, r13;
    P14, R14, p14, r14);
impl_multi!(
    P0, R0, p0, r0;
    P1, R1, p1, r1;
    P2, R2, p2, r2;
    P3, R3, p3, r3;
    P4, R4, p4, r4;
    P5, R5, p5, r5;
    P6, R6, p6, r6;
    P7, R7, p7, r7;
    P8, R8, p8, r8;
    P9, R9, p9, r9;
    P10, R10, p10, r10;
    P11, R11, p11, r11;
    P12, R12, p12, r12;
    P13, R13, p13, r13;
    P14, R14, p14, r14;
    P15, R15, p15, r15);
impl_multi!(
    P0, R0, p0, r0;
    P1, R1, p1, r1;
    P2, R2, p2, r2;
    P3, R3, p3, r3;
    P4, R4, p4, r4;
    P5, R5, p5, r5;
    P6, R6, p6, r6;
    P7, R7, p7, r7;
    P8, R8, p8, r8;
    P9, R9, p9, r9;
    P10, R10, p10, r10;
    P11, R11, p11, r11;
    P12, R12, p12, r12;
    P13, R13, p13, r13;
    P14, R14, p14, r14;
    P15, R15, p15, r15;
    P16, R16, p16, r16);
impl_multi!(
    P0, R0, p0, r0;
    P1, R1, p1, r1;
    P2, R2, p2, r2;
    P3, R3, p3, r3;
    P4, R4, p4, r4;
    P5, R5, p5, r5;
    P6, R6, p6, r6;
    P7, R7, p7, r7;
    P8, R8, p8, r8;
    P9, R9, p9, r9;
    P10, R10, p10, r10;
    P11, R11, p11, r11;
    P12, R12, p12, r12;
    P13, R13, p13, r13;
    P14, R14, p14, r14;
    P15, R15, p15, r15;
    P16, R16, p16, r16;
    P17, R17, p17, r17);
impl_multi!(
    P0, R0, p0, r0;
    P1, R1, p1, r1;
    P2, R2, p2, r2;
    P3, R3, p3, r3;
    P4, R4, p4, r4;
    P5, R5, p5, r5;
    P6, R6, p6, r6;
    P7, R7, p7, r7;
    P8, R8, p8, r8;
    P9, R9, p9, r9;
    P10, R10, p10, r10;
    P11, R11, p11, r11;
    P12, R12, p12, r12;
    P13, R13, p13, r13;
    P14, R14, p14, r14;
    P15, R15, p15, r15;
    P16, R16, p16, r16;
    P17, R17, p17, r17;
    P18, R18, p18, r18);
impl_multi!(
    P0, R0, p0, r0;
    P1, R1, p1, r1;
    P2, R2, p2, r2;
    P3, R3, p3, r3;
    P4, R4, p4, r4;
    P5, R5, p5, r5;
    P6, R6, p6, r6;
    P7, R7, p7, r7;
    P8, R8, p8, r8;
    P9, R9, p9, r9;
    P10, R10, p10, r10;
    P11, R11, p11, r11;
    P12, R12, p12, r12;
    P13, R13, p13, r13;
    P14, R14, p14, r14;
    P15, R15, p15, r15;
    P16, R16, p16, r16;
    P17, R17, p17, r17;
    P18, R18, p18, r18;
    P19, R19, p19, r19);
impl_multi!(
    P0, R0, p0, r0;
    P1, R1, p1, r1;
    P2, R2, p2, r2;
    P3, R3, p3, r3;
    P4, R4, p4, r4;
    P5, R5, p5, r5;
    P6, R6, p6, r6;
    P7, R7, p7, r7;
    P8, R8, p8, r8;
    P9, R9, p9, r9;
    P10, R10, p10, r10;
    P11, R11, p11, r11;
    P12, R12, p12, r12;
    P13, R13, p13, r13;
    P14, R14, p14, r14;
    P15, R15, p15, r15;
    P16, R16, p16, r16;
    P17, R17, p17, r17;
    P18, R18, p18, r18;
    P19, R19, p19, r19;
    P20, R20, p20, r20);