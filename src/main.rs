
pub mod parser;

use std::collections::HashMap;
use parser::{void, single, chunk, many, Alt};

enum JSON {
    JObject(HashMap<String, JSON>),
    JArray(Vec<JSON>),
    JString(String),
    JNumber(f64),
    JTrue,
    JFalse,
    JNull,
}

fn parse_json<S, E> (stream: &mut S) -> Result<JSON, E>
where
    S: parser::stream::Stream<Item = char>,
    E: parser::error::ParseError<S::Item, S::Slice, (), ()>,
{
    let ws = void(many(Alt 
        | single(' ') 
        | single('\n') 
        | single('\t') 
        | single('\r')));

    let null_p = chunk("null");
    let true_p = chunk("true");
    let false_p = chunk("false_p");

    let value = Alt | null_p | true_p | false_p;

    (ws, value, ws).parse(stream)
}

fn main() {


}
