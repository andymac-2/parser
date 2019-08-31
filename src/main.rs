
pub mod parser;

use std::collections::HashMap;
use parser::{void, single, chunk, many, value, Alt, Parser};

#[derive(Debug, Clone)]
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
    S: parser::stream::Stream<Item = char, Slice = &'static str>,
    E: parser::error::ParseError<S::Item, S::Slice, (), ()>,
{
    let ws = void(many(Alt 
        | single(' ') 
        | single('\n') 
        | single('\t') 
        | single('\r')));

    let null_p = value(chunk("null"), JSON::JNull);
    let true_p = value(chunk("true"), JSON::JTrue);
    let false_p = value(chunk("false"), JSON::JFalse);

    let value = Alt | null_p | true_p | false_p;

    let characters = many()

    (ws.clone(), value, ws).parse(stream)
        .map(|(_, res, _)| res)
}

fn main() {


}
