use nom::{Err, IResult, character::complete::digit1};
use nom_language::error::{VerboseError, convert_error};

use crate::ast::Expr::{self, IntLit};

fn parse_int_lit(code: &str) -> IResult<&str, Expr, VerboseError<&str>> {
    let (rest, value) = digit1(code)?;
    Ok((rest, IntLit(value.parse().unwrap())))
}

#[test]
fn test_parse_int_lit() {
    assert_eq!(parse_int_lit("123abc"), Ok(("abc", IntLit(123))))
}

pub fn parse(code: &str) -> Result<(&str, Expr), String> {
    parse_int_lit(code).map_err(|err| match err {
        Err::Incomplete(_) => "incomplete".to_string(),
        Err::Error(verbose_error) | Err::Failure(verbose_error) => {
            convert_error(code, verbose_error)
        }
    })
}
