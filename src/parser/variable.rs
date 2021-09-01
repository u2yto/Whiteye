use nom::bytes::complete::{is_a, tag};
use nom::character::complete::{alphanumeric0, multispace0};
use nom::error::VerboseError;
use nom::sequence::delimited;
use nom::IResult;

use super::expression::parse_add_sub;
use crate::ast::{AssignmentOpKind, Ast, VariableType};

pub fn parse_variable_declaration(input: &str) -> IResult<&str, Ast, VerboseError<&str>> {
    let (input, variable_name) = delimited(
        tag("let"),
        delimited(multispace0, parse_variable_name, multispace0),
        tag(":"),
    )(input)?;
    let (input, variable_type) = parse_variable_type(input)?;
    let (input, _) = tag("=")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, variable_expr) = parse_add_sub(input)?;
    Ok((
        input,
        Ast::VariableDeclaration {
            name: variable_name.to_string(),
            data_type: variable_type,
            expr: Box::new(variable_expr),
        },
    ))
}

pub fn parse_variable_assignment(input: &str) -> IResult<&str, Ast, VerboseError<&str>> {
    let (input, variable_name) = parse_variable_name(input)?;
    let (input, assignment_op) =
        delimited(multispace0, parse_assignment_operator, multispace0)(input)?;
    let (input, variable_expr) = parse_add_sub(input)?;
    Ok((
        input,
        Ast::VariableAssignment {
            name: variable_name.to_string(),
            operator: assignment_op,
            expr: Box::new(variable_expr),
        },
    ))
}

pub fn parse_variable_name(input: &str) -> IResult<&str, &str, VerboseError<&str>> {
    let (input, variable_name) =
        is_a("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_")(input)?;
    match is_a::<&str, &str, VerboseError<&str>>(
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
    )(variable_name)
    {
        Ok((_, _)) => Ok((input, variable_name)),
        Err(e) => Err(e),
    }
}

pub fn parse_variable_type(input: &str) -> IResult<&str, VariableType, VerboseError<&str>> {
    let (input, variable_type_str) = delimited(multispace0, alphanumeric0, multispace0)(input)?;
    Ok((
        input,
        match variable_type_str {
            "int" => VariableType::Int,
            _ => panic!("Unknown VariableDeclaration type"),
        },
    ))
}

pub fn parse_assignment_operator(
    input: &str,
) -> IResult<&str, AssignmentOpKind, VerboseError<&str>> {
    let (input, assignment_op) = is_a("=+-*/")(input)?;
    Ok((
        input,
        match assignment_op {
            "=" => AssignmentOpKind::AEqual,
            "+=" => AssignmentOpKind::AAdd,
            "-=" => AssignmentOpKind::ASub,
            "*=" => AssignmentOpKind::AMul,
            "/=" => AssignmentOpKind::AMul,
            _ => panic!("Unknown Assignment Operation"),
        },
    ))
}
