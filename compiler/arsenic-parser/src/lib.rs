use std::cell::LazyCell;

use ast::{
    Arg, BinanyOp, Expression, FuncDeclaration, Ident, Literal, LiteralKind, Modifier, Module,
    Statement, UnaryOp, VarDeclaration,
};
use enumflags2::BitFlag;
use itertools::Itertools;
use pest::Parser as P;
use pest::iterators::{Pair, Pairs};
use pest::pratt_parser::{Assoc, Op, PrattParser};
use pest_derive::Parser;
use span::Span;

mod ast;
mod span;

const PRATT_PARSER: LazyCell<PrattParser<Rule>> = LazyCell::new(|| {
    PrattParser::new()
        .op(Op::infix(Rule::nullish_eq, Assoc::Right)
            | Op::infix(Rule::lshift_eq, Assoc::Right)
            | Op::infix(Rule::rshift_eq, Assoc::Right)
            | Op::infix(Rule::and_eq, Assoc::Right)
            | Op::infix(Rule::or_eq, Assoc::Right)
            | Op::infix(Rule::pow_eq, Assoc::Right)
            | Op::infix(Rule::add_eq, Assoc::Right)
            | Op::infix(Rule::sub_eq, Assoc::Right)
            | Op::infix(Rule::mul_eq, Assoc::Right)
            | Op::infix(Rule::div_eq, Assoc::Right)
            | Op::infix(Rule::rem_eq, Assoc::Right)
            | Op::infix(Rule::bwand_eq, Assoc::Right)
            | Op::infix(Rule::bwor_eq, Assoc::Right)
            | Op::infix(Rule::bwxor_eq, Assoc::Right)
            | Op::infix(Rule::bwnot_eq, Assoc::Right)
            | Op::infix(Rule::eq, Assoc::Right))
        .op(Op::infix(Rule::nullish, Assoc::Left) | Op::infix(Rule::or, Assoc::Left))
        .op(Op::infix(Rule::and, Assoc::Left))
        .op(Op::infix(Rule::bwor, Assoc::Left))
        .op(Op::infix(Rule::bwxor, Assoc::Left))
        .op(Op::infix(Rule::bwand, Assoc::Left))
        .op(Op::infix(Rule::equality, Assoc::Left) | Op::infix(Rule::inequality, Assoc::Left))
        .op(Op::infix(Rule::gt_eq, Assoc::Left)
            | Op::infix(Rule::gt, Assoc::Left)
            | Op::infix(Rule::lt_eq, Assoc::Left)
            | Op::infix(Rule::lt, Assoc::Left))
        .op(Op::infix(Rule::lshift, Assoc::Left) | Op::infix(Rule::rshift, Assoc::Left))
        .op(Op::infix(Rule::add, Assoc::Left) | Op::infix(Rule::sub, Assoc::Left))
        .op(Op::infix(Rule::mul, Assoc::Left)
            | Op::infix(Rule::div, Assoc::Left)
            | Op::infix(Rule::rem, Assoc::Left))
        .op(Op::infix(Rule::pow, Assoc::Right))
        .op(Op::prefix(Rule::plus) | Op::prefix(Rule::neg))
        .op(Op::postfix(Rule::early_ret))
        .op(Op::postfix(Rule::open_parent) | Op::postfix(Rule::open_bracket))
        .op(Op::infix(Rule::dot, Assoc::Left))
});

type FileId = usize;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct Parser;

pub fn parse<'s>(file_id: FileId, input: &'s str) -> Result<Module, pest::error::Error<Rule>> {
    let mut pairs = Parser::parse(Rule::main, input)?;
    let main = pairs.next().unwrap();
    debug_assert!(pairs.next().is_none());
    debug_assert!(main.as_rule() == Rule::main);

    Ok(Module {
        span: Span::from_pest(file_id, main.as_span()),
        statements: parse_compound(file_id, main.into_inner()),
    })
}

fn parse_compound<'i>(file_id: FileId, pairs: Pairs<'i, Rule>) -> Vec<Statement> {
    pairs
        .filter_map(|pair| match pair.as_rule() {
            Rule::let_declaration | Rule::const_declaration => {
                let span = Span::from_pest(file_id, pair.as_span());
                let modifiers = match pair.as_rule() {
                    Rule::let_declaration => Modifier::Mutability.into(),
                    _ => Modifier::empty(),
                };

                let mut inner = pair.into_inner();
                let name = parse_ident(file_id, inner.next().unwrap());
                let mut ty = inner.next().map(|pair| parse_expr(file_id, pair));
                let value = inner
                    .next()
                    .map(|pair| parse_expr(file_id, pair))
                    .or_else(|| ty.take())
                    .unwrap();

                Some(Statement::VarDeclaration(VarDeclaration {
                    modifiers,
                    name,
                    ty,
                    value,
                    span,
                }))
            }
            Rule::function_declaration => {
                let span = Span::from_pest(file_id, pair.as_span());
                let mut inner = pair.into_inner();
                let name = parse_ident(file_id, inner.next().unwrap());
                let args = parse_args(file_id, inner.next().unwrap());
                let return_ty = parse_expr(file_id, inner.next().unwrap());
                let body = parse_compound(file_id, inner);

                Some(Statement::FuncDeclaration(FuncDeclaration {
                    name,
                    args,
                    return_ty,
                    body,
                    span,
                }))
            }
            Rule::EOI => None,
            _ => unreachable!(),
        })
        .collect::<Vec<_>>()
}

fn parse_args<'i>(file_id: FileId, pair: Pair<'i, Rule>) -> Vec<Arg> {
    debug_assert!(pair.as_rule() == Rule::function_args);

    pair.into_inner()
        .chunks(2)
        .into_iter()
        .map(|mut arg| {
            let name = parse_ident(file_id, arg.next().unwrap());
            let ty = parse_expr(file_id, arg.next().unwrap());

            Arg {
                modifiers: Modifier::empty(),
                span: name.span.extent_right(ty.span()),
                name,
                ty,
            }
        })
        .collect()
}

fn parse_ident<'i>(file_id: FileId, pair: Pair<'i, Rule>) -> Ident {
    debug_assert!(pair.as_rule() == Rule::identifier);

    Ident {
        value: pair.as_str().to_string(),
        span: Span::from_pest(file_id, pair.as_span()),
    }
}

fn parse_literal<'i>(file_id: FileId, pair: Pair<'i, Rule>) -> Literal {
    debug_assert!(matches!(pair.as_rule(), Rule::integer | Rule::float));

    Literal {
        raw: pair.as_str().to_string(),
        span: Span::from_pest(file_id, pair.as_span()),
        kind: match pair.as_rule() {
            Rule::integer => LiteralKind::Int(pair.as_str().parse().unwrap()),
            Rule::float => LiteralKind::Float(pair.as_str().parse().unwrap()),

            _ => unreachable!(),
        },
    }
}

fn parse_expr<'i>(file_id: FileId, pair: Pair<'i, Rule>) -> Expression {
    let x = PRATT_PARSER
        .map_primary(|primary| match primary.as_rule() {
            Rule::expression | Rule::expression_ty => parse_expr(file_id, primary), // from "(" ~ expr ~ ")"
            Rule::integer | Rule::float => Expression::Literal(parse_literal(file_id, primary)),
            Rule::call => Expression::Ident(parse_ident(file_id, primary)),
            Rule::access => Expression::Ident(parse_ident(file_id, primary)),
            Rule::identifier => Expression::Ident(parse_ident(file_id, primary)),
            _ => unreachable!(),
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::plus => Expression::UnaryOp(UnaryOp::Plus, Box::new(rhs)),
            Rule::neg => Expression::UnaryOp(UnaryOp::Neg, Box::new(rhs)),
            Rule::not => Expression::UnaryOp(UnaryOp::Not, Box::new(rhs)),
            Rule::bwnot => Expression::UnaryOp(UnaryOp::Bwnot, Box::new(rhs)),
            _ => unreachable!(),
        })
        .map_postfix(|lhs, op| match op.as_rule() {
            Rule::early_ret => Expression::EarlyRet(Box::new(lhs)),
            _ => unreachable!(),
        })
        .map_infix(|lhs, op, rhs| match op.as_rule() {
            Rule::lshift_eq => Expression::AssignOp(BinanyOp::Lshift, Box::new(lhs), Box::new(rhs)),
            Rule::rshift_eq => Expression::AssignOp(BinanyOp::Rshift, Box::new(lhs), Box::new(rhs)),
            Rule::add_eq => Expression::AssignOp(BinanyOp::Add, Box::new(lhs), Box::new(rhs)),
            Rule::sub_eq => Expression::AssignOp(BinanyOp::Sub, Box::new(lhs), Box::new(rhs)),
            Rule::mul_eq => Expression::AssignOp(BinanyOp::Mul, Box::new(lhs), Box::new(rhs)),
            Rule::div_eq => Expression::AssignOp(BinanyOp::Div, Box::new(lhs), Box::new(rhs)),
            Rule::rem_eq => Expression::AssignOp(BinanyOp::Rem, Box::new(lhs), Box::new(rhs)),
            Rule::bwand_eq => Expression::AssignOp(BinanyOp::Bwand, Box::new(lhs), Box::new(rhs)),
            Rule::bwor_eq => Expression::AssignOp(BinanyOp::Bwor, Box::new(lhs), Box::new(rhs)),
            Rule::bwxor_eq => Expression::AssignOp(BinanyOp::Bwxor, Box::new(lhs), Box::new(rhs)),
            Rule::pow_eq => Expression::AssignOp(BinanyOp::Pow, Box::new(lhs), Box::new(rhs)),
            Rule::and_eq => Expression::AssignOp(BinanyOp::And, Box::new(lhs), Box::new(rhs)),
            Rule::or_eq => Expression::AssignOp(BinanyOp::Or, Box::new(lhs), Box::new(rhs)),
            Rule::nullish_eq => {
                Expression::AssignOp(BinanyOp::Nullish, Box::new(lhs), Box::new(rhs))
            }

            Rule::equality => {
                Expression::BinaryOp(BinanyOp::Equality, Box::new(lhs), Box::new(rhs))
            }
            Rule::inequality => {
                Expression::BinaryOp(BinanyOp::Inequality, Box::new(lhs), Box::new(rhs))
            }
            Rule::gt_eq => Expression::BinaryOp(BinanyOp::GtEq, Box::new(lhs), Box::new(rhs)),
            Rule::lt_eq => Expression::BinaryOp(BinanyOp::LtEq, Box::new(lhs), Box::new(rhs)),
            Rule::pow => Expression::BinaryOp(BinanyOp::Pow, Box::new(lhs), Box::new(rhs)),
            Rule::lshift => Expression::BinaryOp(BinanyOp::Lshift, Box::new(lhs), Box::new(rhs)),
            Rule::rshift => Expression::BinaryOp(BinanyOp::Rshift, Box::new(lhs), Box::new(rhs)),
            Rule::and => Expression::BinaryOp(BinanyOp::And, Box::new(lhs), Box::new(rhs)),
            Rule::or => Expression::BinaryOp(BinanyOp::Or, Box::new(lhs), Box::new(rhs)),
            Rule::nullish => Expression::BinaryOp(BinanyOp::Nullish, Box::new(lhs), Box::new(rhs)),
            Rule::gt => Expression::BinaryOp(BinanyOp::Gt, Box::new(lhs), Box::new(rhs)),
            Rule::lt => Expression::BinaryOp(BinanyOp::Lt, Box::new(lhs), Box::new(rhs)),
            Rule::add => Expression::BinaryOp(BinanyOp::Add, Box::new(lhs), Box::new(rhs)),
            Rule::sub => Expression::BinaryOp(BinanyOp::Sub, Box::new(lhs), Box::new(rhs)),
            Rule::mul => Expression::BinaryOp(BinanyOp::Mul, Box::new(lhs), Box::new(rhs)),
            Rule::div => Expression::BinaryOp(BinanyOp::Div, Box::new(lhs), Box::new(rhs)),
            Rule::rem => Expression::BinaryOp(BinanyOp::Rem, Box::new(lhs), Box::new(rhs)),
            Rule::bwand => Expression::BinaryOp(BinanyOp::Bwand, Box::new(lhs), Box::new(rhs)),
            Rule::bwor => Expression::BinaryOp(BinanyOp::Bwor, Box::new(lhs), Box::new(rhs)),
            Rule::bwxor => Expression::BinaryOp(BinanyOp::Bwxor, Box::new(lhs), Box::new(rhs)),

            Rule::eq => Expression::Assign(Box::new(lhs), Box::new(rhs)),
            Rule::dot => Expression::Field(Box::new(lhs), Box::new(rhs)),
            _ => unreachable!(),
        })
        .parse(pair.into_inner());
    x
}
