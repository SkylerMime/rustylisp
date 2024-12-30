use std::{iter::Peekable, slice::Iter};

use crate::lexer::{AstNumber, Token};

use super::lexer::FuncType;

#[derive(Debug, PartialEq)]
pub enum AstNode {
    NumNode(AstNumber),
    FuncNode(AstFunction),
}

#[derive(Debug, PartialEq)]
pub struct AstFunction {
    func: FuncType,
    operands: Vec<AstNode>,
}

pub fn parse_tokens<'a>(
    tokens: &mut Peekable<Iter<'a, Token<'a>>>,
) -> Result<AstNode, &'static str> {
    parse_f_expr(tokens)
}

fn parse_f_expr<'a>(tokens: &mut Peekable<Iter<'a, Token<'a>>>) -> Result<AstNode, &'static str> {
    if let Some(Token::LeftParen) = tokens.next() {
        if let Some(Token::Func(func_token)) = tokens.next() {
            let operands = match func_token {
                FuncType::Unary(_) => vec![parse_one_arg(tokens).expect("TODO: Full error check")],
                FuncType::Binary(_) => parse_two_args(tokens).expect("TODO: Full error check"),
                FuncType::NAry(_) => parse_many_args(tokens).expect("TODO: Full error check"),
            };
            Ok(AstNode::FuncNode(AstFunction {
                func: func_token.clone(),
                operands,
            }))
        } else {
            Err("The first word is not a known function")
        }
    } else {
        Err("f_expr must start with a left parentheses")
    }
}

fn parse_one_arg<'a>(tokens: &mut Peekable<Iter<'a, Token<'a>>>) -> Result<AstNode, &'static str> {
    let s_expr = parse_s_expr(tokens);
    if let Ok(s_expr_ok) = s_expr {
        if let Some(Token::RightParen) = tokens.next() {
            Ok(s_expr_ok)
        } else {
            Err("This type of function takes only one argument")
        }
    } else {
        s_expr
    }
}

fn parse_two_args<'a>(
    mut tokens: &mut Peekable<Iter<'a, Token<'a>>>,
) -> Result<Vec<AstNode>, &'static str> {
    match parse_s_expr(&mut tokens) {
        Ok(first_expr) => match parse_s_expr(&mut tokens) {
            Ok(second_expr) => match tokens.next() {
                Some(Token::RightParen) => Ok(vec![first_expr, second_expr]),
                _ => Err("This type of function takes two arguments"),
            },
            Err(_) => Err("This type of function takes exactly two arguments"),
        },
        Err(err_msg) => Err(err_msg),
    }
}

fn parse_many_args<'a>(
    tokens: &mut Peekable<Iter<'a, Token<'a>>>,
) -> Result<Vec<AstNode>, &'static str> {
    let mut operands = Vec::new();
    while tokens.peek() != Some(&&Token::RightParen) {
        match parse_s_expr(tokens) {
            Ok(s_expr) => operands.push(s_expr),
            Err(msg) => return Err(msg),
        }
    }

    return Ok(operands);
}

fn parse_s_expr<'a>(tokens: &mut Peekable<Iter<'a, Token<'a>>>) -> Result<AstNode, &'static str> {
    // TODO: Avoid clone
    match tokens.peek() {
        Some(Token::Number(number)) => {
            tokens.next();
            Ok(AstNode::NumNode(number.clone()))
        }
        Some(Token::LeftParen) => parse_f_expr(tokens),
        _ => Err("Unexpected token for s_expr"),
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{parse_tokens, AstFunction, AstNode::*, AstNumber::*};

    use super::super::lexer::{Binary::*, FuncType::*, NAry::*, Token::*, Unary::*};

    #[test]
    fn it_parses_unary_functions() {
        let tokens = vec![LeftParen, Func(Unary(Neg)), Number(Int(3)), RightParen];
        let mut tokens = tokens.iter().peekable();
        let tree_result = FuncNode(AstFunction {
            func: Unary(Neg),
            operands: vec![NumNode(Int(3))],
        });
        assert_eq!(
            parse_tokens(&mut tokens).expect("It should parse without throwing an error."),
            tree_result
        );
    }

    #[test]
    fn it_parses_binary_functions() {
        let tokens = vec![
            LeftParen,
            Func(Binary(Div)),
            Number(Double(3.14)),
            Number(Int(5)),
            RightParen,
        ];
        let mut tokens = tokens.iter().peekable();
        let tree_result = FuncNode(AstFunction {
            func: Binary(Div),
            operands: vec![NumNode(Double(3.14)), NumNode(Int(5))],
        });

        assert_eq!(
            parse_tokens(&mut tokens).expect("It should parse without throwing an error."),
            tree_result
        );
    }

    #[test]
    fn it_parses_nary_functions() {
        let tokens = vec![
            LeftParen,
            Func(NAry(Add)),
            Number(Int(111)),
            Number(Int(222)),
            Number(Int(333)),
            Number(Int(444)),
            RightParen,
        ];
        let mut tokens = tokens.iter().peekable();
        let tree_result = FuncNode(AstFunction {
            func: NAry(Add),
            operands: vec![
                NumNode(Int(111)),
                NumNode(Int(222)),
                NumNode(Int(333)),
                NumNode(Int(444)),
            ],
        });

        assert_eq!(
            parse_tokens(&mut tokens).expect("It should parse without throwing an error."),
            tree_result
        )
    }

    #[test]
    fn it_parses_composite_functions() {
        let tokens = vec![
            LeftParen,
            Func(NAry(Add)),
            LeftParen,
            Func(Binary(Sub)),
            Number(Int(111)),
            Number(Int(222)),
            RightParen,
            Number(Int(333)),
            RightParen,
        ];
        let mut tokens = tokens.iter().peekable();
        let tree_result = FuncNode(AstFunction {
            func: NAry(Add),
            operands: vec![
                FuncNode(AstFunction {
                    func: Binary(Sub),
                    operands: vec![NumNode(Int(111)), NumNode(Int(222))],
                }),
                NumNode(Int(333)),
            ],
        });

        assert_eq!(
            parse_tokens(&mut tokens).expect("It should parse without throwing an error."),
            tree_result
        )
    }
}
