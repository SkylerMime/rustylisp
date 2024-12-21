use std::{error::Error, vec::IntoIter};

use crate::lexer::{AstNumber, Token};

use super::lexer::FuncType;

#[derive(Debug, PartialEq)]
pub struct AstNode<'a> {
    node_type: AstNodeType<'a>,
    next: Option<&'a AstNode<'a>>,
}

#[derive(Debug, PartialEq)]
enum AstNodeType<'a> {
    NumNode(AstNumber),
    FuncNode(AstFunction<'a>),
}

#[derive(Debug, PartialEq)]
pub struct AstFunction<'a> {
    func: FuncType,
    operands: Vec<AstNode<'a>>,
}

pub fn parse_tokens(tokens: IntoIter<Token>) -> Result<AstNode, &'static str> {
    parse_f_expr(tokens)
}

fn parse_f_expr(mut tokens: IntoIter<Token>) -> Result<AstNode, &'static str> {
    if let Some(Token::LeftParen) = tokens.next() {
        if let Some(Token::Func(func_token)) = tokens.next() {
            Ok(AstNode {
                node_type: AstNodeType::FuncNode(AstFunction {
                    func: func_token,
                    operands: vec![parse_one_arg(tokens).expect("TODO: Full error check")],
                }),
                next: None,
            })
        } else {
            Err("The first word is not a known function")
        }
    } else {
        Err("f_expr must start with a left parentheses")
    }
}

fn parse_one_arg(mut tokens: IntoIter<Token>) -> Result<AstNode, &'static str> {
    // TODO: Other types of nodes, not just a number
    if let Some(Token::Number(number)) = tokens.next() {
        if let Some(Token::RightParen) = tokens.next() {
            Ok(AstNode {
                node_type: AstNodeType::NumNode(number),
                next: None,
            })
        } else {
            Err("This type of function takes only one argument")
        }
    } else {
        Err("Composite functions are not yet implemented")
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{parse_tokens, AstFunction, AstNode, AstNodeType::*, AstNumber::*};

    use super::super::lexer::{FuncType::*, Token::*};

    #[test]
    fn it_parses_simple_functions() {
        let tokens = vec![LeftParen, Func(Neg), Number(Int(3)), RightParen].into_iter();
        let tree_result = AstNode {
            node_type: FuncNode(AstFunction {
                func: Neg,
                operands: vec![AstNode {
                    node_type: NumNode(Int(3)),
                    next: None,
                }],
            }),
            next: None,
        };
        assert_eq!(
            parse_tokens(tokens).expect("it should parse without throwing an error"),
            tree_result
        );
    }
}
