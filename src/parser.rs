use std::{iter::Peekable, slice::Iter};

use crate::lexer::{AstNumber, Token};

use super::lexer::FuncType;

#[derive(Debug, PartialEq, Clone)]
pub enum AstNode {
    NumNode(AstNumber),
    FuncNode(AstFunction),
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstFunction {
    pub func: FuncType,
    pub operands: Vec<AstNode>,
}

pub fn parse_tokens<'a>(
    tokens: &mut Peekable<Iter<'a, Token<'a>>>,
) -> Result<AstNode, ParseResult> {
    parse_f_expr(tokens)
}

#[derive(Debug, PartialEq)]
pub enum ParseResult {
    Incomplete,
    Err(String),
}

fn parse_f_expr<'a>(tokens: &mut Peekable<Iter<'a, Token<'a>>>) -> Result<AstNode, ParseResult> {
    if let Some(Token::LeftParen) = tokens.next() {
        if let Some(Token::Func(func_token)) = tokens.next() {
            let operands = match func_token {
                FuncType::Unary(_) => match parse_one_arg(tokens) {
                    Ok(parsed_arg) => Ok(vec![parsed_arg]),
                    Err(error) => Err(error),
                },
                FuncType::Binary(_) => parse_two_args(tokens),
                FuncType::NAry(_) => parse_many_args(tokens),
            };
            let node = AstNode::FuncNode(AstFunction {
                func: func_token.clone(),
                operands: operands?,
            });

            if let Some(Token::RightParen) = tokens.next() {
                Ok(node)
            } else {
                // Return the incomplete parse result to request further processing
                Err(ParseResult::Incomplete)
            }
        } else {
            Err(ParseResult::Err(String::from(
                "The first word is not a known function",
            )))
        }
    } else {
        Err(ParseResult::Err(String::from(
            "f_expr must start with a left parentheses",
        )))
    }
}

fn parse_one_arg<'a>(tokens: &mut Peekable<Iter<'a, Token<'a>>>) -> Result<AstNode, ParseResult> {
    let s_expr = parse_s_expr(tokens)?;

    if let Some(Token::RightParen) = tokens.peek() {
        Ok(s_expr)
    } else {
        Err(ParseResult::Err(String::from(
            "This type of function takes only one argument",
        )))
    }
}

fn parse_two_args<'a>(
    mut tokens: &mut Peekable<Iter<'a, Token<'a>>>,
) -> Result<Vec<AstNode>, ParseResult> {
    let first_expr = parse_s_expr(&mut tokens);
    let second_expr = parse_s_expr(&mut tokens);
    if let Some(Token::RightParen) = tokens.peek() {
        Ok(vec![first_expr?, second_expr?])
    } else {
        Err(ParseResult::Err(String::from(
            "This type of function takes exactly two arguments",
        )))
    }
}

fn parse_many_args<'a>(
    tokens: &mut Peekable<Iter<'a, Token<'a>>>,
) -> Result<Vec<AstNode>, ParseResult> {
    let mut operands = Vec::new();
    while tokens.peek() != Some(&&Token::RightParen) {
        operands.push(parse_s_expr(tokens)?)
    }

    return Ok(operands);
}

fn parse_s_expr<'a>(tokens: &mut Peekable<Iter<'a, Token<'a>>>) -> Result<AstNode, ParseResult> {
    // TODO: Avoid clone
    match tokens.peek() {
        Some(Token::Number(number)) => {
            tokens.next();
            Ok(AstNode::NumNode(number.clone()))
        }
        Some(Token::LeftParen) => parse_f_expr(tokens),
        Some(token) => Err(ParseResult::Err(format!(
            "Unexpected token for start of s_expr: {}",
            token
        ))),
        None => Err(ParseResult::Incomplete),
    }
}

pub fn print_abstract_syntax_tree(root: AstNode, indentation: i32) {
    let mut indentation_marker = String::new();
    for _ in 0..indentation {
        indentation_marker.push_str("| ");
    }
    match root {
        AstNode::FuncNode(function) => {
            println!("{}{:?}", indentation_marker, function.func);
            for operand in function.operands {
                print_abstract_syntax_tree(operand, indentation + 1);
            }
        }
        AstNode::NumNode(number) => {
            println!("{}{:?}", indentation_marker, number);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{parse_tokens, AstFunction, AstNode::*, AstNumber::*, ParseResult};

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
            parse_tokens(&mut tokens).expect("It should parse without throwing an error"),
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

    #[test]
    fn it_parses_multiple_composite_functions() {
        // (sub (mult 1 2) (add 1 2 3 4))
        let tokens = vec![
            LeftParen,
            Func(Binary(Sub)),
            LeftParen,
            Func(NAry(Mult)),
            Number(Int(1)),
            Number(Int(2)),
            RightParen,
            LeftParen,
            Func(NAry(Add)),
            Number(Int(1)),
            Number(Int(2)),
            Number(Int(3)),
            Number(Int(4)),
            RightParen,
            RightParen,
        ];
        let mut tokens = tokens.iter().peekable();
        let tree_result = FuncNode(AstFunction {
            func: Binary(Sub),
            operands: vec![
                FuncNode(AstFunction {
                    func: NAry(Mult),
                    operands: vec![NumNode(Int(1)), NumNode(Int(2))],
                }),
                FuncNode(AstFunction {
                    func: NAry(Add),
                    operands: vec![
                        NumNode(Int(1)),
                        NumNode(Int(2)),
                        NumNode(Int(3)),
                        NumNode(Int(4)),
                    ],
                }),
            ],
        });

        assert_eq!(
            parse_tokens(&mut tokens).expect("It should parse without throwing an error."),
            tree_result
        )
    }

    #[test]
    fn it_handles_partial_expressions() {
        let tokens = vec![LeftParen, Func(NAry(Add)), Number(Int(3))];
        let mut tokens = tokens.iter().peekable();

        assert_eq!(parse_tokens(&mut tokens), Err(ParseResult::Incomplete))
    }
}
