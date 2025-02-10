use std::{iter::Peekable, slice::Iter};

use crate::lexer::{AstNumber, Binary, NAry, Token};

use super::lexer::FuncType;

#[derive(Debug, PartialEq, Clone)]
pub enum AstNode {
    NumNode(AstNumber),
    FuncNode(AstFunction),
}

#[derive(Debug, PartialEq, Clone)]
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
                FuncType::Unary(_) => match parse_one_arg(tokens) {
                    Ok(parsed_arg) => Ok(vec![parsed_arg]),
                    Err(error) => Err(error),
                },
                FuncType::Binary(_) => parse_two_args(tokens),
                FuncType::NAry(_) => parse_many_args(tokens),
            };
            match operands {
                Ok(operands) => Ok(AstNode::FuncNode(AstFunction {
                    func: func_token.clone(),
                    operands,
                })),
                Err(error) => Err(error),
            }
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

pub fn eval(root: &AstNode) -> Result<AstNumber, String> {
    eval_s_expr(root)
}

fn eval_s_expr(s_expr: &AstNode) -> Result<AstNumber, String> {
    match s_expr {
        AstNode::FuncNode(function) => eval_function(function),
        AstNode::NumNode(number) => Ok(number.clone()),
    }
}

fn eval_function(function: &AstFunction) -> Result<AstNumber, String> {
    match function.func.clone() {
        FuncType::NAry(func_type) => match func_type {
            NAry::Add => {
                // TODO: Rework without extra clone / multiple streams
                let maybe_additions = function.operands.iter().map(|op| eval_s_expr(op));
                if maybe_additions
                    .clone()
                    .any(|evaluation| evaluation.is_err())
                {
                    return Err(maybe_additions
                        .filter(|evaluation| evaluation.is_err())
                        .map(|err| err.unwrap_err())
                        .collect());
                };
                maybe_additions
                    .map(|some_num| some_num.expect("These should all be 'Some' entries."))
                    .reduce(|a, b| add(a, b))
                    .ok_or_else(|| String::from("The arguments should not be empty"))
            }
            _ => {
                return Err(String::from("NAry function not implemented"));
            }
        },
        FuncType::Binary(func_type) => match func_type {
            Binary::Sub => {
                if let Some(first_arg) = function.operands.get(0) {
                    if let Some(second_arg) = function.operands.get(1) {
                        let first_evaluated = eval_s_expr(first_arg);
                        let second_evaluated = eval_s_expr(second_arg);

                        if let Ok(first) = first_evaluated {
                            if let Ok(second) = second_evaluated {
                                Ok(sub(first, second))
                            } else {
                                // Send up the error message
                                second_evaluated
                            }
                        } else {
                            // Send up the error message
                            first_evaluated
                        }
                    } else {
                        Err(String::from("Sub function requires two arguments"))
                    }
                } else {
                    Err(String::from("The arguments should not be empty"))
                }
            }
            _ => {
                return Err(String::from("Binary function not implemented"));
            }
        },
        _ => {
            return Err(String::from("Function not yet implemented"));
        }
    }
}

fn add(a: AstNumber, b: AstNumber) -> AstNumber {
    apply_with_case(
        a,
        b,
        |u: f32, v: f32| -> f32 { u + v },
        |u: u32, v: u32| -> u32 { u + v },
    )
}

fn sub(a: AstNumber, b: AstNumber) -> AstNumber {
    apply_with_case(
        a,
        b,
        |u: f32, v: f32| -> f32 { u - v },
        |u: u32, v: u32| -> u32 { u - v },
    )
}

fn apply_with_case(
    a: AstNumber,
    b: AstNumber,
    double_operation: fn(f32, f32) -> f32,
    int_operation: fn(u32, u32) -> u32,
) -> AstNumber {
    match (a, b) {
        (AstNumber::Int(first), AstNumber::Int(second)) => {
            AstNumber::Int(int_operation(first, second))
        }
        (AstNumber::Int(first), AstNumber::Double(second)) => {
            AstNumber::Double(double_operation(first as f32, second))
        }
        (AstNumber::Double(first), AstNumber::Int(second)) => {
            AstNumber::Double(double_operation(first, second as f32))
        }
        (AstNumber::Double(first), AstNumber::Double(second)) => {
            AstNumber::Double(double_operation(first, second))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{eval, parse_tokens, AstFunction, AstNode::*, AstNumber::*};

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

    #[test]
    fn it_evaluates_add_function() {
        let add_tree = FuncNode(AstFunction {
            func: NAry(Add),
            operands: vec![NumNode(Double(1.5)), NumNode(Int(8)), NumNode(Int(2))],
        });

        assert_eq!(eval(&add_tree), Ok(Double(11.5)))
    }

    #[test]
    fn it_evaluates_sub_function() {
        let sub_tree = FuncNode(AstFunction {
            func: Binary(Sub),
            operands: vec![NumNode(Int(8)), NumNode(Int(2))],
        });

        assert_eq!(eval(&sub_tree), Ok(Int(6)))
    }
}
