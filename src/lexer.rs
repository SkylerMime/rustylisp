extern crate pretty_env_logger;

use core::fmt;
use std::{
    fmt::Debug,
    fs,
    io::{self, stdout, Write},
};

pub enum InputMode {
    UserInput,
    FilePath(String),
}

impl InputMode {
    pub fn build(mut args: impl Iterator<Item = String>) -> InputMode {
        // first arg is the program name
        args.next();
        if let Some(file_path) = args.next() {
            InputMode::FilePath(file_path)
        } else {
            InputMode::UserInput
        }
    }
}

#[derive(Debug, PartialEq)]
struct LexStep<'a> {
    token: Token<'a>,
    remaining_to_lex: &'a str,
}

#[derive(Debug, PartialEq)]
pub enum AstNumber {
    Int(u32),
    Double(f32),
}

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Number(AstNumber),
    LeftParen,
    RightParen,
    Ident(&'a str),
    Func(FuncType),
    Quit,
    // TODO: Are these two needed?
    Invalid(char),
    Default,
}

#[derive(Debug, PartialEq)]
pub enum FuncType {
    Neg,
    Abs,
    Add,
    Sub,
    Mult,
    Div,
    Remainder,
    Exp,
    Exp2,
    Pow,
    Log,
    Sqrt,
    Cbrt,
    Hypot,
    Max,
    Min,
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{:?}>", self)
    }
}

pub fn read_file(path: String) {
    if let Ok(contents) = fs::read_to_string(path.clone()) {
        let tokens = lex_string(&contents);
        // result can be ignored in file-reading mode
        let _ = print_tokens(tokens);
    } else {
        println!("Fatal: File {} not found", path);
    }
}

pub fn read_lines() {
    print!("lexer $ ");
    stdout()
        .flush()
        .expect("Next line should have been writable");
    loop {
        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .expect("User input should have been readable");
        let tokens = lex_string(input.as_str());
        match print_tokens(tokens) {
            Ok(()) => {
                print!("\nlexer $ ");
                stdout()
                    .flush()
                    .expect("Next line should have been writable");
            }
            Err(_) => {
                println!("quitting...");
                break;
            }
        }
    }
}

fn lex_string(mut remaining_string: &str) -> Vec<Token> {
    let mut tokens = vec![];
    while let Some(result) = get_next_token(remaining_string) {
        if result.token == Token::Ident("") {
            error!("Infinite loop. Remaining: {}", result.remaining_to_lex);
            break;
        }
        debug!("Adding token: {}", result.token);
        remaining_string = result.remaining_to_lex;
        tokens.push(result.token);
    }
    tokens
}

fn get_next_token(remaining_string: &str) -> Option<LexStep> {
    let remaining_string = remaining_string.trim_start();
    let mut remaining_chars = remaining_string.chars();
    if let Some(first_char) = remaining_chars.nth(0) {
        Some(match first_char {
            'a'..='z' | 'A'..='Z' | '_' | '$' => get_ident_or_function(remaining_string),
            '0'..='9' => get_number(remaining_string),
            _ => {
                // Token is a single character
                LexStep {
                    token: match first_char {
                        '(' => Token::LeftParen,
                        ')' => Token::RightParen,
                        _ => Token::Invalid(first_char),
                    },
                    remaining_to_lex: remaining_chars.as_str(),
                }
            }
        })
    } else {
        None
    }
}

// NOTE: Identifiers aren't used on the current step but will be soon
fn get_ident_or_function(ident_and_remaining: &str) -> LexStep {
    let mut remaining_chars = ident_and_remaining.chars().enumerate();
    if let Some((_, 'a'..='z' | 'A'..='Z' | '_' | '$')) = remaining_chars.next() {
        ();
    } else {
        panic!("Not a valid identifier");
    }
    let mut identifier = "";
    let mut result = LexStep {
        token: Token::Default,
        remaining_to_lex: ident_and_remaining,
    };
    for (identifier_end, character) in remaining_chars {
        match character {
            'a'..='z' | 'A'..='Z' | '_' | '$' | '0'..='9'
                if identifier_end == ident_and_remaining.len() - 1 =>
            {
                result = LexStep {
                    token: Token::Ident(&ident_and_remaining[..identifier_end + 1]),
                    remaining_to_lex: "",
                };
                break;
            }
            'a'..='z' | 'A'..='Z' | '_' | '$' | '0'..='9' => (),
            _ => {
                let ident_and_remaining = ident_and_remaining;
                identifier = &ident_and_remaining[..identifier_end];
                result = LexStep {
                    token: Token::Ident(identifier),
                    remaining_to_lex: &ident_and_remaining[identifier_end..],
                };
                break;
            }
        };
    }
    // See if identifier is a reserved function
    LexStep {
        token: match identifier {
            "neg" => Token::Func(FuncType::Neg),
            "abs" => Token::Func(FuncType::Abs),
            "add" => Token::Func(FuncType::Add),
            "sub" => Token::Func(FuncType::Sub),
            "mult" => Token::Func(FuncType::Mult),
            "div" => Token::Func(FuncType::Div),
            "remainder" => Token::Func(FuncType::Remainder),
            "exp" => Token::Func(FuncType::Exp),
            "exp2" => Token::Func(FuncType::Exp2),
            "pow" => Token::Func(FuncType::Pow),
            "log" => Token::Func(FuncType::Log),
            "sqrt" => Token::Func(FuncType::Sqrt),
            "cbrt" => Token::Func(FuncType::Cbrt),
            "hypot" => Token::Func(FuncType::Hypot),
            "max" => Token::Func(FuncType::Max),
            "min" => Token::Func(FuncType::Min),
            "quit" => Token::Quit, // not technically a token, but a command to the lexer to exit.
            _ => result.token,
        },
        remaining_to_lex: result.remaining_to_lex,
    }
}

fn get_number(number_and_remaining: &str) -> LexStep {
    let mut number_type = AstNumber::Int(0);
    let mut remaining = "";
    let mut number = "";
    let number_and_enumeration = number_and_remaining.chars().enumerate();
    for (number_end, character) in number_and_enumeration {
        match character {
            '.' => {
                if number_type == AstNumber::Int(0) {
                    number_type = AstNumber::Double(0.0);
                } else {
                    error!(
                        "Valid numbers should have no more than one decimal point, token invalid"
                    );
                    return LexStep {
                        token: Token::Invalid('.'),
                        remaining_to_lex: remaining,
                    };
                }
            }
            '0'..='9' if number_end == number_and_remaining.len() - 1 => {
                number = &number_and_remaining[..number_end + 1];
                remaining = "";
                break;
            }
            '0'..='9' => (),
            _ => {
                number = &number_and_remaining[..number_end];
                remaining = &number_and_remaining[number_end..];
                break;
            }
        }
    }
    if number_type == AstNumber::Double(0.0) {
        LexStep {
            token: parse_double(number),
            remaining_to_lex: remaining,
        }
    } else {
        LexStep {
            token: parse_int(number),
            remaining_to_lex: remaining,
        }
    }
}

fn parse_double(number: &str) -> Token {
    Token::Number(AstNumber::Double(
        number.parse().expect("Number should be a parseable double"),
    ))
}

fn parse_int(number: &str) -> Token {
    Token::Number(AstNumber::Int(
        number.parse().expect("Number should be a parseable int"),
    ))
}

fn print_tokens(tokens: Vec<Token>) -> Result<(), Token> {
    for token in tokens {
        if token == Token::Quit {
            return Err(Token::Quit);
        } else {
            println!("{}", token);
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::AstNumber::*;
    use super::Token::*;
    use super::*;

    #[test]
    fn it_gets_ints() {
        let result = get_number("1234");
        assert_eq!(result.token, Number(Int(1234)));
    }

    #[test]
    fn it_lexes_ints() {
        let result = get_next_token("1234").unwrap();
        assert_eq!(result.token, Number(Int(1234)));
    }

    #[test]
    fn it_gets_doubles() {
        let result = get_number("1234.5678");
        assert_eq!(result.token, Number(Double(1234.5678)));
    }

    #[test]
    fn it_gets_idents() {
        let result = get_ident_or_function("test$_ABC");
        assert_eq!(
            result,
            LexStep {
                token: Ident("test$_ABC"),
                remaining_to_lex: ""
            }
        );
    }

    #[test]
    fn it_gets_idents_with_whitespace() {
        let response = get_ident_or_function("test \n nextword");
        assert_eq!(
            response,
            LexStep {
                token: Ident("test"),
                remaining_to_lex: " \n nextword"
            }
        )
    }

    #[test]
    fn it_lexes_first_token_in_two_steps() {
        let response = get_next_token("1234;").unwrap();
        assert_eq!(
            response,
            LexStep {
                token: Number(Int(1234)),
                remaining_to_lex: ";"
            }
        )
    }

    #[test]
    fn it_lexes_two_token_string() {
        let response = lex_string("1234)");
        assert_eq!(response, vec![Number(Int(1234)), RightParen])
    }

    #[test]
    fn it_lexes_identifier_and_right_paren() {
        let response = lex_string("firstvar)");
        assert_eq!(response, vec![Ident("firstvar"), RightParen])
    }

    #[test]
    fn it_ignores_whitespace() {
        let result = get_next_token("   \n\n  test  \n   nextword").unwrap();
        assert_eq!(result.token, Ident("test"));
    }

    #[test]
    fn it_lexes_test_line() {
        let response = lex_string("(add firstvar 123)");
        assert_eq!(
            response,
            vec![
                LeftParen,
                Func(FuncType::Add),
                Ident("firstvar"),
                Number(Int(123)),
                RightParen
            ]
        )
    }

    #[test]
    fn it_skips_invalid_chars() {
        let result = get_next_token(".523.3").unwrap();
        assert_eq!(result.token, Invalid('.'))
    }

    #[test]
    fn it_allows_ident_with_number() {
        let result = get_ident_or_function("var3");
        assert_eq!(
            result,
            LexStep {
                token: Ident("var3"),
                remaining_to_lex: ""
            }
        )
    }

    #[test]
    #[should_panic]
    fn ident_starting_with_number_invalid() {
        get_ident_or_function("3var");
    }
}
