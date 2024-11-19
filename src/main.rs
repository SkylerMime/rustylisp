use core::fmt;
use std::{
    env,
    fmt::Debug,
    fs,
    io::{self, stdout, Write},
};

extern crate pretty_env_logger;
#[macro_use]
extern crate log;

#[derive(Debug, PartialEq)]
struct LexStep<'a> {
    token: Token<'a>,
    remaining_to_lex: &'a str,
}

#[derive(Debug, PartialEq)]
enum Token<'a> {
    Int(u32),
    Float(f32),
    AddOp(char),
    MultOp(char),
    Semicolon,
    LeftParen,
    RightParen,
    Assign,
    Print,
    Repeat,
    Ident(&'a str),
    Invalid(char),
    Default,
    Quit,
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{:?}>", self)
    }
}

enum ProgramMode {
    UserInput,
    FilePath(String),
}

impl ProgramMode {
    pub fn build(mut args: impl Iterator<Item = String>) -> ProgramMode {
        // first arg is the program name
        args.next();
        if let Some(file_path) = args.next() {
            ProgramMode::FilePath(file_path)
        } else {
            ProgramMode::UserInput
        }
    }
}

fn main() {
    pretty_env_logger::init();
    match ProgramMode::build(env::args()) {
        ProgramMode::UserInput => read_lines(),
        ProgramMode::FilePath(file_path) => read_file(file_path),
    };
}

fn read_file(path: String) {
    // TODO: More robust error handling for bad file path
    let contents = fs::read_to_string(path).expect("Should have been able to read the file");
    let tokens = lex_string(&contents);
    // result can be ignored in file-reading mode
    let _ = print_tokens(tokens);
}

fn read_lines() {
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
            'a'..='z' | 'A'..='Z' | '_' | '$' => get_ident(remaining_string),
            '0'..='9' => get_number(remaining_string),
            _ => {
                // Token is a single character
                LexStep {
                    token: match first_char {
                        '+' | '-' => Token::AddOp(first_char),
                        '*' | '/' | '%' => Token::MultOp(first_char),
                        ';' => Token::Semicolon,
                        '=' => Token::Assign,
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

fn get_ident(ident_and_remaining: &str) -> LexStep {
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
    // Confim identifier is not a reserved keyword
    LexStep {
        token: match identifier {
            "print" => Token::Print,
            "repeat" => Token::Repeat,
            "quit" => Token::Quit, // not technically a token, but a command to the lexer to exit.
            _ => result.token,
        },
        remaining_to_lex: result.remaining_to_lex,
    }
}

fn get_number(number_and_remaining: &str) -> LexStep {
    let mut number_type = Token::Int(0);
    let mut remaining = "";
    let mut number = "";
    let number_and_enumeration = number_and_remaining.chars().enumerate();
    for (number_end, character) in number_and_enumeration {
        match character {
            '.' => {
                if number_type == Token::Int(0) {
                    number_type = Token::Float(0.0);
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
    if number_type == Token::Float(0.0) {
        LexStep {
            token: parse_float(number),
            remaining_to_lex: remaining,
        }
    } else {
        LexStep {
            token: parse_int(number),
            remaining_to_lex: remaining,
        }
    }
}

fn parse_float(number: &str) -> Token {
    Token::Float(number.parse().expect("Number should be a parseable float"))
}

fn parse_int(number: &str) -> Token {
    Token::Int(number.parse().expect("Number should be a parseable int"))
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
    use super::*;
    use crate::Token::*;

    #[test]
    fn it_gets_ints() {
        let result = get_number("1234");
        assert_eq!(result.token, Int(1234));
    }

    #[test]
    fn it_lexes_ints() {
        let result = get_next_token("1234").unwrap();
        assert_eq!(result.token, Int(1234));
    }

    #[test]
    fn it_gets_floats() {
        let result = get_number("1234.5678");
        assert_eq!(result.token, Float(1234.5678));
    }

    #[test]
    fn it_gets_idents() {
        let result = get_ident("test$_ABC");
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
        let response = get_ident("test \n nextword");
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
                token: Int(1234),
                remaining_to_lex: ";"
            }
        )
    }

    #[test]
    fn it_lexes_two_token_string() {
        let response = lex_string("1234;");
        assert_eq!(response, vec![Int(1234), Semicolon])
    }

    #[test]
    fn it_lexes_identifier_and_semicolon() {
        let response = lex_string("firstvar;");
        assert_eq!(response, vec![Ident("firstvar"), Semicolon])
    }

    #[test]
    fn it_ignores_whitespace() {
        let result = get_next_token("   \n\n  test  \n   nextword").unwrap();
        assert_eq!(result.token, Ident("test"));
    }

    #[test]
    fn it_lexes_test_line() {
        let response = lex_string("firstvar = 123");
        assert_eq!(response, vec![Ident("firstvar"), Assign, Int(123)])
    }

    #[test]
    fn it_skips_invalid_chars() {
        let result = get_next_token(".523.3").unwrap();
        assert_eq!(result.token, Invalid('.'))
    }

    #[test]
    fn it_allows_ident_with_number() {
        let result = get_ident("var3");
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
        get_ident("3var");
    }
}
