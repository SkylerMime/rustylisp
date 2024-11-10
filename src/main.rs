use core::fmt;
use std::{fmt::Debug, fs, str::Chars};

extern crate pretty_env_logger;
#[macro_use]
extern crate log;

#[derive(Debug, PartialEq)]
enum Token {
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
    Ident(String),
    Invalid(char),
    Default,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{:?}>", self)
    }
}

fn main() {
    pretty_env_logger::init();
    read_file("testfiles/input.txt");
}

fn read_file(path: &str) {
    let contents = fs::read_to_string(path).expect("Should have been able to read the file");
    let tokens = lex_string(contents);
    print_tokens(tokens);
}

fn lex_string(mut remaining_string: String) -> Vec<Token> {
    let mut tokens = vec![];
    // TODO: Refactor without the possibility of an infinite loop
    loop {
        let result = get_next_token(remaining_string);
        remaining_string = if let Some((token, string)) = result {
            if token == Token::Ident("".to_string()) {
                error!("Infinite loop. Remaining: {}", string);
                break;
            }
            debug!("Adding token: {}", token);
            tokens.push(token);
            string
        } else {
            break;
        }
    }
    tokens
}

fn get_next_token(remaining_string: String) -> Option<(Token, String)> {
    let remaining_chars = remaining_string.trim_start().chars();
    let mut last_chars = remaining_chars.clone();
    if let Some(first_char) = last_chars.next() {
        Some(match first_char {
            '+' | '-' => (Token::AddOp(first_char), last_chars.collect::<String>()),
            '*' | '/' | '%' => (Token::MultOp(first_char), last_chars.collect::<String>()),
            ';' => (Token::Semicolon, last_chars.collect::<String>()),
            '=' => (Token::Assign, last_chars.collect::<String>()),
            '(' => (Token::LeftParen, last_chars.collect::<String>()),
            ')' => (Token::RightParen, last_chars.collect::<String>()),
            '0'..='9' => get_number(remaining_chars),
            'a'..='z' | 'A'..='Z' | '_' | '$' => get_ident(remaining_chars),
            _ => (Token::Invalid(first_char), last_chars.collect::<String>()),
        })
    } else {
        None
    }
}

fn get_ident(ident_and_remaining: Chars) -> (Token, String) {
    // TODO: This needs major refactoring
    let mut identifier = String::new();
    let mut result = (
        Token::Default,
        ident_and_remaining.clone().collect::<String>(),
    );
    for (identifier_end, character) in ident_and_remaining.clone().enumerate() {
        match character {
            'a'..='z' | 'A'..='Z' | '_' | '$' | _ if identifier_end == result.1.len() - 1 => {
                result = (
                    Token::Ident(ident_and_remaining.collect::<String>()),
                    String::new(),
                );
                break;
            }
            'a'..='z' | 'A'..='Z' | '_' | '$' => (),
            _ => {
                let mut ident_and_remaining = ident_and_remaining.collect::<String>();
                let remaining = ident_and_remaining.split_off(identifier_end);
                identifier = ident_and_remaining;
                result = (Token::Ident(identifier.clone()), remaining);
                break;
            }
        };
    }
    // Confim identifier is not a reserved keyword
    (
        match identifier.as_str() {
            "print" => Token::Print,
            "repeat" => Token::Repeat,
            _ => result.0,
        },
        result.1,
    )
}

fn get_number(number_and_remaining: Chars) -> (Token, String) {
    let mut number_type = Token::Int(0);
    let mut remaining = String::new();
    let number_and_enumeration = number_and_remaining.clone().enumerate();
    let mut number_and_remaining = number_and_remaining.collect::<String>();
    for (number_end, character) in number_and_enumeration {
        match character {
            '.' => {
                if number_type == Token::Int(0) {
                    number_type = Token::Float(0.0);
                } else {
                    error!(
                        "Valid numbers should have no more than one decimal point, token invalid"
                    );
                    return (Token::Invalid('.'), remaining);
                }
            }
            '0'..='9' => (),
            _ => {
                remaining = number_and_remaining.split_off(number_end);
                break;
            }
        }
    }
    if number_type == Token::Float(0.0) {
        (parse_float(number_and_remaining), remaining)
    } else {
        (parse_int(number_and_remaining), remaining)
    }
}

fn parse_float(number: String) -> Token {
    Token::Float(number.parse().expect("Number should be a parseable float"))
}

fn parse_int(number: String) -> Token {
    Token::Int(number.parse().expect("Number should be a parseable int"))
}

fn print_tokens(tokens: Vec<Token>) {
    for token in tokens {
        println!("{}", token);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Token::*;

    #[test]
    fn it_lexes_ints() {
        let (result, _) = get_next_token("1234".to_string()).unwrap();
        assert_eq!(result, Int(1234));
    }

    #[test]
    fn it_lexes_floats() {
        let (result, _) = get_number("1234.5678".to_string().chars());
        assert_eq!(result, Float(1234.5678));
    }

    #[test]
    fn it_lexes_idents() {
        let result = get_ident("test$_ABC".to_string().chars());
        assert_eq!(result, (Ident("test$_ABC".to_string()), String::new()));
    }

    #[test]
    fn it_lexes_idents_with_whitespace() {
        let response = get_ident("test \n nextword".to_string().chars());
        assert_eq!(
            response,
            (Ident("test".to_string()), " \n nextword".to_string())
        )
    }

    #[test]
    fn it_lexes_first_token_in_two_steps() {
        let response = get_next_token("1234;".to_string()).unwrap();
        assert_eq!(response, (Int(1234), ";".to_string()))
    }

    #[test]
    fn it_lexes_two_token_string() {
        let response = lex_string("1234;".to_string());
        assert_eq!(response, vec![Int(1234), Semicolon])
    }

    #[test]
    fn it_ignores_whitespace() {
        let (result, _) = get_next_token("   \n\n  test  \n   nextword".to_string()).unwrap();
        assert_eq!(result, Ident("test".to_string()));
    }

    #[test]
    fn it_lexes_test_line() {
        let response = lex_string("firstvar = 123".to_string());
        assert_eq!(
            response,
            vec![Ident("firstvar".to_string()), Assign, Int(123)]
        )
    }

    #[test]
    fn it_skips_invalid_chars() {
        let (result, _) = get_next_token(".523.3".to_string()).unwrap();
        assert_eq!(result, Invalid('.'))
    }

    // TODO: Grammar allows for identifiers with numbers at the end
}
