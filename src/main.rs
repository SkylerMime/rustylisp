use core::fmt;
use std::{fmt::Debug, fs};

extern crate pretty_env_logger;
#[macro_use]
extern crate log;

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
}

impl<'a> fmt::Display for Token<'a> {
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
    let tokens = lex_string(&contents);
    print_tokens(tokens);
}

fn lex_string(mut remaining_string: &str) -> Vec<Token> {
    let mut tokens = vec![];
    // TODO: Refactor without the possibility of an infinite loop
    loop {
        let result = get_next_token(remaining_string);
        remaining_string = if let Some((token, string)) = result {
            if token == Token::Ident("") {
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

fn get_next_token(remaining_string: &str) -> Option<(Token, &str)> {
    let remaining_string = remaining_string.trim_start();
    let mut remaining_chars = remaining_string.chars();
    if let Some(first_char) = remaining_chars.nth(0) {
        Some(match first_char {
            '+' | '-' => (Token::AddOp(first_char), remaining_chars.as_str()),
            '*' | '/' | '%' => (Token::MultOp(first_char), remaining_chars.as_str()),
            ';' => (Token::Semicolon, remaining_chars.as_str()),
            '=' => (Token::Assign, remaining_chars.as_str()),
            '(' => (Token::LeftParen, remaining_chars.as_str()),
            ')' => (Token::RightParen, remaining_chars.as_str()),
            '0'..='9' => get_number(remaining_string),
            'a'..='z' | 'A'..='Z' | '_' | '$' => get_ident(remaining_string),
            _ => (Token::Invalid(first_char), remaining_chars.as_str()),
        })
    } else {
        None
    }
}

fn get_ident(ident_and_remaining: &str) -> (Token, &str) {
    let mut remaining_chars = ident_and_remaining.chars().enumerate();
    if let Some((_, 'a'..='z' | 'A'..='Z' | '_' | '$')) = remaining_chars.next() {
        ();
    } else {
        panic!("Not a valid identifier");
    }
    let mut identifier = "";
    let mut result = (Token::Default, ident_and_remaining);
    for (identifier_end, character) in remaining_chars {
        match character {
            'a'..='z' | 'A'..='Z' | '_' | '$' | '0'..='9'
                if identifier_end == ident_and_remaining.len() - 1 =>
            {
                result = (Token::Ident(&ident_and_remaining[..identifier_end + 1]), "");
                break;
            }
            'a'..='z' | 'A'..='Z' | '_' | '$' | '0'..='9' => (),
            _ => {
                let ident_and_remaining = ident_and_remaining;
                identifier = &ident_and_remaining[..identifier_end];
                result = (
                    Token::Ident(identifier),
                    &ident_and_remaining[identifier_end..],
                );
                break;
            }
        };
    }
    // Confim identifier is not a reserved keyword
    (
        match identifier {
            "print" => Token::Print,
            "repeat" => Token::Repeat,
            _ => result.0,
        },
        result.1,
    )
}

fn get_number(number_and_remaining: &str) -> (Token, &str) {
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
                    return (Token::Invalid('.'), remaining);
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
        (parse_float(number), remaining)
    } else {
        (parse_int(number), remaining)
    }
}

fn parse_float(number: &str) -> Token {
    Token::Float(number.parse().expect("Number should be a parseable float"))
}

fn parse_int(number: &str) -> Token {
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
    fn it_gets_ints() {
        let (result, _) = get_number("1234");
        assert_eq!(result, Int(1234));
    }

    #[test]
    fn it_lexes_ints() {
        let (result, _) = get_next_token("1234").unwrap();
        assert_eq!(result, Int(1234));
    }

    #[test]
    fn it_gets_floats() {
        let (result, _) = get_number("1234.5678");
        assert_eq!(result, Float(1234.5678));
    }

    #[test]
    fn it_gets_idents() {
        let result = get_ident("test$_ABC");
        assert_eq!(result, (Ident("test$_ABC"), ""));
    }

    #[test]
    fn it_gets_idents_with_whitespace() {
        let response = get_ident("test \n nextword");
        assert_eq!(response, (Ident("test"), " \n nextword"))
    }

    #[test]
    fn it_lexes_first_token_in_two_steps() {
        let response = get_next_token("1234;").unwrap();
        assert_eq!(response, (Int(1234), ";"))
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
        let (result, _) = get_next_token("   \n\n  test  \n   nextword").unwrap();
        assert_eq!(result, Ident("test"));
    }

    #[test]
    fn it_lexes_test_line() {
        let response = lex_string("firstvar = 123");
        assert_eq!(response, vec![Ident("firstvar"), Assign, Int(123)])
    }

    #[test]
    fn it_skips_invalid_chars() {
        let (result, _) = get_next_token(".523.3").unwrap();
        assert_eq!(result, Invalid('.'))
    }

    #[test]
    fn it_allows_ident_with_number() {
        let result = get_ident("var3");
        assert_eq!(result, (Ident("var3"), ""))
    }

    #[test]
    #[should_panic]
    fn ident_starting_with_number_invalid() {
        get_ident("3var");
    }
}
