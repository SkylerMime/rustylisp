use core::fmt;
use std::{fmt::Debug, fs, str::Chars};

#[derive(Debug, PartialEq)]
enum Token {
    Int(u32),
    Float(f32),
    AddOp(char),
    MultOp(char),
    Semicolon,
    Ident(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{:?}>", self)
    }
}

fn main() {
    read_file("testfiles/input.txt");
}

fn read_file(path: &str) {
    // TODO
    let tokens = lex_string("1234".to_string());
    print_tokens(tokens);
}

fn lex_string(mut remaining_string: String) -> Vec<Token> {
    let mut tokens = vec![];
    loop {
        let result = get_next_token(remaining_string);
        remaining_string = if let Some((token, string)) = result {
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
            '0'..='9' => get_number(remaining_chars),
            _ => get_ident(remaining_chars),
        })
    } else {
        None
    }
}

fn get_ident(ident_and_remaining: Chars) -> (Token, String) {
    for (identifier_end, character) in ident_and_remaining.clone().enumerate() {
        match character {
            'a'..='z' | 'A'..='Z' | '_' | '$' => (),
            _ => {
                let mut ident_and_remaining = ident_and_remaining.collect::<String>();
                let remaining = ident_and_remaining.split_off(identifier_end);
                return (Token::Ident(ident_and_remaining), remaining);
            }
        };
    }
    (
        Token::Ident(ident_and_remaining.collect::<String>()),
        String::new(),
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
                    panic!("Valid numbers should have no more than one decimal point")
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
        let (result, _) = get_ident("test$_ABC".to_string().chars());
        assert_eq!(result, Ident("test$_ABC".to_string()));
    }

    #[test]
    fn it_lexes_idents_with_whitespace() {
        let result = get_ident("test \n nextword".to_string().chars());
        assert_eq!(
            result,
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
}
