use core::fmt;
use std::fmt::Debug;

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
    let mut token;
    let mut tokens = vec![];
    loop {
        (token, remaining_string) = get_next_token(remaining_string);
        tokens.push(token);
        if remaining_string.len() == 0 {
            break;
        }
    }
    tokens
}

fn get_next_token(remaining_string: String) -> (Token, String) {
    let first_char = remaining_string
        .chars()
        .nth(0)
        .expect("String should not be empty");
    let last_chars = remaining_string.clone().split_off(1);
    match first_char {
        '+' | '-' => (Token::AddOp(first_char), last_chars),
        '*' | '/' | '%' => (Token::MultOp(first_char), last_chars),
        ';' => (Token::Semicolon, last_chars),
        '0'..='9' => get_number(remaining_string),
        _ => get_ident(remaining_string),
    }
}

fn get_ident(mut ident_and_remaning: String) -> (Token, String) {
    for (index, character) in ident_and_remaning.chars().enumerate() {
        match character {
            'a'..='z' | 'A'..='Z' | '_' | '$' => (),
            _ => {
                let identifier = ident_and_remaning.split_off(index);
                return (Token::Ident(identifier), ident_and_remaning);
            }
        };
    }
    (Token::Ident(ident_and_remaning), String::new())
}

fn get_number(mut number_and_remaining: String) -> (Token, String) {
    let mut number_type = Token::Int(0);
    let mut remaining = String::new();
    for (index, character) in number_and_remaining.chars().enumerate() {
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
                remaining = number_and_remaining.split_off(index);
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

fn print_tokens(tokens: Vec<Token>) -> Vec<String> {
    let mut lines = Vec::new();

    for token in tokens {
        lines.push(format!("{}", token));
    }

    lines
}

#[cfg(test)]
mod tests {
    use std::fs;

    use super::*;
    use crate::Token::*;

    #[test]
    fn it_lexes_ints() {
        let (result, _) = get_next_token("1234".to_string());
        assert_eq!(result, Int(1234));
    }

    #[test]
    fn it_lexes_floats() {
        let (result, _) = get_number("1234.5678".to_string());
        assert_eq!(result, Float(1234.5678));
    }

    #[test]
    fn it_lexes_idents() {
        let (result, _) = get_ident("test$_ABC".to_string());
        assert_eq!(result, Ident("test$_ABC".to_string()));
    }

    #[test]
    fn it_lexes_first_token_in_two_steps() {
        let response = get_next_token("1234;".to_string());
        assert_eq!(response, (Int(1234), ";".to_string()))
    }

    #[test]
    fn it_lexes_two_token_string() {
        let response = lex_string("1234;".to_string());
        assert_eq!(response, vec![Int(1234), Semicolon])
    }

    #[test]
    fn it_prints_an_int() {
        let lines = print_tokens(vec![Int(1234)]);
        assert_eq!("<Int(1234)>", lines[0]);
    }
}
