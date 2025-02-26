use std::str::FromStr;
extern crate pretty_env_logger;

use core::fmt;
use std::{
    fmt::Debug,
    fs,
    io::{self, stdout, Write},
};

use strum_macros::{Display, EnumString};

use crate::evaluator::eval;
use crate::parser::{parse_tokens, print_abstract_syntax_tree};

#[derive(Debug, PartialEq)]
pub enum InputMode {
    UserInput,
    FilePath(String),
}

#[derive(Debug, PartialEq)]
pub struct ProgramMode {
    pub input_mode: InputMode,
    pub lex: bool,
    pub parse: bool,
    pub eval: bool,
}

impl ProgramMode {
    pub fn build(mut args: impl Iterator<Item = String>) -> Option<ProgramMode> {
        let mut program_mode = ProgramMode {
            input_mode: InputMode::UserInput,
            lex: false,
            parse: false,
            eval: false,
        };
        // first arg should be the program name
        if let None = args.next() {
            None
        } else {
            while let Some(arg) = args.next() {
                match arg.as_str() {
                    "-l" | "--lex" => program_mode.lex = true,
                    "-p" | "--parse" => program_mode.parse = true,
                    "-e" | "--eval" => program_mode.eval = true,
                    filepath => {
                        if program_mode.input_mode == InputMode::UserInput {
                            program_mode.input_mode = InputMode::FilePath(filepath.to_string())
                        } else {
                            return None;
                        }
                    }
                }
            }
            if program_mode.any_option_selected() {
                Some(program_mode)
            } else {
                None
            }
        }
    }

    fn any_option_selected(&self) -> bool {
        return self.lex || self.parse || self.eval;
    }
}

#[derive(Debug, PartialEq)]
struct LexStep<'a> {
    token: Token<'a>,
    remaining_to_lex: &'a str,
}

#[derive(Debug, PartialEq, Clone)]
pub enum AstNumber {
    Int(i32),
    Double(f32),
    NAN,
}

impl fmt::Display for AstNumber {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let inner = match self {
            AstNumber::Int(inner) => inner.to_string(),
            AstNumber::Double(inner) => inner.to_string(),
            AstNumber::NAN => "nan".to_string(),
        };
        write!(f, "{}", inner)
    }
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

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{:?}>", self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum FuncType {
    Unary(Unary),
    Binary(Binary),
    NAry(NAry),
}

#[derive(Debug, PartialEq, EnumString, Clone)]
#[strum(serialize_all = "lowercase")]
pub enum Unary {
    Neg,
    Abs,
    Exp,
    Exp2,
    Log,
    Sqrt,
    Cbrt,
}

#[derive(Debug, PartialEq, EnumString, Clone)]
#[strum(serialize_all = "lowercase")]
pub enum Binary {
    Sub,
    Div,
    Remainder,
    Pow,
}

#[derive(Debug, PartialEq, EnumString, Clone, Display)]
#[strum(serialize_all = "lowercase")]
pub enum NAry {
    Add,
    Mult,
    Hypot,
    Max,
    Min,
}

pub trait NoArgs {
    fn no_args(&self) -> AstNumber;
}

impl NoArgs for NAry {
    fn no_args(&self) -> AstNumber {
        println!("Warn: No inputs for {:?}, returning zero", self);
        match self {
            NAry::Add => AstNumber::Int(0),
            NAry::Mult => AstNumber::Int(1),
            NAry::Hypot => AstNumber::Double(0.0),
            NAry::Max => AstNumber::NAN,
            NAry::Min => AstNumber::NAN,
        }
    }
}

pub fn read_file(program_mode: ProgramMode) {
    if let InputMode::FilePath(ref path) = program_mode.input_mode {
        if let Ok(contents) = fs::read_to_string(path.clone()) {
            for line in contents.lines() {
                if let Err(_) = process_line(&program_mode, line) {
                    println!("Warn: Ended file reading early due to quit command");
                    break;
                }
            }
        } else {
            println!("Fatal: File {} not found", path);
        }
    } else {
        println!("Fatal: Program mode is not 'read from file'")
    }
}

pub fn read_lines(program_mode: ProgramMode) {
    print!("parser $ ");
    stdout()
        .flush()
        .expect("Next line should have been writable");
    loop {
        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .expect("User input should have been readable");
        if let Err(_) = process_line(&program_mode, input.as_str()) {
            break;
        }
    }
}

fn process_line(program_mode: &ProgramMode, line: &str) -> Result<(), ()> {
    let tokens = lex_string(line);
    if program_mode.lex {
        match print_tokens(&tokens) {
            Ok(()) => {
                println!();
            }
            Err(_) => {
                println!("quitting...");
                return Err(());
            }
        }
    } else {
        if let Err(_) = check_for_quit(&tokens) {
            println!("quitting...");
            return Err(());
        }
    }

    match parse_tokens(&mut tokens.iter().peekable()) {
        Ok(root) => {
            if program_mode.parse {
                print_abstract_syntax_tree(root.clone(), 0);
                println!();
            }
            if program_mode.eval {
                match eval(&root) {
                    Ok(parse_result) => println!("=> {}", parse_result),
                    Err(msg) => println!("Evaluation Error: {}", msg),
                }
            }
        }
        Err(error_message) => {
            println!("Parsing Error: {}", error_message);
        }
    }
    print!("\nparser $ ");
    stdout()
        .flush()
        .expect("Next line should have been writable");
    Ok(())
}

pub fn print_help_message() {
    println!("Usage: parser [-l | --lex] [-p | --parse] [-e | --eval] [filepath]")
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
            '-' | '0'..='9' => get_number(remaining_string),
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
    // Confirm the first character is legal for the start of an identifier
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
                identifier = &ident_and_remaining[..identifier_end + 1];
                result = LexStep {
                    token: Token::Ident(identifier),
                    remaining_to_lex: "",
                };
                break;
            }
            'a'..='z' | 'A'..='Z' | '_' | '$' | '0'..='9' => (),
            _ => {
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
        // TODO: Try rewriting as match expression
        token: if let Ok(func_token) = Unary::from_str(identifier) {
            Token::Func(FuncType::Unary(func_token))
        } else if let Ok(func_token) = Binary::from_str(identifier) {
            Token::Func(FuncType::Binary(func_token))
        } else if let Ok(func_token) = NAry::from_str(identifier) {
            Token::Func(FuncType::NAry(func_token))
        } else if identifier == "quit" {
            Token::Quit
        } else {
            result.token
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
            '-' => {
                if number_end != 0 {
                    // TODO: Use Result instead of error! log
                    error!("Negative signs can only occur at the start of numbers, token invalid");
                    return LexStep {
                        token: Token::Invalid('-'),
                        remaining_to_lex: remaining,
                    };
                }
            }
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

fn print_tokens<'a>(tokens: &Vec<Token>) -> Result<(), Token<'a>> {
    for_token(tokens, |token| println!("{}", token))
}

fn check_for_quit<'a>(tokens: &Vec<Token>) -> Result<(), Token<'a>> {
    for_token(&tokens, |_| ())
}

fn for_token<'a>(tokens: &Vec<Token>, action: fn(&Token) -> ()) -> Result<(), Token<'a>> {
    for token in tokens {
        if *token == Token::Quit {
            return Err(Token::Quit);
        } else {
            action(token);
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
    fn it_lexes_negative_ints() {
        let result = get_next_token("-3").unwrap();
        assert_eq!(result.token, Number(Int(-3)));
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
                Func(FuncType::NAry(NAry::Add)),
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

    #[test]
    fn it_gets_flags() {
        let args = vec!["parser".to_string(), "-l".to_string(), "-p".to_string()].into_iter();
        let configuration = ProgramMode::build(args);
        assert_eq!(
            configuration,
            Some(ProgramMode {
                input_mode: InputMode::UserInput,
                lex: true,
                parse: true,
                eval: false,
            })
        )
    }

    #[test]
    fn it_ends_with_bad_args() {
        let args = vec![
            "parser".to_string(),
            "filepath".to_string(),
            "badarg".to_string(),
        ]
        .into_iter();
        let configuration = ProgramMode::build(args);
        assert_eq!(configuration, None)
    }

    #[test]
    fn it_ends_with_no_options() {
        let args = vec!["parser".to_string()].into_iter();
        let configuration = ProgramMode::build(args);
        assert_eq!(configuration, None)
    }

    #[test]
    fn it_reads_functions() {
        let result = lex_string("add");
        assert_eq!(result, vec![Func(FuncType::NAry(NAry::Add))])
    }

    #[test]
    fn it_reads_quit() {
        let result = lex_string("quit");
        assert_eq!(result, vec![Quit])
    }
}
