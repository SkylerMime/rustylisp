use std::str::FromStr;
extern crate pretty_env_logger;

use clap::{Args, Parser};

use core::fmt;
use std::{
    fmt::Debug,
    fs,
    io::{self, stdout, Write},
};

use strum_macros::{Display, EnumString};

use crate::parser::{parse_tokens, print_abstract_syntax_tree};
use crate::{evaluator::eval, parser::ParseResult};

#[derive(Parser, Debug)]
#[command(name = "RustyLisp")]
#[command(version, about="Interpreter for a Lisp-like language", long_about=None)]
pub struct CliArgs {
    #[command(flatten)]
    mode: Mode,

    /// file to read from
    pub filepath: Option<String>,
}

#[derive(Args, Debug)]
#[group(required = true, multiple = true)]
struct Mode {
    /// lex input
    #[arg(short, long, group = "mode")]
    pub lex: bool,
    /// parse input
    #[arg(short, long, group = "mode")]
    pub parse: bool,
    /// evaluate input
    #[arg(short, long, group = "mode")]
    pub eval: bool,
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
    Symbol(&'a str),
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

pub fn read_file(args: CliArgs) {
    if let Some(ref path) = args.filepath {
        if let Ok(contents) = fs::read_to_string(path.clone()) {
            let mut buffer = String::new();
            let mut lines = contents.lines();

            while let Some(line) = lines.next() {
                buffer += line;
                match process_command_buffer(&args, &buffer) {
                    ProcessResult::Processed => {
                        // Clear the buffer for the next command
                        buffer = String::new();
                    }
                    ProcessResult::Incomplete => {
                        // continue
                    }
                    ProcessResult::Quit => {
                        println!("Warn: Ended file reading early due to quit command");
                        break;
                    }
                }
            }
        } else {
            println!("Fatal: File {} not found", path);
        }
    } else {
        println!("Fatal: Program mode is not 'read from file'")
    }
}

pub fn read_user_input(args: CliArgs) {
    // TODO: Respond to Backspace

    print_prompt();
    stdout()
        .flush()
        .expect("Next command should have been writable");
    let mut buffer = String::new();
    loop {
        io::stdin()
            .read_line(&mut buffer)
            .expect("User input should have been readable");
        match process_command_buffer(&args, &buffer) {
            ProcessResult::Processed => {
                // Clear the buffer for the next command
                buffer = String::new();
            }
            ProcessResult::Incomplete => {
                print_multiline_prompt();
                stdout()
                    .flush()
                    .expect("Next buffer should have been writable");
            }
            ProcessResult::Quit => {
                break;
            }
        }
    }
}

enum ProcessResult {
    Processed,
    Incomplete,
    Quit,
}

fn process_command_buffer(args: &CliArgs, buffer: &str) -> ProcessResult {
    // TODO: No early returns

    let tokens = lex_string(buffer);
    if args.mode.lex {
        match print_tokens(&tokens) {
            Ok(()) => {
                println!();
            }
            Err(_) => {
                println!("quitting...");
                return ProcessResult::Quit;
            }
        }
    } else {
        if let Err(_) = check_for_quit(&tokens) {
            println!("quitting...");
            return ProcessResult::Quit;
        }
    }

    if args.mode.parse || args.mode.eval {
        match parse_tokens(&mut tokens.iter().peekable()) {
            Ok(root) => {
                if args.mode.parse {
                    print_abstract_syntax_tree(root.clone(), 0);
                    println!();
                }
                if args.mode.eval {
                    match eval(&root) {
                        Ok(parse_result) => println!("=> {}", parse_result),
                        Err(msg) => println!("Evaluation Error: {}", msg),
                    }
                }
            }
            Err(error) => match error {
                ParseResult::Incomplete => {
                    return ProcessResult::Incomplete;
                }
                ParseResult::Err(error_message) => {
                    println!("Parsing Error: {}", error_message);
                }
            },
        }
    }
    print_prompt();
    stdout()
        .flush()
        .expect("Next buffer should have been writable");
    ProcessResult::Processed
}

fn print_prompt() {
    print!("\nrustylisp $ ");
}

fn print_multiline_prompt() {
    print!("........... ")
}

fn lex_string(mut remaining_string: &str) -> Vec<Token> {
    let mut tokens = vec![];
    while let Some(result) = get_next_token(remaining_string) {
        if result.token == Token::Symbol("") {
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
            'a'..='z' | 'A'..='Z' | '_' | '$' => get_symbol_or_function(remaining_string),
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
fn get_symbol_or_function(ident_and_remaining: &str) -> LexStep {
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
                    token: Token::Symbol(identifier),
                    remaining_to_lex: "",
                };
                break;
            }
            'a'..='z' | 'A'..='Z' | '_' | '$' | '0'..='9' => (),
            _ => {
                identifier = &ident_and_remaining[..identifier_end];
                result = LexStep {
                    token: Token::Symbol(identifier),
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
        let result = get_symbol_or_function("test$_ABC");
        assert_eq!(
            result,
            LexStep {
                token: Symbol("test$_ABC"),
                remaining_to_lex: ""
            }
        );
    }

    #[test]
    fn it_gets_idents_with_whitespace() {
        let response = get_symbol_or_function("test \n nextword");
        assert_eq!(
            response,
            LexStep {
                token: Symbol("test"),
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
        assert_eq!(response, vec![Symbol("firstvar"), RightParen])
    }

    #[test]
    fn it_ignores_whitespace() {
        let result = get_next_token("   \n\n  test  \n   nextword").unwrap();
        assert_eq!(result.token, Symbol("test"));
    }

    #[test]
    fn it_lexes_test_line() {
        let response = lex_string("(add firstvar 123)");
        assert_eq!(
            response,
            vec![
                LeftParen,
                Func(FuncType::NAry(NAry::Add)),
                Symbol("firstvar"),
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
        let result = get_symbol_or_function("var3");
        assert_eq!(
            result,
            LexStep {
                token: Symbol("var3"),
                remaining_to_lex: ""
            }
        )
    }

    #[test]
    #[should_panic]
    fn ident_starting_with_number_invalid() {
        get_symbol_or_function("3var");
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
