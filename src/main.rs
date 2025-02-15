mod lexer;
mod parser;

#[macro_use]
extern crate log;

use lexer::print_help_message;
use lexer::read_file;
use lexer::read_lines;
use lexer::InputMode;
use lexer::ProgramMode;
use std::env;

fn main() {
    pretty_env_logger::init();
    if let Some(program_mode) = ProgramMode::build(env::args()) {
        match program_mode.input_mode {
            InputMode::UserInput => read_lines(program_mode),
            InputMode::FilePath(_) => read_file(program_mode),
        };
    } else {
        print_help_message();
    }
}
