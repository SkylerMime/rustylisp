mod evaluator;
mod lexer;
mod parser;

#[macro_use]
extern crate log;

use clap::Parser;
use lexer::read_file;
use lexer::read_user_input;

use lexer::CliArgs;

fn main() {
    pretty_env_logger::init();
    let program_mode = CliArgs::parse();

    if program_mode.filepath.is_some() {
        read_file(program_mode)
    } else {
        read_user_input(program_mode)
    }
}
