mod lexer;

#[macro_use]
extern crate log;

use lexer::read_file;
use lexer::read_lines;
use lexer::InputMode;
use std::env;

fn main() {
    pretty_env_logger::init();
    match InputMode::build(env::args()) {
        InputMode::UserInput => read_lines(),
        InputMode::FilePath(file_path) => read_file(file_path),
    };
}
