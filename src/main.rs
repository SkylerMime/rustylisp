use lexer::read_file;
use lexer::read_lines;
use lexer::ProgramMode;
use std::env;

fn main() {
    pretty_env_logger::init();
    match ProgramMode::build(env::args()) {
        ProgramMode::UserInput => read_lines(),
        ProgramMode::FilePath(file_path) => read_file(file_path),
    };
}
