use assert_cmd::Command; // Add methods on commands
use predicates::prelude::*; // Used for writing assertions

#[test]
fn lexer_prints_no_parse_errors() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("rustylisp")?;

    cmd.arg("-l").write_stdin("(\nquit");

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Parsing Error").not());

    Ok(())
}

#[test]
fn add_prints_no_warnings() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("rustylisp")?;

    cmd.arg("-e").write_stdin("(add 1 2 3)\nquit");

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Warn").not());

    Ok(())
}

#[test]
fn mult_prints_no_warnings() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("rustylisp")?;

    cmd.arg("-e").write_stdin("(mult 1 2 3)\nquit");

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Warn").not());

    Ok(())
}
