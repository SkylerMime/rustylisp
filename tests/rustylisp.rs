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
