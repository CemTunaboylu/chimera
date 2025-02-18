use parser::parse_behaviors::IgnoreTrivia;
use parser::parser::Parser as ChimeraParser;
use std::io::{Write, stdin, stdout};

use miette::{IntoDiagnostic, Report, Result as MietteResult};

fn main() -> MietteResult<()> {
    let stdin = stdin();
    let mut stdout = stdout();
    let mut input = String::new();

    loop {
        write!(stdout, "→ ").into_diagnostic()?;
        stdout.flush().into_diagnostic()?;

        stdin.read_line(&mut input).into_diagnostic()?;

        let parse = ChimeraParser::new(&input).parse::<IgnoreTrivia>();
        if parse.errors.is_empty() {
            println!("{}", parse.debug_tree());
        } else {
            for err in parse.errors {
                let report: Report = err.into();
                println!("{:?}", report);
            }
        }

        input.clear();
    }
}
