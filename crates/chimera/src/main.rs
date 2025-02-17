use parser::parser::{IgnoreTrivia, Parser as ChimeraParser};
use std::io::{Result as IoResult, Write, stdin, stdout};

fn main() -> IoResult<()> {
    let stdin = stdin();
    let mut stdout = stdout();
    let mut input = String::new();

    loop {
        write!(stdout, "→ ")?;
        stdout.flush()?;

        stdin.read_line(&mut input)?;

        let parse = ChimeraParser::new(&input).parse::<IgnoreTrivia>();
        println!("{:?}", parse);
        if let Err(err) = parse {
            eprintln!("{err:}");
        } else {
            println!("{}", parse.expect("expected parse").debug_tree());
        }

        input.clear();
    }
}
