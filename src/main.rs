use std::{
    fs,
    io::{self, Write},
    path::PathBuf,
};

use chimera::parser::Parser as ChimeraParser;
use chimera::{lexer::SyntaxKind, parser::IgnoreTrivia};
use clap::{Parser, Subcommand};
use logos::Logos;

use miette::{IntoDiagnostic, Result as MietteResult, WrapErr};

#[derive(Debug, Subcommand)]
enum ChimeraCommand {
    Tokenize { filename: PathBuf },
    Parse { filename: PathBuf },
    Run { filename: PathBuf },
    Repl,
}
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: ChimeraCommand,
}

fn main() -> MietteResult<()> {
    let cmd = Args::parse();
    match cmd.command {
        ChimeraCommand::Tokenize { filename } => {
            let file_contents = fs::read_to_string(&filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("reading '{}' failed", filename.display()))?;

            let lexer = SyntaxKind::lexer(file_contents.as_str());

            for token in lexer {
                let token = match token {
                    Ok(tokenkind) => tokenkind,
                    Err(e) => {
                        eprintln!("{e:?}");
                        continue;
                    }
                };
                println!("{token:?}");
            }
            println!("EOF  null");
            Result::Ok(())
        }
        ChimeraCommand::Parse { .. } => todo!(),
        ChimeraCommand::Run { .. } => todo!(),
        ChimeraCommand::Repl => {
            let stdin = io::stdin();
            let mut stdout = io::stdout();

            let mut input = String::new();

            loop {
                write!(stdout, "→ ").unwrap();
                stdout.flush().unwrap();

                stdin.read_line(&mut input).unwrap();

                let parse = ChimeraParser::new(&input).parse::<IgnoreTrivia>();
                println!("{}", parse.debug_tree());

                input.clear();
            }
        }
    }
}
