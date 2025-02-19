use miette::{IntoDiagnostic, Report, Result as MietteResult};
use parser::{
    ast::{Expr, Root, Stmt},
    cst::ConcreteSyntaxTree,
    parse_behaviors::IgnoreTrivia,
    parser::Parser as ChimeraParser,
};
use std::io::{Write, stdin, stdout};
use std::path::PathBuf;

use clap::{Parser, Subcommand};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Commands,
}
#[derive(Subcommand, Debug)]
// TODO: add options for parser behavior
enum Commands {
    Tokenize { filename: PathBuf },
    Parse { filename: PathBuf },
    Run { filename: PathBuf },
    CST,
    AST,
}

fn main() -> MietteResult<()> {
    let args = Args::parse();

    let stdin = stdin();
    let mut stdout = stdout();
    let mut input = String::new();

    let display = match args.command {
        Commands::AST => display_as_ast,
        Commands::CST => display_as_cst,
        _ => display_as_cst,
    };

    loop {
        write!(stdout, "→ ").into_diagnostic()?;
        stdout.flush().into_diagnostic()?;

        stdin.read_line(&mut input).into_diagnostic()?;

        let parse = ChimeraParser::new(&input).parse::<IgnoreTrivia>();
        display(&parse);

        input.clear();
    }
}

fn display_as_cst(cst: &ConcreteSyntaxTree) {
    if cst.errors.is_empty() {
        println!("{}", cst.debug_tree());
    } else {
        for err in &cst.errors {
            let report: Report = err.clone().into();
            println!("{:?}", report);
        }
    }
}

fn display_as_ast(cst: &ConcreteSyntaxTree) {
    let ast = Root::try_from(cst.syntax_node_root()).unwrap();
    dbg!(
        ast.statements()
            // .filter_map(|stmt| match stmt {
            //     Stmt::VarDef(var_def) => Some(var_def.value()),
            //     Stmt::Expr(expr) => None,
            // match expr {
            //     Expr::VarRef(var_ref) => Some(var_ref.name()),
            //     _ => None,
            // },
            // })
            .collect::<Vec<_>>()
    );
}
