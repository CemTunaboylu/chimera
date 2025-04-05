use hir::hir::lower;
use miette::{Context, IntoDiagnostic, Report, Result as MietteResult};
use parser::{cst::ConcreteSyntaxTree, parser::Parser as ChimeraParser};

use ast::{ast::Root, statement::Stmt as AstStmt};
// use hir::lower;

use std::path::PathBuf;
use std::{
    fs::read_to_string,
    io::{Write, stdin, stdout},
};

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
    Cst,
    Ast,
    Hir,
}

fn main() -> MietteResult<()> {
    let args = Args::parse();

    let stdin = stdin();
    let mut stdout = stdout();
    let mut input = String::new();

    let display = match args.command {
        Commands::Ast => display_as_ast,
        Commands::Cst => display_as_cst,
        Commands::Hir => display_as_hir,
        Commands::Parse { filename } => {
            let content = get_file_contents(filename)?;
            let cst = ChimeraParser::new(&content).parse();
            display_as_cst(&cst);
            return Ok(());
        }
        _ => display_as_cst,
    };

    loop {
        write!(stdout, "→ ").into_diagnostic()?;
        stdout.flush().into_diagnostic()?;

        stdin.read_line(&mut input).into_diagnostic()?;

        let parse = ChimeraParser::new(&input).parse();
        display(&parse);

        input.clear();
    }
}

fn display_as_cst(cst: &ConcreteSyntaxTree) {
    if !cst.errors.is_empty() {
        for report in &cst.errors {
            println!("{:?}", report);
        }
    }
    println!("{}", cst.debug_tree());
}

fn display_as_ast(cst: &ConcreteSyntaxTree) {
    let ast = Root::try_from(cst).unwrap();
    dbg!(
        ast.statements()
            .filter_map(|stmt| match stmt {
                AstStmt::VarDef(var_def) => Some(var_def),
                AstStmt::Expr(_) => None,
                AstStmt::FnDef(_) => None,
                AstStmt::Jump(_) => None,
                AstStmt::Semi(_) => todo!(),
                AstStmt::ControlFlow(_) => todo!(),
                AstStmt::Loop(_) => todo!(),
                AstStmt::Return(_) => todo!(),
            })
            // match expr {
            //     Expr::VarRef(var_ref) => Some(var_ref.name()),
            //     _ => None,
            // },
            // })
            .collect::<Vec<_>>()
    );
}

fn display_as_hir(cst: &ConcreteSyntaxTree) {
    display_as_cst(cst);
    let ast_root = Root::try_from(cst).unwrap();
    println!("ast_root {:?}", ast_root);
    let mut expr_arena = lower(ast_root);
    // dbg!("begin: {expr_arena :?}", &expr_arena);

    for elm in &mut expr_arena {
        println!("{:?}", elm);
    }
    // dbg!("fin: {expr_arena :?}", &expr_arena);
}

fn get_file_contents(file_name: PathBuf) -> MietteResult<String> {
    let file_contents = read_to_string(&file_name)
        .into_diagnostic()
        .wrap_err_with(|| format!("reading '{}' failed", file_name.display()))?;
    Ok(file_contents)
}
