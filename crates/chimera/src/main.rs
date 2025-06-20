use hir::builder::lower;
use miette::{Context, IntoDiagnostic, Result as MietteResult};
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
    ast.statements().for_each(|stmt| match stmt {
        AstStmt::LetBinding(let_binding) => println!("Let Binding: {:?}", let_binding),
        AstStmt::Expr(_) => {}
        AstStmt::FnDef(fn_def) => println!("FnDef: {:?}", fn_def),
        AstStmt::Jump(_) => {}
        AstStmt::Semi(_) => todo!(),
        AstStmt::ControlFlow(_) => todo!(),
        AstStmt::Loop(_) => todo!(),
        AstStmt::Return(_) => todo!(),
        AstStmt::Impl(_) => todo!(),
        AstStmt::StructDef(_) => todo!(),
    });
}

fn display_as_hir(cst: &ConcreteSyntaxTree) {
    display_as_cst(cst);
    let ast_root = Root::try_from(cst).unwrap();
    println!("ast_root {:?}", ast_root);
    let mut expr_arena = lower(ast_root);

    for elm in &mut expr_arena {
        println!("{:?}", elm);
    }
}

fn get_file_contents(file_name: PathBuf) -> MietteResult<String> {
    let file_contents = read_to_string(&file_name)
        .into_diagnostic()
        .wrap_err_with(|| format!("reading '{}' failed", file_name.display()))?;
    Ok(file_contents)
}
