pub mod errors;
mod event;
mod event_holder;
mod language;
pub mod parse_behaviors;
extern crate lexer;
mod expression;
pub mod marker;
pub mod parser;
pub mod s_expression;
mod sink;
mod statement;
pub mod syntax;

#[cfg(test)]
mod tests {
    use miette::{Context, IntoDiagnostic, Result as MietteResult};
    use parameterized_test::create;
    use std::{fs::read_to_string, path::PathBuf};

    const TEST_DIR: &str = "test_programs";

    fn get_file_contents(file_name: PathBuf) -> MietteResult<String> {
        let file_contents = read_to_string(&file_name)
            .into_diagnostic()
            .wrap_err_with(|| format!("reading '{}' failed", file_name.display()))?;
        Ok(file_contents)
    }

    fn create_path_for(file: &str) -> String {
        let mut f = String::from(TEST_DIR);
        f.push_str(file);
        f
    }

    create! {
        create_file_reading_parser_test,
        (file_name), {
            let file_contents = get_file_contents(file_name.into()).expect("expected a valid file");
            let mut parser = Parser::new(file_contents.as_str());
            let mut err : MietteResult<Parse>;
            let parse = parser.parse::<IgnoreTrivia>();
            match parse {
                Ok(tokenkind) => todo!(),
                Err(e) => {
                    eprintln!("{e:?}");
                    err = Err(e);
                }
            }
            err.expect("expected no error");
        }
    }

    create_file_reading_parser_test! {}
}
