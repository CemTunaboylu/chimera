pub mod errors;
pub mod lexer;
pub mod token_kind;
pub mod token_type;

#[cfg(test)]
mod tests {
    use miette::{Context, IntoDiagnostic, Result as MietteResult};
    use parameterized_test::{AnyhowResult, create};
    use std::{env, fs::read_to_string, path::PathBuf};

    use super::{errors::LexError, lexer::Lexer};

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
        create_file_reading_erroring_lexer_test,
        (file_name), {
            let path = env::current_dir()?;
            println!("The current directory is {}", path.display());

            let file_contents = get_file_contents(file_name.into()).expect("expected a valid file");
            let lexer = Lexer::new(file_contents.as_str());
            let mut err : AnyhowResult<(), LexError> = Ok(());
            for token in lexer {
                let token = match token {
                    Ok(tokenkind) => tokenkind,
                    Err(e) => {
                        eprintln!("{e:?}");
                        err = Err(e.into());
                        break;
                    }
                };
                println!("{token:?}");
            }
            err.expect_err("expected error");
        }
    }

    create_file_reading_erroring_lexer_test! {
        unlexable: create_path_for("/square.chi"),
    }
}
