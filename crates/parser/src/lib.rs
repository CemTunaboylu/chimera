pub mod conditionals;
pub mod containers;
pub mod cst;
pub mod delimited;
pub mod errors;
mod event;
mod event_holder;
pub mod func;
pub mod implements;
pub mod loops;
pub mod marker;
mod operator;
pub mod parse;
pub mod parser;
mod recovery;
mod sink;
pub mod statement;
pub mod structure;

#[cfg(test)]
mod tests {
    use miette::{Context, IntoDiagnostic, Result as MietteResult};
    use std::{
        fs::{read_dir, read_to_string},
        path::PathBuf,
    };

    use crate::parser::Parser;

    // use crate::{parse_behaviors::IgnoreTrivia, parser::Parser};

    const TEST_DIR: &str = "test_programs";
    const OK: &str = "ok";
    const ERRD: &str = "errd";

    fn get_file_contents(file_name: &PathBuf) -> MietteResult<String> {
        let file_contents = read_to_string(&file_name)
            .into_diagnostic()
            .wrap_err_with(|| format!("reading '{}' failed", file_name.display()))?;
        Ok(file_contents)
    }

    // fn parse_file_contents(file_path: PathBuf, expect_errs: bool) {
    fn parse_file_contents(file_path: PathBuf, expect_errs: bool) {
        assert!(
            file_path.to_str().unwrap().ends_with(".chi"),
            "{file_path:?}"
        );
        let file_name = file_path.file_name().unwrap();
        let file_contents = get_file_contents(&file_path).expect("expected a valid file");
        let parser = Parser::new(file_contents.as_str());
        let cst = parser.parse();
        match (expect_errs, cst.errors.is_empty()) {
            (true, false) => {}
            (false, true) => {}
            (true, true) => {
                panic!("expected errors for {:?}", file_name);
            }
            (false, false) => {
                for report in &cst.errors {
                    println!("{:?}", report);
                }
                panic!("expected no error {:?}", file_name);
            }
        }
    }

    fn test_programs_in(target: String, expect_errs: bool) {
        let test_files = read_dir(&target)
            .unwrap()
            .map(|r| r.ok().unwrap().path())
            .collect::<Vec<PathBuf>>();
        for test_file in test_files {
            parse_file_contents(test_file, expect_errs);
        }
    }

    #[test]
    fn test_ok_programs() {
        let target = format!("{TEST_DIR}/{OK}");
        test_programs_in(target, false);
    }

    #[test]
    fn test_errd_programs() {
        let target = format!("{TEST_DIR}/{ERRD}");
        test_programs_in(target, true);
    }
}
