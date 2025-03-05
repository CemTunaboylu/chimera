use crate::{operator::starting_precedence, parse::Finished, parser::Parser};
use lexer::token_type::TokenType;
use syntax::{Syntax, syntax_kind::SyntaxKind};

use SyntaxKind::*;

impl<'input> Parser<'input> {
    // TODO: KWFn should prepare what it expects in order
    // expectations should be able to be ordered
    // fn <ident>({parameters as CSV}) {-> RetTYpe} {}
    pub fn parse_function_def(&mut self) -> Option<Finished> {
        self.expect(KwFn)?;
        let marker = self.start();
        self.bump();
        self.expect_and_bump(Ident);
        self.expect_and_bump(LParen);
        self.parse_comma_separated_typed_declarations_until(|syntax: Syntax| {
            !syntax.is_of_kind(RParen)
        });
        self.clean_buffer();
        self.expect_and_bump(RParen);

        match self.peek() {
            Some(Ok(syntax)) => {
                if matches!(syntax.get_kind(), RArrow) {
                    self.expect_and_bump(RArrow);
                    self.bump_with_marker(RetType);
                }
            }
            Some(Err(err)) => {
                self.recover_from_err(err);
            }
            _ => {
                let checkpoint = self.context.clone();
                self.context.expect(Block);
                self.recover_unmet_expectation();
                self.context = checkpoint;
            }
        }
        self.clean_buffer();
        self.parse_block();
        Some(self.complete_marker_with(marker, SyntaxKind::VarDef))
    }

    // TODO: accept a function instead i.e. whatever is comma separated
    pub fn parse_comma_separated_typed_declarations_until(
        &mut self,
        until: fn(Syntax) -> bool,
    ) -> Option<()> {
        let ident_or_type = |syntax: Syntax| {
            matches!(
                syntax.get_token_type(),
                TokenType::Type | TokenType::Identifier
            )
        };
        while until(self.peek()?.ok()?) {
            self.clean_buffer();
            let marker = self.start();
            self.expect_and_bump(Ident);
            self.expect_and_bump(Colon);
            self.expect_f_and_bump(ident_or_type);
            self.ignore_if(Comma);
            self.complete_marker_with(marker, ParamDecl);
            self.clean_buffer();
        }
        Some(())
    }

    pub fn parse_comma_separated_arguments_until(
        &mut self,
        until: fn(Syntax) -> bool,
    ) -> Option<()> {
        let checkpoint = self.context.clone();
        self.context.allow(RParen);
        while until(self.peek()?.ok()?) {
            let marker = self.start();
            self.parse_expression_until_binding_power(starting_precedence());
            self.ignore_if(Comma);
            self.complete_marker_with(marker, FnArg);
        }
        self.context = checkpoint;
        Some(())
    }
}
