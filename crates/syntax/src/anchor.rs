use crate::{bitset::SyntaxKindBitSet, context::ParserContext};

pub struct RollingBackAnchor<'caller> {
    reference: &'caller ParserContext,
    syntax_sets: [SyntaxKindBitSet; 3],
}

impl RollingBackAnchor<'_> {
    pub unsafe fn with(ctx: *mut ParserContext) -> Self {
        let as_ref = unsafe { ctx.as_ref().unwrap() };
        let sets = [
            as_ref.get_expectations(),
            as_ref.get_recovery_set(),
            as_ref.get_allowed(),
        ];
        Self {
            reference: as_ref,
            syntax_sets: sets,
        }
    }
}

impl Drop for RollingBackAnchor<'_> {
    fn drop(&mut self) {
        let reverted: ParserContext = self.syntax_sets.as_ref().into();
        self.reference.take(reverted);
    }
}

#[cfg(test)]
mod tests {
    use crate::syntax_kind::SyntaxKind;

    use super::*;
    #[test]
    #[allow(unused_variables)]
    fn test_anchor_drop_scope_rollbacks() {
        let mut ctx = ParserContext::new();
        ctx.allow_all();
        let to_forbid = SyntaxKind::LParen;
        assert!(ctx.is_allowed(to_forbid));
        {
            let disallow_l_paren_anchor =
                unsafe { RollingBackAnchor::with(&mut ctx as *mut ParserContext) };
            ctx.forbid(to_forbid);
            {
                assert!(!ctx.is_allowed(to_forbid));
                let to_allow = to_forbid;
                let reallow_l_paren_anchor =
                    unsafe { RollingBackAnchor::with(&mut ctx as *mut ParserContext) };
                ctx.allow(to_allow);
                assert!(ctx.is_allowed(to_forbid));
            }
            assert!(!ctx.is_allowed(to_forbid));
        }
        assert!(ctx.is_allowed(to_forbid));
    }
}
