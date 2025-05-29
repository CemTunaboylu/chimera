use crate::{bitset::SyntaxKindBitSet, context::ParserContext};

pub struct RollingBackAnchor<'caller> {
    reference: &'caller ParserContext,
    syntax_sets: [SyntaxKindBitSet; 4],
}

impl RollingBackAnchor<'_> {
    /// Creates a new `RollingBackAnchor` from a raw pointer to a `ParserContext`.
    ///
    /// This function creates an anchor that captures the current state of the parser context
    /// and will restore it when dropped. This is used internally by `ParserContext::rolling_back_anchor`
    /// to implement scoped modifications to parsing rules.
    ///
    /// # Safety
    ///
    /// The caller must guarantee that:
    /// - The `ctx` pointer points to a valid, properly aligned `ParserContext` instance
    /// - The `ParserContext` instance must outlive the returned `RollingBackAnchor`
    /// - No other code can modify the `ParserContext` through mutable references while the
    ///   `RollingBackAnchor` exists
    /// - The `ctx` pointer must remain valid for the entire lifetime of the returned `RollingBackAnchor`
    pub unsafe fn with(ctx: *mut ParserContext) -> Self {
        let as_ref = unsafe { ctx.as_ref().unwrap() };
        let sets = [
            as_ref.get_expectations(),
            as_ref.get_recovery_set(),
            as_ref.get_allowed(),
            as_ref.get_in_the_middle_of(),
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
