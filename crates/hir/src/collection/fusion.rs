use thin_vec::ThinVec;

use crate::{
    collection::{builtin::Op, contract::LayoutContract, expr::TensorExpr},
    scope::ScopedExprIdx,
};

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub struct FusedOp {
    pub id: ScopedExprIdx,
    pub op: TensorExpr,
    pub inputs: ThinVec<ScopedExprIdx>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
/// FusionGroup holds the DAG of the fused layout changes/ operations. The fused unints are not always linearly fused.
/// For example, `let z = x.add(y).relu()` is linear iff
/// - x and y have the same layouts
/// - x and y is NOT shared elsewhere
/// - produced by previous Ops
/// `let z = x.matmul(y).add(relu(x))` requires 2 reads from x, multiple temporaries and forked subgraphs.
/// To be able to fuse Ops operating on different layouts, fused kernel has to be able to handle layout transform internally.
pub struct FusionGroup {
    pub inputs: ThinVec<ScopedExprIdx>,
    pub outputs: ThinVec<ScopedExprIdx>,
    pub action: Op,
    /// topologically ordered FusedOps
    pub topo: ThinVec<FusedOp>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
/// a 'compiled' subgraph that AST/HIR will be eventually lower to
pub struct FusedKernel {
    pub inputs: ThinVec<ScopedExprIdx>,
    pub outputs: ThinVec<ScopedExprIdx>,
    /// graph of fused ops
    pub body: FusionGroup,
    /// expected layout behavior
    pub layout_contract: LayoutContract,
    /// for debugging or caching
    // ! can be hash too
    pub name: Option<String>,
}
