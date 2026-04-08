use thin_vec::ThinVec;

use crate::collection::layout::Layout;

pub trait Contracting {
    fn create(&self) -> LayoutContract;
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub struct LayoutContract {
    pub expected_input_layouts: ThinVec<Option<Layout>>,
    pub output_layout: Option<Layout>,
    pub allows_inplace: bool,
    pub requires_relayout: bool,
}
