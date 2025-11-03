use thin_vec::ThinVec;

use crate::{
    collection::{canonical::CanonicalBuffer, layout::Layout, shape::Shape},
    literal::Value,
};

/*
   numel - number of elements
   coalesce
   from_file
   like(expr) -> b_or_t.like() or same_size()
   full_like
   arange
   linspace
   eye
   empty
   full
   concat
   split(horizontal or vertical)
   stack
   gather
   reshape
   slice(dim, index) same as tensor[a_b]
   scatter(src, offset, dim1, dim2)
   squeeze
   stack(tensors, dim)
   tile(dims) repeats the elements of input wrt repetitions given for each dim
   unsqueeze
   where
   rand
   randint
   save
   load
   abs
   bitwise ops are as they are
   ceil
   clamp
   floor
   round
   flatten
   unflatten
   dot

   gradient
   sigmoid
   softmax
   argmax
   argmin
   tanh

   all
   any
   mean
   median
   norm
   std
   std_mean
   unique
   var
   cov
   var_mean
   count
   sort
   diag


   g, ge, l, le, eq
   what about nan


*/
// ! we need axis information for some Ops

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum BuiltinMethod {
    Rand,                    // no args
    Ones,                    // no args (shape inferred from context or another arg)
    Reshape(ThinVec<usize>), // explicit shape
    Full { value: Value },   // full(value)
    Transpose,
    // ... etc.
}

trait BuiltinEvaluator {
    fn evaluate(&self, args: &[Value], shape: &Shape, layout: &Layout) -> CanonicalBuffer;
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
/// Op's are builtin methods or attributes to buffer/tensor type.
pub enum Op {
    Dot,
    /// buffer_or_tensor.T or buffer_or_tensor.transpose()
    Transpose,
    /// buffer_or_tensor.reshape(new_shape_buffer)
    Reshape(ThinVec<usize>),
    /// buffer_or_tensor.max()
    Max,
    /// buffer_or_tensor.amax(dim)
    Amax(isize),
    Min,
    Sum,
    Scale(Value),
    Add(Value),
    Mul,
    MatMul,
}

/*
GATHER:
>>> t = torch.tensor([[1,2], [3,4]])
>>> torch.gather(t, 1, torch.tensor([[0, 0], [1, 0]]))
tensor([[1, 1],
        [4, 3]])
>>> torch.gather(t, 1, torch.tensor([[0, 0], [0, 0]]))
tensor([[1, 1],
        [3, 3]])
>>> torch.gather(t, 1, torch.tensor([[1, 0], [0, 0]]))
tensor([[2, 1],
        [3, 3]])
>>> torch.gather(t, 0, torch.tensor([[0, 0], [0, 0]]))
tensor([[1, 2],
        [1, 2]])
>>> torch.gather(t, 0, torch.tensor([[0, 0]]))
tensor([[1, 2]])
>>> torch.gather(t, 1, torch.tensor([[1, 1], [0, 0]]))
tensor([[2, 2],
        [3, 3]])
>>> torch.gather(t, 0, torch.tensor([[0, 1], [0, 1]]))
tensor([[1, 4],
        [1, 4]])
>>> torch.gather(t, 0, torch.tensor([[0, 0], [1, 1]]))
tensor([[1, 2],
        [3, 4]])
 */
