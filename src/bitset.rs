use std::ops::{BitAnd, BitAndAssign, BitOrAssign, Not};

use crate::set::{Indexable, Set};

pub(crate) struct BitSet<I: Copy + Default> {
    set: Vec<I>,
}

#[inline]
pub fn mod_bits<E>() -> usize {
    // now that this may not yield the correct number of bits all the time
    // but for now that's fine
    size_of::<E>() * 8
}

impl<
        I: BitAnd<Output = I>
            + BitAndAssign
            + BitOrAssign
            + Copy
            + Default
            + From<u64>
            + Not<Output = I>
            + PartialOrd,
    > BitSet<I>
{
    pub fn new(size: usize) -> Self {
        let num_rows = size / mod_bits::<I>() as usize + 1;
        Self {
            set: vec![I::default(); num_rows],
        }
    }

    #[inline]
    pub fn get_row_and_bit(item: impl Indexable) -> (I, usize) {
        let idx = item.index();
        let bits = mod_bits::<I>();
        let bit = 1 << (idx % bits);
        let row = idx / bits;
        (bit.into(), row)
    }
}

impl<
        I: BitAnd<Output = I>
            + BitAndAssign
            + BitOrAssign
            + Copy
            + Default
            + From<u64>
            + Not<Output = I>
            + PartialOrd,
    > Set for BitSet<I>
{
    fn add(&mut self, item: impl Indexable) {
        let (bit, row) = Self::get_row_and_bit(item);
        self.set[row] |= bit;
    }

    fn del(&mut self, item: impl Indexable) {
        let (bit, row) = Self::get_row_and_bit(item);
        self.set[row] &= !bit;
    }

    fn has(&self, item: impl Indexable) -> bool {
        let (bit, row) = Self::get_row_and_bit(item);
        let res = self.set[row] & bit;
        res > I::default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl Indexable for usize {
        fn index(self) -> usize {
            self
        }
    }

    macro_rules! set_tests {
    ($($name:ident: $value:expr,)*) => {
    $(
        #[test]
        fn $name() {
            let (size, input, call_del) = $value;
            let mut set = BitSet::<u64>::new(size);
                set.add(input);
            if call_del{
                set.del(input);
            }
            assert_eq!(set.has(input), !call_del);
        }
    )*
    }
    }
    const NO_DEL_CALL: bool = false;
    const DEL_CALL: bool = true;

    set_tests! {
        add_0: (1, 0 as usize, NO_DEL_CALL),
        add_1: (2, 1 as usize, NO_DEL_CALL),
        add_63: (64, 63 as usize, NO_DEL_CALL),
        add_64: (65, 64 as usize, NO_DEL_CALL),
        del_0: (1, 0 as usize, DEL_CALL),
        del_1: (2, 1 as usize, DEL_CALL),
        del_63: (64, 63 as usize, DEL_CALL),
        del_64: (65, 64 as usize, DEL_CALL),
    }
}
