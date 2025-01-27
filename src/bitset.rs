use crate::set::{Indexable, Set};

pub(crate) struct BitSet {
    set: Vec<u64>,
}

impl BitSet {
    #[inline]
    pub fn mod_bits() -> usize {
        64
    }
    pub fn new(size: usize) -> Self {
        let num_rows = size / Self::mod_bits() as usize + 1;
        Self {
            set: vec![0; num_rows],
        }
    }

    #[inline]
    pub fn get_row_and_bit(item: impl Indexable) -> (u64, usize) {
        let idx = item.index();
        let bits = Self::mod_bits();
        let bit: u64 = 1 << (idx % bits) as u64;
        let row = idx / bits;
        (bit, row)
    }
}

impl Set for BitSet {
    fn add(&mut self, item: impl Indexable) {
        let (bit, row) = Self::get_row_and_bit(item);
        self.set[row] |= bit;
    }

    fn del(&mut self, item: impl Indexable) {
        let (bit, row) = Self::get_row_and_bit(item);
        self.set[row] &= &!bit;
    }

    fn has(self, item: impl Indexable) -> bool {
        let (bit, row) = Self::get_row_and_bit(item);
        self.set[row] & bit > 0
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
            let mut set = BitSet::new(size);
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
