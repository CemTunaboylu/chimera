// ! the type must implement Cmp
#[derive(Default)]
pub struct MinMax {
    max: Option<Value>,
    min: Option<Value>,
}

impl MinMax {
    pub fn min_max(&mut self, value: &Value) {
        let v = value.clone();
        let mx = self.max.get_or_insert(v.clone());
        if *mx < v {
            *mx = v;
            return;
        }
        let mn = self.min.get_or_insert(v.clone());
        if *mn > v {
            *mn = v;
        }
    }
}
