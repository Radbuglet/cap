use cap::cap;
use std::any::Any;

cap! {
    pub foo = u32;
    pub bar = i32;
    pub baz: Any;
    pub maz => ref foo, mut bar, mut baz;
}

fn main() {}
