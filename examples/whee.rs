use cap::cap;
use std::any::Any;

cap! {
    pub foo = u32;
    pub bar = i32;
    pub baz: Any;
    pub maz => mut foo, mut bar, ref baz;
    pub laz => maz, maz, mut baz;
}

impl<B: ?Sized + laz::TyBundle> laz::Bundle<'_, B> {
    pub fn whee(&mut self) {}
}

fn main() {}
