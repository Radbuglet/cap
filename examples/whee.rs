use cap::{cap, cx};
use std::any::Any;

cap! {
    pub foo = u32;
    pub bar = i32;
    pub baz: Any;
    pub maz => mut foo, ref bar, ref baz;
    pub laz => maz, maz, mut baz;
}

impl<B: ?Sized + maz::TyBundle> maz::Bundle<'_, B> {
    pub fn whee(&mut self) {
        dbg!(cx!(self => mut foo));
    }
}

impl<B: ?Sized + laz::TyBundle> laz::Bundle<'_, B> {
    pub fn whee(&mut self) {
        let a = cx!(self => mut foo);
        let b = cx!(self => bar);

        *a += *b as u32;

        cx!(self => maz).whee();
    }
}

fn main() {}
