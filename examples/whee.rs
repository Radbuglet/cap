use cap::{cap, cx};
use std::{any::Any, fmt};

cap! {
    pub foo = u32;
    pub bar = i32;
    pub baz: Any;
    pub faz: fmt::Debug;
    pub maz => mut foo, ref bar, ref baz;
    pub laz => maz, maz, mut baz, mut faz;
}

impl<B: maz::TyBundle> maz::Bundle<'_, B> {
    pub fn whee(&mut self) {
        // let foo = cx!(self => mut foo);
        // dbg!();
    }
}

//
// impl<B: laz::TyBundle> laz::Bundle<'_, B> {
//     pub fn whee(&mut self) {
//         let faz = cx!(self => faz);
//         let a = cx!(self => mut foo);
//         let b = cx!(self => bar).abs();
//
//         *a += b as u32;
//
//         cx!(self => maz).whee();
//         dbg!(faz);
//     }
// }

fn main() {}
