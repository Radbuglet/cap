use cap::cap;

cap! {
    pub foo = u32;
    pub bar = i32;
    pub baz: Send;
    pub maz => foo, bar, baz;
}

fn main() {}
