# Cap

A Convenient Context Passing System for the Rust Programming Language

> **WARNING:** This crate is currently just a prototype. Generic support was very hastily put
> together and thus prone to all sorts of breakage! This crate will likely be reimplemented in the
> future now that the concept has been proven but this will almost certainly introduce
> incompatibilities with the current version. Early adopters, beware!

## Usage Guide

Cap provides a proc-macro which allows you to quickly pass bundles of context around your application in a refactor-friendly way.

There are two types of items one can define in Cap: *components* and *bundles*. Components define individual elements one can borrow in a bundle and are defined like so:

```rust
use cap::cap;

cap! {
    // This is called a type component because it has a fixed type.
    pub my_component1 = TypeOfTheComponent;

    // This is called a trait component because its precise type can be anything so long as it
    // implements `TraitForTheComponent`.
    pub my_component2: TraitForTheComponent;

    // Components need not have unique types. They will be "wired up" by identity alone.
    pub my_component3 = TypeOfTheComponent;
}
```

Bundles, meanwhile, are collections of references to components which can be reborrowed into other bundles containing a subset of those components. They can be defined like so:

```rust
use cap::cap;

cap! {
    // This line defines a bundle with a mutable reference to `my_component_1` and an immutable
    // reference to `my_component_2`.
    pub my_bundle => mut my_component_1, ref my_component_2;

    // This line defines a bundle with everything in `my_bundle` and an additional mutable
    // reference to `my_component3`. The duplication of `my_component_1` does not matter.
    pub my_inherited_bundle => my_bundle, mut my_component_1, mut my_component3;

    // You can inherit as many bundles as you'd like and can have as much overlap as you want.
    // It all works just fine, taking the strongest mutability for each component to resolve
    // conflicts.
    pub my_conflicted_bundle = my_bundle, my_inherited_bundle, mut my_component_2;
}
```

All bundle definitions also define an associated module with the `Bundle` structure and the `TyBundle` "type bundle." `Bundle<'a, M>` is a structure of all references the bundle uses. The lifetime `'a` is the lifetime of each reference and `M` is a type bundle of type `Sized + TyBundle` which provides the types for each trait component in the bundle.

To extract data from a component, one can use one of the two forms of the `cx!` proc macro: a fetch form and a constructor form. The fetch form is used like so:

```rust
use cap::cx;

impl<M: my_bundle::TyBundle> my_bundle::Bundle<'_, M> {
    pub fn do_something(&mut self) {
        // This syntax fetches individual component references from the bundle.
        let my_component1 = cx!(self => mut my_component1);
        let my_component2 = cx!(self => my_component2);
    }
}

impl<M: my_inherited_bundle::TyBundle> my_inherited_bundle::Bundle<'_, M> {
    pub fn do_something_on_inherited(&mut self) {
        let my_component3 = cx!(self => mut my_component3);

        // This syntax, meanwhile, reconstructs entire sub-bundles from the source bundle.
        cx!(self => my_inherited).do_something();

        // It's important to note that all of these operations are borrow aware—they only reborrow
        // what they need and let all other references in the bundle be.
        let _ = my_component3;
    }
}
```

The constructor form is the only valid way to construct new bundles and it is used like so:

```rust
use cap::cx;

fn main() {
    let mut my_component1 = ...;
    let my_component2 = ...;
    let mut my_component3 = ...;

    let my_bundle = cx!(my_bundle {
        // You can construct bundles by specifying each component manually...
        my_component1 = &mut my_component1,
        my_component2 = &my_component2,
    });

    cx!(my_inherited_bundle {
        my_component3 = &mut my_component3,

        // ...or by inheriting entire bundles into the construction of your new bundle. Once again,
        // this is borrow aware and will only borrow the fields in `my_bundle` which are actually
        // needed by this operation.
        my_bundle = my_bundle,
    })
    .do_something_on_inherited();
}
```

## Half-Baked Features

In addition to this useful core, Cap has several currently half-baked features which are nonetheless very helpful.

When constructing a bundle, one can use the spread syntax to have the expression on the right side get evaluated for every required component.

```rust
use cap::cx;

cx!(my_inherited_bundle {
    // Provide `my_component3` directly.
    my_component3 = ...,
    my_component1 = ...,

    // Evaluate the RHS expression for every component in `my_bundle` which has not yet received a
    // value. This will be evaluated for `my_component2` only since it is the only component in
    // `my_bundle` which has not been given a value yet.
    //
    // You can use as many of these as you want in a given constructor and they will be "evaluated"
    // from first to last.
    ...my_bundle = ...,
});
```

This feature is currently half baked because, while you can access the requested component type with the `TARGET` macro import, there's no way to determine the requested mutability without trait magic. Additionally, this feature does not currently work with the generics feature described below.

There often arises the situation where one needs to wrap every component in a bundle in a wrapper type such as a `Storage` or an `AccessToken`. This can be done with generic components. Components can be defined as generic with respect to other components like so:

```rust
use cap::cap;

cap! {
    pub access_token<T> = AccessToken<T>;
}
```

You can then wrap other components or bundles in generic components while defining a bundle. Generic components satisfy the special rule of never wrapping a given structure in a given generic component more than once (i.e. `access_token<access_token<T>>` is just `access_token<T>`). This can be used like so:

```rust
use cap::cap;

cap! {
	pub access_token<T> = AccessToken<T>;

	pub my_component1 = ...;
	pub my_component2 = ...;
	pub my_component3 = ...;
	pub my_component4 = ...;

	// You can wrap individual components like so...
	pub my_bundle1 => mut my_component1, mut my_component2, ref access_token<my_component3>;

	// ...or wrap every component in a bundle like so...
	pub my_bundle2 => access_token<my_bundle_1>, mut my_component_4;

	// Which is equivalent to writing...
	pub my_bundle2 =>
		// (expansion of `access_token<my_bundle_1>`)
		mut access_token<my_component1>,
		mut access_token<my_component2>,
		ref access_token<my_component3>,  // Note that double-wraps are reduced to single wraps.
		// (regular component list)
		mut my_component_4;
}
```

This feature is half-baked because trait components cannot be defined as generic or wrapped by a generic component. Additionally, it does not work properly with the spread syntax described above.
