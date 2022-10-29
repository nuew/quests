#![forbid(unsafe_code)]
#![deny(future_incompatible, nonstandard_style, rust_2018_idioms)]
#![warn(
    macro_use_extern_crate,
    meta_variable_misuse,
    // missing_copy_implementations,
    missing_debug_implementations,
    // missing_docs,
    rustdoc::all,
    single_use_lifetimes,
    trivial_casts,
    trivial_numeric_casts,
    unreachable_pub,
    // unused_crate_dependencies,
    unused_import_braces,
    unused_lifetimes,
    unused_qualifications,
    unused_results,
    variant_size_differences
)]

mod object;
mod user;

pub mod roles;

use std::marker::PhantomData;

use self::object::Handle;

pub use self::user::User;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Chat<T> {
    handle: Handle<Self>,
    _marker: PhantomData<fn(T)>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Quest<T> {
    handle: Handle<Self>,
    _marker: PhantomData<fn(T)>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Shelf<T> {
    handle: Handle<Self>,
    _marker: PhantomData<fn(T)>,
}

#[derive(Debug)]
pub struct Context;
