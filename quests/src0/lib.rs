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

mod context;
mod object;
mod roles;
mod upload;

pub mod authenticate;
pub mod chats;
pub mod dice;
pub mod polls;
pub mod quests;
pub mod users;

pub use self::{
    context::Context,
    object::*,
    roles::{AccessControlled, HeldRole, PermissionsError},
    upload::UploadedImage,
};

pub type EmailAddress = String; // FIXME use some crate for this
