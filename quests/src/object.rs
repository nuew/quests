use std::marker::PhantomData;

use crate::{Chat, Quest, Shelf, User};

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub(crate) struct Handle<T> {
    id: u64,
    _marker: PhantomData<fn() -> T>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub(crate) enum ObjectHandle {
    Chat(Handle<Chat<()>>),
    Quest(Handle<Quest<()>>),
    Shelf(Handle<Shelf<()>>),
    User(Handle<User<()>>),
}
