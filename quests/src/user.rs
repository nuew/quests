use std::marker::PhantomData;

use url::Url;

use crate::{
    object::Handle,
    roles::{Guest, Owner, Participant},
};

pub type Slug = String;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
#[non_exhaustive]
pub enum Role {
    Moderator,
    Owner,
    Operator,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct User<T> {
    handle: Handle<Self>,
    _marker: PhantomData<fn(T)>,
}

impl User<Guest> {
    /// Returns the user's display name.
    ///
    /// # Examples
    /// ```rust
    /// use quests::users::User;
    /// use smol::block_on;
    ///
    /// # let user = User::_private_dummy();
    /// println!("{} has voted!", block_on(user.name()));
    /// ```
    pub async fn name(&self) -> &str {
        unimplemented!()
    }

    /// Returns the user's avatar, if one is set.
    pub async fn avatar(&self) -> Option<Url> {
        unimplemented!()
    }

    pub async fn roles(&self) -> impl Iterator<Item = Role> {
        unimplemented!();
        #[allow(unreachable_code)]
        std::iter::empty()
    }
}

impl User<Participant> {
    pub async fn slugs(&self) -> impl Iterator<Item = &str> {
        unimplemented!();
        #[allow(unreachable_code)]
        std::iter::empty()
    }

    pub async fn following(&self) -> impl Iterator<Item = User> {
        unimplemented!();
        #[allow(unreachable_code)]
        std::iter::empty()
    }

    pub async fn followers(&self) -> impl Iterator<Item = User> {
        unimplemented!();
        #[allow(unreachable_code)]
        std::iter::empty()
    }
}

impl User<Owner> {
    /// Updates the user's display name.
    pub async fn set_name(&self, _name: String) {
        unimplemented!()
    }

    pub async fn email(&self) -> String {
        unimplemented!()
    }

    pub async fn set_email(&self, _email: String) {
        unimplemented!()
    }

    pub async fn add_slug(&self, _slug: Slug) {
        unimplemented!()
    }

    /// Sets the users avatar as provided, or removes it if given `None`.
    pub async fn set_avatar(&self, _avatar: Option<Url>) {
        unimplemented!()
    }
}
