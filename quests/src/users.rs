use std::time::SystemTime;

use crate::{authenticate::AuthenticationMethod, EmailAddress, PermissionsError, UploadedImage};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub enum Role {
    Moderator,
    Operator,
    Owner,
}

pub type Slug = String;

#[derive(Debug)]
pub struct User {
    _marker: (),
}

impl User {
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

    /// Updates the user's display name.
    ///
    /// # Examples
    /// ```rust
    /// use quests::users::User;
    /// use smol::block_on;
    ///
    /// # let user = User::_private_dummy();
    /// block_on(async {
    ///     let old_name = user.name().await;
    ///
    ///     let mut new_name = String::from(old_name);
    ///     new_name.push('!');
    ///
    ///     user.set_name(&user, new_name).await;
    ///     assert_ne!(user.name().await, old_name)
    /// });
    /// ```
    pub async fn set_name(&self, _name: String) -> Result<(), PermissionsError> {
        unimplemented!()
    }

    pub async fn add_slug(&self, _slug: Slug) -> Result<(), PermissionsError> {
        unimplemented!()
    }

    pub async fn slugs(&self) -> impl Iterator<Item = &str> {
        unimplemented!();
        #[allow(unreachable_code)]
        std::iter::empty()
    }

    pub async fn email(&self) -> EmailAddress {
        unimplemented!()
    }

    pub async fn set_email(&self, _email: EmailAddress) -> Result<(), PermissionsError> {
        unimplemented!()
    }

    /// Returns the user's avatar, if one is set.
    ///
    /// # Examples
    /// By default, the user's avatar won't be set:
    /// ```rust
    /// use quests::users::User;
    /// use smol::block_on;
    ///
    /// # let user = User::_private_dummy();
    /// assert_eq!(block_on(user.avatar()), None);
    /// ```
    pub async fn avatar(&self) -> Option<UploadedImage> {
        unimplemented!()
    }

    /// Sets the users avatar as provided, or removes it if given `None`.
    ///
    /// # Examples
    /// To clear the user's avatar:
    /// ```rust
    /// use quests::users::User;
    /// use smol::block_on;
    ///
    /// # let user = User::_private_dummy();
    /// block_on(async {
    ///     user.set_avatar(&user, None).await;
    ///     assert_eq!(user.avatar().await, None);
    /// });
    /// ```
    pub async fn set_avatar(
        &self,
        _user: &User,
        _avatar: Option<UploadedImage>,
    ) -> Result<(), PermissionsError> {
        unimplemented!()
    }

    /// Queries the time that the user was created.
    ///
    /// # Examples
    /// ```rust
    /// use quests::users::User;
    /// use smol::block_on;
    /// use std::time::SystemTime;
    ///
    /// # let user = User::_private_dummy();
    /// block_on(async {
    ///     let time_since_created = user.created_at().await.elapsed();
    ///     println!("{} was created {:?} ago", user.name().await, time_since_created);
    /// });
    /// ```
    pub async fn created_at(&self) -> SystemTime {
        unimplemented!()
    }

    /// Queries the last time the user took any action.
    ///
    /// # Examples
    /// ```rust
    /// use quests::users::User;
    /// use smol::block_on;
    /// use std::time::SystemTime;
    ///
    /// # let user = User::_private_dummy();
    /// block_on(async {
    ///     let time_since_last_seen = user.last_seen_at().await.elapsed();
    ///     println!("{} was last seen {:?} ago", user.name().await, time_since_last_seen);
    /// });
    /// ```
    pub async fn last_seen_at(&self) -> SystemTime {
        unimplemented!()
    }

    pub async fn authentication_methods(&self) -> impl Iterator<Item = AuthenticationMethod> {
        unimplemented!();
        #[allow(unreachable_code)]
        std::iter::empty()
    }

    pub async fn roles(&self) -> impl Iterator<Item = Role> {
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

    /// Returns a dummy user for use in doc tests.
    ///
    /// This is not a part of the crate API; it must **never** be used outside of doctests in this
    /// file.
    #[doc(hidden)]
    pub fn _private_dummy() -> Self {
        Self { _marker: () }
    }
}
