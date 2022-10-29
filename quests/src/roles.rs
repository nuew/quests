use std::{
    cmp::Ordering,
    error::Error,
    fmt::{self, Display, Formatter},
};

use crate::{
    object::{Handle, ObjectHandle},
    Context, User,
};

/// A Role that is bound to a particular user and object.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
#[non_exhaustive]
pub enum Role {
    /// The role of an entirely unelevated user who would otherwise be without one.
    ///
    /// See [`Guest`] for details.
    Guest(Guest),
    /// The role of a participant/voiced/etc. user, who has been marked as entitled to vote,
    /// comment, or view the protected object even if it has been configured not to allow that
    /// for the general public.
    ///
    /// See [`Participant`] for details.
    Participant(Participant),
    /// The role of a coauthor/moderator of an object, who can contribute to or moderate that
    /// object.
    ///
    /// See [`Author`] for details.
    Author(Author),
    /// The role of the owner of an object; they have full control over it, including of roles.
    ///
    /// See [`Owner`] for details.
    Owner(Owner),
}

impl Role {
    async fn new<T>(_context: &Context, _object: &ObjectHandle, _user: &Handle<User<T>>) -> Self {
        todo!()
    }
}

/// Ordering is [`Guest`] < [`Participant`] < [`Author`] < [`Owner`], except that two roles of the
/// same level are incomparable if they are applied to different users. Of course, if applied to
/// the same user, two roles on the same level are Equal.
impl PartialOrd for Role {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self {
            Role::Guest(r) => match other {
                Role::Guest(o) if r == o => Some(Ordering::Equal),
                Role::Guest(_) => None,
                _ => Some(Ordering::Less),
            },
            Role::Participant(r) => match other {
                Role::Guest(_) => Some(Ordering::Greater),
                Role::Participant(o) if r == o => Some(Ordering::Equal),
                Role::Participant(_) => None,
                _ => Some(Ordering::Less),
            },
            Role::Author(r) => match other {
                Role::Owner(_) => Some(Ordering::Less),
                Role::Author(o) if r == o => Some(Ordering::Equal),
                Role::Author(_) => None,
                _ => Some(Ordering::Greater),
            },
            Role::Owner(r) => match other {
                Role::Owner(o) if r == o => Some(Ordering::Equal),
                Role::Owner(_) => None,
                _ => Some(Ordering::Greater),
            },
        }
    }
}

impl From<Guest> for Role {
    fn from(from: Guest) -> Self {
        Self::Guest(from)
    }
}

impl From<Participant> for Role {
    fn from(from: Participant) -> Self {
        Self::Participant(from)
    }
}

impl From<Author> for Role {
    fn from(from: Author) -> Self {
        Self::Author(from)
    }
}

impl From<Owner> for Role {
    fn from(from: Owner) -> Self {
        Self::Owner(from)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
struct RoleInner {
    user: Handle<User>,
    object: ObjectHandle,
}

/// The role of an entirely unelevated user who would otherwise be without one.
///
/// The specific powers of a Guest are determined by the relevant object.
///
/// Corresponds to [`Role::Guest`].
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Guest(RoleInner);

impl Guest {
    /// Returns a [`Role`] containing the highest permission this can be upgraded to.
    pub async fn upgrade(&self, context: &Context) -> Role {
        Role::new(context, &self.0.object, &self.0.user).await
    }
}

/// This is a special case; if you have any role, you can always get a [`Guest`] out of that role.
impl From<Role> for Guest {
    fn from(from: Role) -> Self {
        match from {
            Role::Guest(guest) => guest,
            Role::Participant(participant) => participant.into(),
            Role::Author(author) => author.into(),
            Role::Owner(owner) => owner.into(),
        }
    }
}

impl From<Participant> for Guest {
    fn from(from: Participant) -> Self {
        Self(from.0)
    }
}

impl From<Author> for Guest {
    fn from(from: Author) -> Self {
        Self(from.0)
    }
}

impl From<Owner> for Guest {
    fn from(from: Owner) -> Self {
        Self(from.0)
    }
}

/// The role of a participant/voiced/etc. user, who has been marked as entitled to vote,
/// comment, or view the protected object even if it has been configured not to allow that
/// for the general public.
///
/// The specific powers of a Participant are determined by the relevant object.
///
/// Corresponds to [`Role::Participant`].
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Participant(RoleInner);

impl Participant {
    /// Returns a [`Role`] containing the highest permission this can be upgraded to.
    pub async fn upgrade(&self, context: &Context) -> Role {
        Role::new(context, &self.0.object, &self.0.user).await
    }
}

/// Note that this does **not** attempt to upgrade the role, as that would require `async`.
impl TryFrom<Role> for Participant {
    type Error = PermissionsError;

    fn try_from(from: Role) -> Result<Self, Self::Error> {
        match from {
            Role::Guest(_) => Err(PermissionsError),
            Role::Participant(participant) => Ok(participant),
            Role::Author(author) => Ok(author.into()),
            Role::Owner(owner) => Ok(owner.into()),
        }
    }
}

impl From<Author> for Participant {
    fn from(from: Author) -> Self {
        Self(from.0)
    }
}

impl From<Owner> for Participant {
    fn from(from: Owner) -> Self {
        Self(from.0)
    }
}

/// The role of a coauthor/moderator of an object, who can contribute to or moderate that
/// object.
///
/// The specific powers of an Author are determined by the relevant object.
///
/// Corresponds to [`Role::Author`].
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Author(RoleInner);

impl Author {
    /// Returns a [`Role`] containing the highest permission this can be upgraded to.
    pub async fn upgrade(&self, context: &Context) -> Role {
        Role::new(context, &self.0.object, &self.0.user).await
    }
}

/// Note that this does **not** attempt to upgrade the role, as that would require `async`.
impl TryFrom<Role> for Author {
    type Error = PermissionsError;

    fn try_from(from: Role) -> Result<Self, Self::Error> {
        match from {
            Role::Guest(_) | Role::Participant(_) => Err(PermissionsError),
            Role::Author(author) => Ok(author),
            Role::Owner(owner) => Ok(owner.into()),
        }
    }
}

impl From<Owner> for Author {
    fn from(from: Owner) -> Self {
        Self(from.0)
    }
}

/// The role of the owner of an object; they have full control over it, including of roles.
///
/// The specific powers of an Owner are determined by the relevant object.
///
/// Corresponds to [`Role::Owner`].
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Owner(RoleInner);

/// Note that this does **not** attempt to upgrade the role, as that would require `async`.
impl TryFrom<Role> for Owner {
    type Error = PermissionsError;

    fn try_from(from: Role) -> Result<Self, Self::Error> {
        match from {
            Role::Guest(_) | Role::Participant(_) | Role::Author(_) => Err(PermissionsError),
            Role::Owner(owner) => Ok(owner),
        }
    }
}

/// An error returned when permission to take an action was either unavailable or uncached.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct PermissionsError;

impl Display for PermissionsError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "access denied")
    }
}

impl Error for PermissionsError {}
