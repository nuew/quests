use std::{
    error::Error,
    fmt::{self, Display, Formatter},
    time::SystemTime,
};

use async_trait::async_trait;

use crate::users::User;

#[async_trait]
pub trait AccessControlled {
    type Roles: Send + Sync;
    type RoleUsersIter: Iterator<Item = User> + Send + Sync;

    async fn add_role_to(&self, target: &User, role: Self::Roles) -> Result<(), PermissionsError>;

    async fn remove_role_from(
        &self,
        target: &User,
        role: Self::Roles,
    ) -> Result<(), PermissionsError>;

    async fn current_user(&self) -> Option<User>;

    async fn current_role(&self) -> Option<HeldRole<Self::Roles>>;

    async fn role_users(&self, role: Self::Roles) -> Self::RoleUsersIter;
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct HeldRole<T> {
    pub user: User,
    pub role: T,
    pub applied_at: SystemTime,
    pub applied_by: User,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct PermissionsError;

impl Display for PermissionsError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "access denied")
    }
}

impl Error for PermissionsError {}
