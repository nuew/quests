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

    async fn add_role(&self, target: &User, role: Self::Roles) -> Result<(), PermissionsError>;

    async fn remove_role(&self, target: &User, role: Self::Roles) -> Result<(), PermissionsError>;

    async fn role_users(&self, role: Self::Roles) -> Self::RoleUsersIter;

    async fn my_role_applied_at(&self) -> Option<SystemTime>;

    async fn my_role_applied_by(&self) -> Result<Option<User>, PermissionsError>;
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct PermissionsError;

impl Display for PermissionsError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "access denied")
    }
}

impl Error for PermissionsError {}
