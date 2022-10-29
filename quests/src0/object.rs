use std::time::SystemTime;

use async_trait::async_trait;

use crate::roles::PermissionsError;

#[async_trait]
pub trait HasTimeMetadata {
    async fn created_at(&self) -> SystemTime;

    async fn last_active_at(&self) -> SystemTime;
}

#[async_trait]
pub trait Deletable {
    async fn delete(&self) -> Result<(), PermissionsError>;
}
