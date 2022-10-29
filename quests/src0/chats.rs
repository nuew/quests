use std::time::SystemTime;

use async_trait::async_trait;

use crate::{
    dice::Dice, polls::Poll, users::User, AccessControlled, Deletable, HasTimeMetadata, HeldRole,
    PermissionsError, UploadedImage,
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub enum Role {
    Voiced,
    Moderator,
    Owner,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub enum PostingRestriction {
    Open,
    VoicedOnly,
    ModeratorOnly,
    Closed,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Chat {
    _marker: (),
}

impl Chat {
    pub async fn topic(&self) -> &str {
        unimplemented!()
    }

    pub async fn set_topic(&self, _topic: String) -> Result<(), PermissionsError> {
        unimplemented!()
    }

    pub async fn restriction(&self) -> PostingRestriction {
        unimplemented!()
    }

    pub async fn set_restriction(
        &self,
        _restriction: PostingRestriction,
    ) -> Result<(), PermissionsError> {
        unimplemented!()
    }

    pub async fn send_message(
        &self,
        _author: &User,
        _message: String,
        _attachment: Option<Attachment>,
    ) -> Result<Message, PermissionsError> {
        unimplemented!()
    }

    pub async fn messages(&self) -> impl Iterator<Item = Message> {
        unimplemented!();
        #[allow(unreachable_code)]
        std::iter::empty()
    }
}

#[async_trait]
impl AccessControlled for Chat {
    type Roles = Role;
    type RoleUsersIter = std::vec::IntoIter<User>;

    async fn add_role_to(&self, _user: &User, _role: Self::Roles) -> Result<(), PermissionsError> {
        unimplemented!()
    }

    async fn remove_role_from(
        &self,
        _user: &User,
        _role: Self::Roles,
    ) -> Result<(), PermissionsError> {
        unimplemented!()
    }

    async fn current_user(&self) -> Option<User> {
        unimplemented!()
    }

    async fn current_role(&self) -> Option<HeldRole<Self::Roles>> {
        unimplemented!()
    }

    async fn role_users(&self, _role: Self::Roles) -> Self::RoleUsersIter {
        unimplemented!()
    }
}

#[async_trait]
impl HasTimeMetadata for Chat {
    async fn created_at(&self) -> SystemTime {
        unimplemented!()
    }

    async fn last_active_at(&self) -> SystemTime {
        unimplemented!()
    }
}

#[async_trait]
impl Deletable for Chat {
    async fn delete(&self) -> Result<(), PermissionsError> {
        unimplemented!()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Message {
    _marker: (),
}

impl Message {
    pub async fn author(&self) -> &User {
        unimplemented!()
    }

    pub async fn message(&self) -> &str {
        unimplemented!()
    }

    pub async fn edit_message(&self, _message: &str) -> Result<(), PermissionsError> {
        unimplemented!()
    }

    pub async fn attachment(&self) -> Option<Attachment> {
        unimplemented!()
    }

    pub async fn delete_attachment(&self) -> Result<(), PermissionsError> {
        unimplemented!()
    }
}

#[async_trait]
impl HasTimeMetadata for Message {
    async fn created_at(&self) -> SystemTime {
        unimplemented!()
    }

    async fn last_active_at(&self) -> SystemTime {
        unimplemented!()
    }
}

#[async_trait]
impl Deletable for Message {
    async fn delete(&self) -> Result<(), PermissionsError> {
        unimplemented!()
    }
}

#[derive(Debug)]
pub enum Attachment {
    Dice(Dice<Chat>),
    Poll(Poll<Chat>),
    UploadedImage(UploadedImage),
}
