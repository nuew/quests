use std::time::SystemTime;

use async_trait::async_trait;

use crate::{
    dice::Dice, polls::Poll, users::User, AccessControlled, Handle, PermissionsError, UploadedImage,
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

#[derive(Debug)]
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
impl Handle for Chat {
    async fn created_at(&self) -> SystemTime {
        unimplemented!()
    }

    async fn last_active_at(&self) -> SystemTime {
        unimplemented!()
    }

    async fn delete(&self) -> Result<(), PermissionsError> {
        unimplemented!()
    }
}

#[async_trait]
impl AccessControlled for Chat {
    type Roles = Role;
    type RoleUsersIter = std::vec::IntoIter<User>;

    async fn add_role(&self, _user: &User, _role: Self::Roles) -> Result<(), PermissionsError> {
        unimplemented!()
    }

    async fn remove_role(&self, _user: &User, _role: Self::Roles) -> Result<(), PermissionsError> {
        unimplemented!()
    }

    async fn role_users(&self, _role: Self::Roles) -> Self::RoleUsersIter {
        unimplemented!()
    }

    async fn my_role_applied_at(&self) -> Option<SystemTime> {
        unimplemented!()
    }

    async fn my_role_applied_by(&self) -> Result<Option<User>, PermissionsError> {
        unimplemented!()
    }
}

#[derive(Debug)]
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

    pub async fn posted_at(&self) -> SystemTime {
        unimplemented!()
    }

    pub async fn last_edited_at(&self) -> SystemTime {
        unimplemented!()
    }

    pub async fn delete(&self) -> Result<(), PermissionsError> {
        unimplemented!()
    }
}

#[derive(Debug)]
pub enum Attachment {
    Dice(Dice<Chat>),
    Poll(Poll<Chat>),
    UploadedImage(UploadedImage),
}
