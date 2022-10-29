use std::time::SystemTime;

use async_trait::async_trait;

use crate::{
    chats::{Chat, Message},
    dice::Dice,
    polls::Poll,
    users::User,
    AccessControlled, Deletable, HasTimeMetadata, HeldRole, PermissionsError, UploadedImage,
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub enum Role {
    Participant,
    Author,
    Owner,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub enum LiveStatus {
    Live(SystemTime),
    Complete,
    NotLive(Option<SystemTime>),
    OnHiatus(Option<SystemTime>),
    Cancelled,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub enum Visibility {
    Private,
    Unlisted,
    Public,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub enum ChapterCategory {
    Story,
    Appendix,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Quest {
    _marker: (),
}

impl Quest {
    pub async fn name(&self) -> &str {
        unimplemented!()
    }

    pub async fn set_name(&self, _name: String) -> Result<(), PermissionsError> {
        unimplemented!()
    }

    pub async fn summary(&self) -> &str {
        unimplemented!()
    }

    pub async fn set_summary(&self, _banner: String) -> Result<(), PermissionsError> {
        unimplemented!()
    }

    pub async fn cover(&self) -> UploadedImage {
        unimplemented!()
    }

    pub async fn set_cover(&self, _cover: UploadedImage) -> Result<(), PermissionsError> {
        unimplemented!()
    }

    pub async fn live_status(&self) -> LiveStatus {
        unimplemented!()
    }

    pub async fn set_live_status(&self, _live_status: LiveStatus) -> Result<(), PermissionsError> {
        unimplemented!()
    }

    pub async fn visibility(&self) -> Visibility {
        unimplemented!()
    }

    pub async fn set_visibility(&self, _visibility: Visibility) -> Result<(), PermissionsError> {
        unimplemented!()
    }

    pub async fn is_approved(&self) -> bool {
        unimplemented!()
    }

    pub async fn set_approved(&self, _approved: bool) -> Result<(), PermissionsError> {
        unimplemented!()
    }

    pub async fn chat(&self) -> Chat {
        unimplemented!()
    }

    pub async fn add_chapter(
        &self,
        _name: String,
        _category: ChapterCategory,
    ) -> Result<Chapter, PermissionsError> {
        unimplemented!()
    }

    pub async fn chapters(&self) -> impl Iterator<Item = Chapter> {
        unimplemented!();
        #[allow(unreachable_code)]
        std::iter::empty()
    }

    pub async fn reorder_chapters(&self, _new_order: &[Chapter]) -> Result<(), PermissionsError> {
        unimplemented!()
    }
}

#[async_trait]
impl AccessControlled for Quest {
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
impl HasTimeMetadata for Quest {
    async fn created_at(&self) -> SystemTime {
        unimplemented!()
    }

    async fn last_active_at(&self) -> SystemTime {
        unimplemented!()
    }
}

#[async_trait]
impl Deletable for Quest {
    async fn delete(&self) -> Result<(), PermissionsError> {
        unimplemented!()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Chapter {
    _marker: (),
}

impl Chapter {
    pub async fn name(&self) -> &str {
        unimplemented!()
    }

    pub async fn set_name(&self, _name: String) -> Result<(), PermissionsError> {
        unimplemented!()
    }

    pub async fn category(&self) -> ChapterCategory {
        unimplemented!()
    }

    pub async fn set_category(&self, _category: ChapterCategory) {
        unimplemented!()
    }

    pub async fn passages(&self) -> impl Iterator<Item = Passage> {
        unimplemented!();
        #[allow(unreachable_code)]
        std::iter::empty()
    }

    pub async fn reorder_passages(&self, _new_order: &[Passage]) -> Result<(), PermissionsError> {
        unimplemented!()
    }
}

#[async_trait]
impl HasTimeMetadata for Chapter {
    async fn created_at(&self) -> SystemTime {
        unimplemented!()
    }

    async fn last_active_at(&self) -> SystemTime {
        unimplemented!()
    }
}

#[async_trait]
impl Deletable for Chapter {
    async fn delete(&self) -> Result<(), PermissionsError> {
        unimplemented!()
    }
}

#[derive(Debug)]
// will hopefully fix itself once other stuff starts being filled out
#[allow(variant_size_differences)]
pub enum PassageContents {
    Chat(Chat),
    Dice(Dice<Quest>),
    Poll(Poll<Quest>),
    Text(Message), // TODO should this get its own struct?
    UploadedImage(UploadedImage),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Passage {
    _marker: (),
}

impl Passage {
    pub async fn contents(&self) -> PassageContents {
        unimplemented!()
    }
}

#[async_trait]
impl HasTimeMetadata for Passage {
    async fn created_at(&self) -> SystemTime {
        unimplemented!()
    }

    async fn last_active_at(&self) -> SystemTime {
        unimplemented!()
    }
}

#[async_trait]
impl Deletable for Passage {
    async fn delete(&self) -> Result<(), PermissionsError> {
        unimplemented!()
    }
}
