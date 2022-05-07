use crate::{
    authenticate::Session,
    chats::Chat,
    quests::Quest,
    users::{Slug, User},
    EmailAddress, PermissionsError,
};

#[derive(Debug)]
pub struct Context {
    _marker: (),
}

impl Context {
    pub async fn get_chat(&self, _session: Session, _id: u64) -> Result<Chat, PermissionsError> {
        unimplemented!()
    }

    pub async fn new_chat(&self, _owner: &User, _topic: String) -> Chat {
        unimplemented!()
    }

    pub async fn get_user_by_id(
        &self,
        _session: Session,
        _id: u64,
    ) -> Result<User, PermissionsError> {
        unimplemented!()
    }

    pub async fn get_user_by_slug(
        &self,
        _session: Session,
        _slug: Slug,
    ) -> Result<User, PermissionsError> {
        unimplemented!()
    }

    pub async fn new_user(&self, _name: String, _email: EmailAddress) -> User {
        unimplemented!()
    }

    pub async fn new_quest(&self, _name: String) -> Quest {
        unimplemented!()
    }
}
