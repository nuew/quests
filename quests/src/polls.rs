use std::{marker::PhantomData, time::SystemTime};

use async_trait::async_trait;

use crate::{users::User, AccessControlled, Handle, PermissionsError};

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
// use tallystick?
pub enum Method {
    Approval,
    Borda,
    FirstPastThePost,
    Schulze,
    SingleTransferrableVote,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub enum LiveCount {
    Private,
    OwnerOnly,
    ModeratorOnly,
    Public,
}

#[derive(Debug)]
pub struct Poll<T> {
    _marker: PhantomData<T>,
}

impl<T> Poll<T>
where
    T: AccessControlled + Sync,
{
    pub async fn question(&self) -> &str {
        unimplemented!()
    }

    pub async fn set_question(&self, _question: String) -> Result<(), PermissionsError> {
        unimplemented!()
    }

    pub async fn method(&self) -> Method {
        unimplemented!()
    }

    pub async fn set_method(&self, _method: Method) -> Result<(), PermissionsError> {
        unimplemented!()
    }

    pub async fn num_winners(&self) -> usize {
        unimplemented!()
    }

    pub async fn set_num_winners(&self, _num_winners: usize) -> Result<(), PermissionsError> {
        unimplemented!()
    }

    pub async fn live_count(&self) -> LiveCount {
        unimplemented!()
    }

    pub async fn set_live_count(&self, _live_count: LiveCount) -> Result<(), PermissionsError> {
        unimplemented!()
    }

    pub async fn options(&self) -> impl Iterator<Item = BallotOption> {
        unimplemented!();
        #[allow(unreachable_code)]
        std::iter::empty()
    }

    pub async fn new_option(
        &self,
        _submitter: &User,
        _option: String,
    ) -> Result<BallotOption, PermissionsError> {
        unimplemented!()
    }

    pub async fn vote(&self, _ranking: &[BallotOption]) -> Result<(), PermissionsError> {
        unimplemented!()
    }

    pub async fn tabulate(&self) -> impl Iterator<Item = BallotOption> {
        unimplemented!();
        #[allow(unreachable_code)]
        std::iter::empty()
    }

    pub async fn open(&self) -> Result<(), PermissionsError> {
        unimplemented!()
    }

    pub async fn close(&self) -> Result<(), PermissionsError> {
        unimplemented!()
    }
}

#[async_trait]
impl<T> Handle for Poll<T>
where
    T: AccessControlled + Sync,
{
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
impl<T> AccessControlled for Poll<T>
where
    T: AccessControlled + Sync,
{
    type Roles = T::Roles;
    type RoleUsersIter = T::RoleUsersIter;

    async fn add_role(&self, _target: &User, _role: Self::Roles) -> Result<(), PermissionsError> {
        unimplemented!()
    }

    async fn remove_role(
        &self,
        _target: &User,
        _role: Self::Roles,
    ) -> Result<(), PermissionsError> {
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
pub struct BallotOption {
    _marker: (),
}

impl BallotOption {
    pub async fn choice(&self) -> &str {
        unimplemented!()
    }

    pub async fn set_choice(&self, _choice: String) -> Result<(), PermissionsError> {
        unimplemented!()
    }

    pub async fn votes(&self) -> Result<impl Iterator<Item = usize>, PermissionsError> {
        unimplemented!();
        #[allow(unreachable_code)]
        Ok(std::iter::empty())
    }

    pub async fn cancel(&self, _reason: String) -> Result<(), PermissionsError> {
        unimplemented!()
    }
}

#[async_trait]
impl Handle for BallotOption {
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
