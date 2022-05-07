#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct PasswordAuthentication {
    _marker: (),
}

impl PasswordAuthentication {
    pub async fn authenticate(&self, _password: &str) -> Option<AuthenticatedSession> {
        unimplemented!()
    }
}

#[derive(Debug, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct OAuthAuthentication {
    _marker: (),
}

impl OAuthAuthentication {
    pub async fn authenticate(&self, _token: ()) -> Option<AuthenticatedSession> {
        unimplemented!()
    }
}

#[derive(Debug, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum AuthenticationMethod {
    Password(PasswordAuthentication),
    OAuth(OAuthAuthentication),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct AuthenticatedSession {
    _marker: (),
}

pub type Session = Option<AuthenticatedSession>;
