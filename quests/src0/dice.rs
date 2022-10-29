use std::marker::PhantomData;

use crate::AccessControlled;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Dice<T> {
    _marker: PhantomData<T>,
}

impl<T> Dice<T> where T: AccessControlled + Sync {
    pub async fn 
}
