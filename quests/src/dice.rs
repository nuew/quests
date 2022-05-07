use std::marker::PhantomData;

#[derive(Debug)]
pub struct Dice<T> {
    _marker: PhantomData<T>,
}
