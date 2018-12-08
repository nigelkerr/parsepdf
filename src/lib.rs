#[macro_use]
extern crate nom;

#[macro_use]
extern crate approx;
extern crate regex;

// parse a pdf file, per ISO 32000-2_2017(en)

mod filters;
mod nesting;
mod simple;
mod structs;

pub use crate::nesting::*;
pub use crate::simple::*;
pub use crate::structs::*;
