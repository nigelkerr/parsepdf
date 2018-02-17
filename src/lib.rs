#[macro_use]
extern crate nom;

#[macro_use]
extern crate approx;
extern crate regex;

// parse a pdf file, per ISO 32000-2_2017(en)

mod structs;
mod simple;
mod nesting;
mod filters;

pub use structs::*;
pub use simple::*;
pub use nesting::*;

