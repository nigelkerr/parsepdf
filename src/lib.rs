//! parsepdf, library for parsing and validating PDF 2.0 files.
//!
//! It should be possible to implement a characterizer of features and
//! problems on top of this library.  Currently, there are no goals for
//! this library to support creating new PDFs.

#[macro_use]
extern crate nom;
#[macro_use]
extern crate approx;
#[macro_use]
extern crate quick_error2;

mod files;
mod parser;

pub use crate::files::*;
pub use crate::parser::*;

#[cfg(test)]
mod tests {}
