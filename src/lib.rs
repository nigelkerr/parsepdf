//! parsepdf, library for parsing and validating PDF 2.0 files.
//!
//! It should be possible to implement a characterizer of features and
//! problems on top of this library.  Currently, there are no goals for
//! this library to support creating new PDFs.

#[macro_use]
extern crate nom;
#[macro_use]
extern crate approx;

mod parser;

pub use crate::parser::*;

#[cfg(test)]
mod tests {
    use nom::{
        bytes, character, error::ErrorKind, error::VerboseError, error::VerboseErrorKind, Err,
        IResult, Needed,
    };

    fn take_4_streaming(i: &[u8]) -> IResult<&[u8], &[u8]> {
        bytes::streaming::take(4u8)(i)
    }

    fn take_4_complete(i: &[u8]) -> IResult<&[u8], &[u8]> {
        bytes::complete::take(4u8)(i)
    }

    // the alpha0 function recognizes 0 or more alphabetic characters
    fn alpha0_streaming(i: &str) -> IResult<&str, &str> {
        character::streaming::alpha0(i)
    }

    fn alpha0_complete(i: &str) -> IResult<&str, &str> {
        character::complete::alpha0(i)
    }

    #[test]
    fn it_works() {
        // both parsers will take 4 bytes as expected
        assert_eq!(
            take_4_streaming(&b"abcde"[..]),
            Ok((&b"e"[..], &b"abcd"[..]))
        );
        assert_eq!(
            take_4_complete(&b"abcde"[..]),
            Ok((&b"e"[..], &b"abcd"[..]))
        );

        // if the input is smaller than 4 bytes, the streaming parser
        // will return `Incomplete` to indicate that we need more data
        assert_eq!(
            take_4_streaming(&b"abc"[..]),
            Err(Err::Incomplete(Needed::Size(4)))
        );

        // but the complete parser will return an error
        assert_eq!(
            take_4_complete(&b"abc"[..]),
            Err(Err::Error((&b"abc"[..], ErrorKind::Eof)))
        );

        // if there's a clear limit to the recognized characters, both parsers work the same way
        assert_eq!(alpha0_streaming("abcd;"), Ok((";", "abcd")));
        assert_eq!(alpha0_complete("abcd;"), Ok((";", "abcd")));

        // but when there's no limit, the streaming version returns `Incomplete`, because it cannot
        // know if more input data should be recognized. The whole input could be "abcd;", or
        // "abcde;"
        assert_eq!(
            alpha0_streaming("abcd"),
            Err(Err::Incomplete(Needed::Size(1)))
        );

        // while the complete version knows that all of the data is there
        assert_eq!(alpha0_complete("abcd"), Ok(("", "abcd")));

        assert_eq!(2 + 2, 4);
    }
}
