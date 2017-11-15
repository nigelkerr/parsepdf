#[macro_use]
extern crate nom;

use nom::*;
use nom::ErrorKind;
use nom::IResult::*;
use std::str;
use std::ops::Range;
use std::ops::RangeFrom;
use std::ops::RangeTo;
use std::str::FromStr;
use std::str::from_utf8;

#[derive(Debug, PartialEq, Eq)]
enum PdfVersion {
    Known{major: u32, minor: u32}
}

// while i figure out how to live this way better: fails are 0.
// you want to know ahead of time that this will succeed, is
// what this silly thing means.
fn byte_to_uint(slice: &[u8]) -> u32 {
    match str::from_utf8(slice) {
        Ok(numstr) => match numstr.parse() {
            Ok(num) => num,
            Err(_) => 0
        },
        Err(_) => 0
    }
}

// which of these is better?  how will i figure that out?

named!(pdf_line_ending_by_macro,
    alt!(
        complete!(tag!(b"\r\n")) |
        complete!(tag!(b"\r")) |
        complete!(tag!(b"\n"))
    )
);

pub fn pdf_line_ending<T>(input:T) -> nom::IResult<T, T> where
    T: nom::Slice<Range<usize>>+nom::Slice<RangeFrom<usize>>+nom::Slice<RangeTo<usize>>,
    T: nom::InputIter+nom::InputLength,
    T: nom::Compare<&'static str> {

    // this here _ => is amounting to the complete! behavior above, it seems.
    match input.compare("\r\n") {
        CompareResult::Ok => Done(input.slice(2..), input.slice(0..2)),
        _ => match input.compare("\r") {
            CompareResult::Ok         => Done(input.slice(1..), input.slice(0..1)),
            _ => match input.compare("\n") {
                CompareResult::Ok         => Done(input.slice(1..), input.slice(0..1)),
                _ => Error(error_position!(ErrorKind::CrLf, input)),
            }
        }
    }
}

pub fn is_not_line_end_chars(chr:u8) -> bool {
    (chr != b'\n' && chr != b'\r')
}

// comments are going to by my undoing, given where all they can occur ( § 7.2.4 )

named!(pdf_comment<&[u8]>,
    do_parse!(
        tag!(b"%") >>
        val: take_while!( is_not_line_end_chars ) >>
        pdf_line_ending >>
        (val)
    )
);

named!(pdf_version,
    alt!(
        tag!(b"1.0") |
        tag!(b"1.1") |
        tag!(b"1.2") |
        tag!(b"1.3") |
        tag!(b"1.4") |
        tag!(b"1.5") |
        tag!(b"1.6") |
        tag!(b"1.7") |
        tag!(b"2.0")
    )
);

named!(pdf_magic<&[u8],PdfVersion>,
    do_parse!(
        tag!(b"%PDF-") >>
        ver_bytes: pdf_version >>
        pdf_line_ending_by_macro >>
        ( PdfVersion::Known{ major: byte_to_uint(&ver_bytes[0..1]), minor: byte_to_uint(&ver_bytes[2..])} )
    )
);

named!(pdf_header<&[u8],PdfVersion>,
    do_parse!(
        ver: pdf_magic >>
        opt!( pdf_comment ) >>
        ( ver )
    )
);

// § 7.3.2

named!(pdf_boolean<&[u8],bool>,
    map_res!(map_res!( alt!( tag!(b"true") | tag!(b"false")), str::from_utf8 ), FromStr::from_str)
);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pdf_boolean_test() {
        assert_eq!(true, pdf_boolean(b"true ").to_result().unwrap());
        assert_eq!(false, pdf_boolean(b"false").to_result().unwrap());
        assert_eq!(nom::Err::Position(nom::ErrorKind::Tag, &[98u8, 108u8, 97u8, 104u8][..]),
                   pdf_magic(b"blah").to_result().unwrap_err());
    }

    #[test]
    fn pdf_magic_test() {
        assert_eq!(PdfVersion::Known{major: 1, minor: 0}, pdf_magic(b"%PDF-1.0\r\n").to_result().unwrap());
        assert_eq!(nom::Err::Position(nom::ErrorKind::Alt, &[51u8, 46u8, 48u8, 13u8][..]),
                   pdf_magic(b"%PDF-3.0\r").to_result().unwrap_err());
    }

    #[test]
    fn pdf_linendings_test() {
        assert_eq!( b"\r".as_bytes(), pdf_line_ending(b"\rdd".as_bytes()).to_result().unwrap());
        assert_eq!( b"\r\n".as_bytes(), pdf_line_ending(b"\r\ndd".as_bytes()).to_result().unwrap());
        assert_eq!( b"\n".as_bytes(), pdf_line_ending(b"\ndd".as_bytes()).to_result().unwrap());
    }

    #[test]
    fn pdf_linendings_by_macro_test() {
        assert_eq!( b"\r".as_bytes(), pdf_line_ending_by_macro(b"\rdd".as_bytes()).to_result().unwrap());
        assert_eq!( b"\r\n".as_bytes(), pdf_line_ending_by_macro(b"\r\ndd".as_bytes()).to_result().unwrap());
        assert_eq!( b"\n".as_bytes(), pdf_line_ending_by_macro(b"\ndd".as_bytes()).to_result().unwrap());
    }

    #[test]
    fn pdf_comments_test() {
        assert_eq!(b"hiya".as_bytes(), pdf_comment(b"%hiya\n").to_result().unwrap());
        assert_eq!("なななな".as_bytes(), pdf_comment("%なななな\n".as_bytes()).to_result().unwrap());
    }

    #[test]
    fn pdf_header_test() {
        assert_eq!(nom::IResult::Done(b" ".as_bytes(), PdfVersion::Known{major: 1, minor: 0}),
                   pdf_header(b"%PDF-1.0\r ".as_bytes()));
        assert_eq!(nom::IResult::Done(b" ".as_bytes(), PdfVersion::Known{major: 2, minor: 0}),
                   pdf_header("%PDF-2.0\r%なななな\n ".as_bytes()));
    }
}

