use nom::{
    branch::alt, bytes::complete::*, character, combinator::*, error::*, sequence::*, Err, IResult,
    Needed,
};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum PdfVersion {
    V1_0,
    V1_1,
    V1_2,
    V1_3,
    V1_4,
    V1_5,
    V1_6,
    V1_7,
    V2_0,
    Unknown,
}

impl From<&[u8]> for PdfVersion {
    fn from(i: &[u8]) -> Self {
        match i {
            b"1.0" => PdfVersion::V1_0,
            b"1.1" => PdfVersion::V1_1,
            b"1.2" => PdfVersion::V1_2,
            b"1.3" => PdfVersion::V1_3,
            b"1.4" => PdfVersion::V1_4,
            b"1.5" => PdfVersion::V1_5,
            b"1.6" => PdfVersion::V1_6,
            b"1.7" => PdfVersion::V1_7,
            b"2.0" => PdfVersion::V2_0,
            _ => PdfVersion::Unknown,
        }
    }
}

pub fn recognize_pdf_version(
    i: &[u8],
) -> IResult<&[u8], PdfVersion> {
    preceded(
        tag(b"%PDF-"),
        map(
            alt((
                tag(b"1.0"),
                tag(b"1.1"),
                tag(b"1.2"),
                tag(b"1.3"),
                tag(b"1.4"),
                tag(b"1.5"),
                tag(b"1.6"),
                tag(b"1.7"),
                tag(b"2.0"),
            )),
            |s: &[u8]| PdfVersion::from(s),
        ),
    )(i)
}

#[cfg(test)]
mod tests {

    use super::*;
    use nom::AsBytes;

    #[test]
    fn recognize_version() {
        assert_eq!(
            Ok((b"\r\n".as_bytes(), PdfVersion::V1_0)),
            recognize_pdf_version(b"%PDF-1.0\r\n")
        );
        assert_eq!(
            Err(nom::Err::Error((b"1.9\r\n".as_bytes(), nom::error::ErrorKind::Tag))),
            recognize_pdf_version(b"%PDF-1.9\r\n")
        );
    }
}
