extern crate kmpsearch;
extern crate nom;

use crate::parser::PdfObject;
use crate::parser::XrefTable;
use kmpsearch::Haystack;

quick_error! {
    #[derive(Debug)]
    pub enum PdfError {
        /// IO Error
        Io(err: std::io::Error) {
            from()
        }
        NotAFile {}
        NotAPdfOrNeedsFrontTrimming {}
        VeryShort {}
        /// something up with the trailer
        TrailerNotFound {}
        TrailerPuzzlingStructure {}
        Nom {}
        PdfParsing {}
        NotImplementedYet {}
    }
}

/// State of understanding of the organization of data and constructs inside
/// some PDF file.  This struct does not own the PDF file representation itself,
/// but works on a &[u8] provided.  Some PdfObject's have sub-slices of bytes
/// representing a stream.

#[derive(Debug, PartialEq, Clone)]
pub struct PdfFile {
    start_offset: u64,
    end_offset: u64,
    trailer_offsets: Vec<u64>,
    xref_offsets: Vec<u64>,
    xref_is_stream: Vec<bool>,
    trailers: Vec<PdfObject>,
    xref_tables: Vec<XrefTable>,
}

impl PdfFile {
    pub fn new() -> PdfFile {
        PdfFile {
            start_offset: 0,
            end_offset: 0,
            trailer_offsets: Vec::new(),
            xref_offsets: Vec::new(),
            xref_is_stream: Vec::new(),
            trailers: Vec::new(),
            xref_tables: Vec::new(),
        }
    }
}

pub fn parse_pdf(i: &[u8]) -> Result<PdfFile, PdfError> {
    Err(PdfError::NotImplementedYet)
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::AsBytes;
}
