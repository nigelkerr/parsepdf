extern crate kmpsearch;
extern crate nom;

use crate::parser::PdfObject;
use crate::parser::XrefTable;
use kmpsearch::Haystack;
use crate::{PdfVersion, recognize_pdf_trailer, recognize_pdf_cross_reference_section, NameMap, recognize_pdf_version};
use nom::AsBytes;

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
        NotARecognizedPdfVersion {}
        /// something up with the trailer
        TrailerNotFound {}
        TrailerPuzzlingStructure {}
        Nom {}
        PdfParsing {}
        NotImplementedYet {}
        StartXrefAttemptedInfiniteLoop {}
        StartXrefAttemptedNonsensicalValue {}
        TrailerPrevNotAnOffset {}
    }
}

/// State of understanding of the organization of data and constructs inside
/// some PDF file.  This struct does not own the PDF file representation itself,
/// but works on a &[u8] provided.  Some PdfObject's have sub-slices of bytes
/// representing a stream.
#[derive(Debug, PartialEq, Clone)]
pub struct PdfFile {
    version: PdfVersion,
    start_offset: u64,
    // how far into the byte array did the PDF actually start?
    end_offset: u64,
    // how many bytes does the PDF itself possibly take up?
    trailers: Vec<NameMap>,
    xref_offsets: Vec<u64>,
    xref_is_stream: Vec<bool>,
    xref_tables: Vec<XrefTable>,
    master_xref_table: XrefTable,
}

impl PdfFile {
    pub fn new() -> PdfFile {
        PdfFile {
            version: PdfVersion::Unknown,
            start_offset: 0,
            end_offset: 0,
            trailers: Vec::new(),
            xref_offsets: Vec::new(),
            xref_is_stream: Vec::new(),
            xref_tables: Vec::new(),
            master_xref_table: XrefTable::new(),
        }
    }

    pub fn populate_master_xref_table() -> Result<(), PdfError> {

    }
}

/// find early in the byte slice where the first occurrence of what might
/// be the PDF header is. pdfs are allowed to not be at the very start of the bytestream,
/// which is rather a shame (§ 7.5.2 Note 1)
fn locate_start_offset(i: &[u8]) -> Option<u64> {
    match i.first_indexof_needle("%PDF-") {
        Some(index) => { Some(index as u64) }
        None => { None }
    }
}

fn offset_of_latest_xref(i: &[u8]) -> Result<u64, PdfError> {
    let last_bytes = if i.len() < 1024 { i.len() } else { 1024 };

    let last_kaye = i.len() - last_bytes;
    let last_kay_slice = &i[last_kaye..];

    if let Some(offset) = last_kay_slice.last_indexof_needle(b"trailer") {
        let trailer_offset = last_kaye + offset;

        if let Ok(x) = recognize_pdf_trailer(&i[trailer_offset..]) {
            return Ok((x.1).1);
        } else {
            return Err(PdfError::Nom);
        }
    }
    println!("ugh2");
    Err(PdfError::TrailerNotFound)
}


pub fn parse_pdf(i: &[u8], file_len: u64) -> Result<PdfFile, PdfError> {
    let mut input = i;
    let mut pdf_file: PdfFile = PdfFile::new();

    match locate_start_offset(input) {
        Some(start_offset) => {
            pdf_file.start_offset = start_offset;
            if pdf_file.start_offset != 0 {
                input = &i[pdf_file.start_offset as usize..];
            }
            match recognize_pdf_version(&input) {
                Ok((_rest, version)) => {
                    pdf_file.version = version;
                },
                Err(_err) => {
                    return Err(PdfError::NotARecognizedPdfVersion);
                }
            }
            pdf_file.end_offset = file_len - pdf_file.start_offset;
        }
        None => { return Err(PdfError::NotAPdfOrNeedsFrontTrimming); }
    }

    let mut next_xref = offset_of_latest_xref(input)?;

    // try to march backwards through the file.
    loop {
        match recognize_pdf_cross_reference_section(&input[next_xref as usize..]) {
            Ok((rest, xr)) => {
                pdf_file.xref_offsets.push(next_xref);
                pdf_file.xref_is_stream.push(xr.is_stream());
                pdf_file.xref_tables.push(xr);

                match recognize_pdf_trailer(rest) {
                    Ok((_rest2, (PdfObject::Dictionary(trailer_map), _same_xref))) => {
                        match trailer_map.get2(b"Prev".as_bytes()) {
                            Ok(Some(PdfObject::Integer(new_xref))) => {
                                let new_xref_u64: u64 = new_xref as u64;
                                next_xref = new_xref_u64;
                                pdf_file.trailers.push(trailer_map);
                            }
                            Ok(Some(_other_pdf_object)) => {
                                return Err(PdfError::TrailerPrevNotAnOffset);
                            }
                            _ => {
                                pdf_file.trailers.push(trailer_map);
                                break;
                            }
                        }
                    }
                    Ok((_rest3, _unknown_tuple)) => {
                        return Err(PdfError::TrailerPuzzlingStructure);
                    }
                    Err(_err) => {
                        return Err(PdfError::Nom);
                    }
                }
            }
            Err(_err) => {
                return Err(PdfError::TrailerPuzzlingStructure);
            }
        }
    }

    Ok(pdf_file)
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::AsBytes;


    #[test]
    fn basic_xref_and_trailer_structure() {
        let data = include_bytes!("../assets/silly_xref_stack.dat");

        match parse_pdf(data, data.len() as u64) {
            Ok(pdffile) => {
                assert_eq!(3, pdffile.xref_tables.len());
                assert_eq!(PdfVersion::V1_0, pdffile.version);
            },
            Err(err) => {
                println!("err: {:#?}", err);
                assert_eq!(100, 0);
            }
        }
    }
}
