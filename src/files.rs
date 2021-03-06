extern crate kmpsearch;

use crate::parser::PdfObject;
use crate::parser::XrefTable;
use crate::PdfError;
use crate::{
    recognize_pdf_cross_reference, recognize_pdf_startxref, recognize_pdf_version, NameMap,
    PdfVersion,
};
use kmpsearch::Haystack;
use nom::AsBytes;

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
    xref_tables: Vec<XrefTable>,
    master_xref_table: XrefTable,
}

impl Default for PdfFile {
    fn default() -> Self {
        Self::new()
    }
}

impl PdfFile {
    pub fn new() -> PdfFile {
        PdfFile {
            version: PdfVersion::Unknown,
            start_offset: 0,
            end_offset: 0,
            trailers: Vec::new(),
            xref_offsets: Vec::new(),
            xref_tables: Vec::new(),
            master_xref_table: XrefTable::new(),
        }
    }

    pub fn populate_master_xref_table(&mut self) -> Result<(), PdfError> {
        for xt in self.xref_tables.iter() {
            for xref_entry in xt.all_entries().iter() {
                self.master_xref_table.add(*xref_entry);
            }
        }

        Ok(())
    }

    pub fn master_xref_table(&self) -> &XrefTable {
        &self.master_xref_table
    }
}

/// find early in the byte slice where the first occurrence of what might
/// be the PDF header is. pdfs are allowed to not be at the very start of the bytestream,
/// which is rather a shame (§ 7.5.2 Note 1)
fn locate_start_offset(i: &[u8]) -> Option<u64> {
    i.first_indexof_needle("%PDF-").map(|index| index as u64)
}

fn offset_of_latest_xref(i: &[u8]) -> Result<u64, PdfError> {
    let last_bytes = if i.len() < 1024 { i.len() } else { 1024 };

    let last_kaye = i.len() - last_bytes;
    let last_kay_slice = &i[last_kaye..];

    if let Some(offset) = last_kay_slice.last_indexof_needle(b"startxref") {
        let startxref_offset = last_kaye + offset;

        if let Ok(x) = recognize_pdf_startxref(&i[startxref_offset..]) {
            return Ok(x.1);
        } else {
            return Err(PdfError::Nom);
        }
    }
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
                }
                Err(_err) => {
                    return Err(PdfError::NotARecognizedPdfVersion);
                }
            }
            pdf_file.end_offset = file_len - pdf_file.start_offset;
        }
        None => {
            return Err(PdfError::NotAPdfOrNeedsFrontTrimming);
        }
    }

    let mut next_xref = offset_of_latest_xref(input)?;

    // try to march backwards through the file.
    loop {
        match recognize_pdf_cross_reference(&input[next_xref as usize..]) {
            Ok((_rest, (Some(xr), PdfObject::Dictionary(trailer_map)))) => {
                pdf_file.xref_offsets.push(next_xref);
                pdf_file.xref_tables.push(xr);

                match trailer_map.get2(b"Prev".as_bytes()) {
                    Some(PdfObject::Integer(new_xref)) => {
                        let new_xref_u64: u64 = new_xref as u64;

                        // these ought never loop!
                        if pdf_file.xref_offsets.contains(&new_xref_u64) {
                            return Err(PdfError::StartXrefAttemptedInfiniteLoop);
                        }

                        // these ought always strictly decrease!
                        if let Some(last_value) = pdf_file.xref_offsets.last() {
                            if *last_value <= new_xref_u64 {
                                return Err(PdfError::StartXrefAttemptedNonsensicalValue);
                            }
                        }

                        next_xref = new_xref_u64;
                        pdf_file.trailers.push(trailer_map);
                    }
                    Some(_other_pdf_object) => {
                        return Err(PdfError::TrailerPrevNotAnOffset);
                    }
                    _ => {
                        pdf_file.trailers.push(trailer_map);
                        break;
                    }
                }
            }
            Ok((_rest, (None, PdfObject::Stream(trailer_map, data)))) => {
                return Err(PdfError::NotImplementedYet);
            }
            _ => {
                return Err(PdfError::TrailerPuzzlingStructure);
            }
        }
    }

    match pdf_file.populate_master_xref_table() {
        Ok(()) => Ok(pdf_file),
        Err(err) => Err(err),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_xref_and_trailer_structure() {
        let data = include_bytes!("../assets/silly_xref_stack.dat");

        match parse_pdf(data, data.len() as u64) {
            Ok(pdffile) => {
                assert_eq!(3, pdffile.xref_tables.len());
                assert_eq!(PdfVersion::V1_0, pdffile.version);

                assert_eq!(5, pdffile.master_xref_table.in_use().len());
                assert_eq!(1, pdffile.master_xref_table.free().len());
            }
            Err(err) => {
                println!("err: {:#?}", err);
                assert_eq!(100, 0);
            }
        }
    }
}
