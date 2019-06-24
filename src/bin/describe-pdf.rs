extern crate kmpsearch;
extern crate memmap;
extern crate parsepdf;
#[macro_use]
extern crate quick_error2;

use std::env;
use std::fs::File;

use kmpsearch::Haystack;
use parsepdf::*;

use memmap::MmapOptions;

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
    }
}

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();

    for possible_file in args {
        println!("arg: {}", possible_file);
        match process_file(possible_file.clone()) {
            Ok(()) => {}
            Err(e) => {
                println!("reading {} got error {:#?}", possible_file, e);
            }
        }
    }
}

fn process_file(possible_file: String) -> Result<(), PdfError> {
    println!("processing alleged file '{}'", possible_file);
    let metadata = std::fs::metadata(possible_file.clone())?;

    if metadata.is_file() {
        let file_len = metadata.len();
        let file = File::open(possible_file).unwrap();
        let mmap = unsafe { MmapOptions::new().map(&file)? };

        let pdf_version = get_header(&mmap, file_len)?;
        println!("version is {}", pdf_version);

        match get_trailer_and_xref(&mmap, file_len) {
            Ok((trailer_dict, xref_table, startxref)) => {
                println!("last xref starts at {}", startxref);
                println!("trailer dictionary: {}", &trailer_dict);
                println!("xref table: {}", xref_table);

                for object_number in xref_table.in_use() {
                    println!("processing object number {}", object_number);
                    let obj_bytes = &mmap[xref_table.offset_of(object_number).unwrap() as usize .. xref_table
                        .max_length_of(object_number, file_len)
                        .unwrap() as usize];
                    match recognize_pdf_indirect_object(&obj_bytes) {
                        Ok((_rest, ind_obj)) => {
                            println!("ind obj: {}", ind_obj);
                        }
                        Err(err) => {
                            println!("error processing: {:#?}", err);
                            return Err(PdfError::PdfParsing);
                        }
                    }
                }

                println!("completed.\n");
                Ok(())
            }
            Err(err) => Err(err),
        }
    } else {
        Err(PdfError::NotAFile)
    }
}

fn get_header(file: &[u8], file_len: u64) -> Result<PdfVersion, PdfError> {
    if 32 > file_len {
        return Err(PdfError::VeryShort);
    }
    let start_of_file = &file[0..32];
    match recognize_pdf_header(&start_of_file) {
        Ok((_rest, pdf_version)) => Ok(pdf_version),
        Err(_) => Err(PdfError::NotAPdfOrNeedsFrontTrimming),
    }
}

fn get_trailer_and_xref(
    file: &[u8],
    file_len: u64,
) -> Result<(PdfObject, XrefTable, u64), PdfError> {
    let last_kaye = &file[(file_len - 1024) as usize..];
    let (dict, startxref) = parse_trailer(&last_kaye)?;
    let xref_bytes = &file[startxref as usize..];
    match recognize_pdf_cross_reference_section(&xref_bytes) {
        Ok((_rest, crt)) => Ok((dict, crt, startxref)),
        Err(err) => {
            println!("Nom error: {:#?}", err);
            return Err(PdfError::Nom);
        },
    }
}

// find in last_kaye where the last instance of "trailer" is,
// start our file_trailer from that last instance.

fn parse_trailer(last_kaye: &[u8]) -> Result<(PdfObject, u64), PdfError> {
    match last_kaye.last_indexof_needle(b"trailer") {
        Some(trailer_offset) => match recognize_pdf_trailer(&last_kaye[trailer_offset..]) {
            Ok((_rest, (trailer_dict, startxref_offset))) => Ok((trailer_dict, startxref_offset)),
            Err(_) => Err(PdfError::TrailerPuzzlingStructure),
        },
        None => Err(PdfError::TrailerNotFound),
    }
}

