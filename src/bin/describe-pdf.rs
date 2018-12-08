extern crate nom;
extern crate parsepdf;
extern crate regex;
extern crate memmap;

use nom::*;

use regex::bytes::Regex;
use std::env;
use std::fs::File;
use std::io;
use std::io::Read;
use std::io::Seek;
use std::io::SeekFrom;

use parsepdf::*;

use memmap::MmapOptions;
use std::io::Write;

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();

    for possible_file in args {
        match process_file(possible_file.clone()) {
            Ok(()) => {}
            Err(e) => {
                println!("reading {:?} got error {:?}", possible_file, e);
            }
        }
    }
}

fn process_file(possible_file: String) -> Result<(), PdfError> {
    println!("processing alleged file '{:?}'", possible_file);
    let metadata = std::fs::metadata(possible_file.clone())?;

    if metadata.is_file() {
        let file_len = metadata.len();
        let mut file = File::open(possible_file).unwrap();
        let mmap = unsafe { MmapOptions::new().map(&file)? };

        match get_header(&mmap, file_len) {
            Ok(pdf_version) => {
                println!("version is {}", pdf_version)
            },
            Err(err) => {
                return Err(err);
            }
        }


        match get_trailer_and_xref(&mmap, file_len) {
            Ok((trailer_dict, xref_table, startxref)) => {
                println!("last xref starts at {}", startxref);
                println!("trailer dictionary: {}", &trailer_dict);
                println!("xref table: {}", xref_table);

                for object_number in xref_table.in_use() {
                    println!("processing object number {:?}", object_number);
                    let obj_bytes = get_byte_array_from_file(
                        &mut file,
                        xref_table.offset_of(object_number).unwrap(),
                        xref_table.max_length_of(object_number).unwrap(),
                    )?;
                    match indirect_object(&obj_bytes) {
                        Ok((_rest, ind_obj)) => {
                            println!("ind obj: {:?}", ind_obj);
                        },
                        Err(Err::Error(Context::Code(slice, descriptor))) => {
                            println!("error processing: {:?} on {:?}", descriptor, slice);
                        },
                        Err(some_error) => {
                            println!("error processing: {:?}", some_error);
                        },
                    }
                }

                println!("completed.\n");
                Ok(())
            },
            Err(err) => {
                Err(err)
            },
        }


    } else {
        Err(PdfError {
            desc: "argument is not a file".to_string(),
            underlying: None,
        })
    }
}

fn get_header( file: &[u8], file_len: u64, ) -> Result<PdfVersion, PdfError> {
    if 32 > file_len {
        return Err( PdfError{ desc: "file very short".to_owned(), underlying: None });
    }
    let start_of_file = &file[0..32];
    match pdf_header(&start_of_file) {
        Ok((_rest, pdf_version)) => {
            Ok(pdf_version)
        },
        Err(_) => {
            Err( PdfError{ desc: "pdf_header parsing failed; not a pdf, or needs front trimming".to_owned(), underlying: None })
        }
    }
}

fn get_trailer_and_xref(
    file: &[u8],
    file_len: u64,
) -> Result<(PdfObject, CrossReferenceTable, usize), PdfError> {
    let last_kaye = &file[(file_len - 1024) as usize..];
    let (dict, startxref) = parse_trailer(&last_kaye)?;
    let xref_bytes = &file[startxref..];
    match xref_table(&xref_bytes) {
        Ok((_rest, mut crt)) => {
            // assert something about _rest ?
            crt.pdf_length(file_len as usize);
            Ok((dict, crt, startxref))
        }
        Err(Err::Incomplete(_whatever)) => Err(PdfError {
            desc: "not enough bytes for xref_table?".to_string(),
            underlying: None,
        }),
        Err(_err) => Err(PdfError {
            desc: "Error(err) from nom".to_string(),
            underlying: None,
        }),
    }
}

// find in last_kaye where the last instance of "trailer" is,
// start our file_trailer from that last instance.

fn parse_trailer(last_kaye: &[u8]) -> Result<(PdfObject, usize), PdfError> {
    let re = Regex::new(r"trailer").unwrap();
    let mut trailer_offset: Option<usize> = None;
    for mat in re.find_iter(&last_kaye[..]) {
        trailer_offset = Some(mat.start());
    }
    match trailer_offset {
        Some(n) => match file_trailer(&last_kaye[(n as usize)..]) {
            Ok((_rest, (dict, startxref))) => {
                return Ok((dict, startxref));
            }
            _ => {
                return Err(PdfError {
                    desc: "Something wrong with file trailer".to_string(),
                    underlying: None,
                });
            }
        },
        None => {
            return Err(PdfError {
                desc: "Trailer not recognized in the last 1024 bytes of the file.".to_string(),
                underlying: None,
            });
        }
    }
}

/// caller expected to get the file state and numbers right!
fn get_byte_array_from_file(
    file: &mut File,
    start: usize,
    length: usize,
) -> Result<Vec<u8>, io::Error> {
    let _seek_result = file.seek(SeekFrom::Start(start as u64))?;
    let mut retval = vec![0u8; length as usize];
    let _read_result = file.read_exact(&mut retval[..])?;
    Ok(retval)
}