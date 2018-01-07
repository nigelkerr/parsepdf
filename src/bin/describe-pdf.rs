
extern crate regex;
extern crate nom;
extern crate parsepdf;

use nom::IResult::*;

use std::env;
use std::fs::File;
use std::io::Seek;
use std::io::SeekFrom;
use std::io::Read;
use std::io;
use std::error::Error;
use regex::bytes::Regex;

use parsepdf::*;

/// caller expected to get the file state and numbers right!
fn get_byte_array_from_file(file: &mut File, start: u64, length: u64) -> Result<Vec<u8>, io::Error> {

    match file.seek(SeekFrom::Start(start)) {
        Ok(_offset) => {
            let mut retval = vec![0u8; length as usize];
            match file.read_exact(&mut retval[..]) {
                Ok(_) => {
                    Ok(retval)
                },
                Err(err) => {
                    Err(err)
                }
            }
        },
        Err(err) => {
            Err(err)
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();


    for possible_file in args {
        match std::fs::metadata(possible_file.clone()) {
            Ok(metadata) => {
                if metadata.is_file() {

                    let file_len = metadata.len();
                    let mut file = File::open(possible_file).unwrap();

                    match get_trailer_and_xref(&mut file, file_len) {
                        Ok((trailer_dict, xref_table)) => {
                            println!("trailer dictionary: {:?}", trailer_dict);
                            println!("xref table: {:?}", xref_table);
                        },
                        Err(err) => {
                            println!("trying to read file: {:?}", err);
                        }
                    }

                } else {
                    println!("argument {:?} is not a file.", possible_file);
                }
            },
            _ => {
                println!("argument {:?} could not get fs metadata.  does it exist?", possible_file);
            }
        }
    }


}

fn get_trailer_and_xref(file: &mut File, file_len: u64) -> Result<(PdfObject, CrossReferenceTable), PdfError> {

    let last_kaye = get_byte_array_from_file(file, file_len - 1024, 1024)?;
    let (dict, startxref) = parse_trailer(&last_kaye)?;
    let xref_bytes = get_byte_array_from_file(file, startxref, (file_len - startxref))?;
    match xref_table(&xref_bytes) {
        Done(_rest, crt) => {
            // assert something about _rest ?
            Ok((dict, crt))
        },
        Incomplete(_whatever) => {
            Err(PdfError{ desc: "not enough bytes for xref_table?".to_string(), underlying: None})
        },
        Error(_err) => {
            Err(PdfError{ desc: "Error(err) from nom".to_string(), underlying: None})
        }
    }
}

// find in last_kaye where the last instance of "trailer" is,
// start our file_trailer from that last instance.

fn parse_trailer(last_kaye: &Vec<u8>) -> Result<(PdfObject,u64), PdfError> {
    let re = Regex::new(r"trailer").unwrap();
    let mut trailer_offset: Option<u64> = None;
    for mat in re.find_iter(&last_kaye[..]) {
        trailer_offset = Some(mat.start() as u64);
    }
    match trailer_offset {
        Some(n) => {
            match file_trailer(&last_kaye[(n as usize)..]) {
                Done(_rest, (dict, startxref)) => {
                    return Ok((dict, startxref));
                },
                _ => {
                    return Err(PdfError { desc: "Something wrong with file trailer".to_string(), underlying: None});
                }
            }
        },
        None => {
            return Err(PdfError{ desc: "Trailer not recognized in the last 1024 bytes of the file.".to_string(), underlying: None });
        }
    }
}
