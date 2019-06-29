extern crate memmap;
extern crate parsepdf;

use std::env;
use std::fs::File;

use parsepdf::*;

use memmap::MmapOptions;

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

        let pdffile = parse_pdf(&mmap, file_len)?;
        println!("pdffile: {:#?}", pdffile);
        Ok(())
    } else {
        Err(PdfError::NotAFile)
    }
}

