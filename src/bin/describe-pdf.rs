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

        for obj_num in pdffile.master_xref_table().in_use() {
            match pdffile.master_xref_table().offset_of(obj_num) {
                Some(offset) => match recognize_pdf_indirect_object(&mmap[offset as usize..]) {
                    Ok((_rest, pdf_ind_obj)) => {
                        println!(
                            "object num {} at offset {}\n{}\n",
                            obj_num, offset, pdf_ind_obj
                        );
                    }
                    Err(_err) => {
                        println!(
                            "parsing error on object num {} at offset {}",
                            obj_num, offset
                        );
                        return Err(PdfError::Nom);
                    }
                },
                _ => {
                    println!("object num {} no offset???\n", obj_num);
                }
            }
        }

        Ok(())
    } else {
        Err(PdfError::NotAFile)
    }
}
