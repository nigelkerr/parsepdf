extern crate nom;

use nom::*;
use nom::ErrorKind;

// given a filter, possibly some parameters, and some bytes,
// return either some decoded bytes, or an error.
// something over in the main library figures out how to get here
// from the pdf objects themselves.

// this wants nice chainability.

use simple::bare_hexadecimal_string;
use simple::skip_whitespace;
use structs::PdfObject;

#[derive(Debug,PartialEq,Eq)]
pub enum DecodingResponse {
    RefuseToDecode,
    DecodeError,
}

pub fn decode_asciihex(input: &Vec<u8>) -> Result<Vec<u8>, DecodingResponse> {

    match bare_hexadecimal_string(&input[..]) {

        Ok((remainder, PdfObject::String(hex_part))) => {
            let skip_ws = skip_whitespace(remainder);
            if remainder[skip_ws] != b'>' {
                return Err(DecodingResponse::DecodeError);
            }
            Ok(hex_part)
        },
        _ => {
            Err(DecodingResponse::DecodeError)
        }
    }

}



//named!(enc_ascii85<&[u8],Vec<u8> >,
//
//);
//
//pub fn encode_ascii85(input: &Vec<u8>) -> Result<Vec<u8>, DecodingResponse> {
//
//}
//
//pub fn decode_ascii85(input: &Vec<u8>) -> Result<Vec<u8>, DecodingResponse> {
//
//
//}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_decode_asciihex() {

        assert_eq!(Ok(b"@@"[..].to_owned()), decode_asciihex(&b"4040>"[..].to_owned()));
        assert_eq!(Ok(b"@@"[..].to_owned()), decode_asciihex(&b"4 0  4\n0\t>"[..].to_owned()));

    }

}