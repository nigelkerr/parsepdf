extern crate lzw;
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
use simple::is_pdf_whitespace;
use structs::PdfObject;
use std::io;

#[derive(Debug, PartialEq, Eq)]
pub enum DecodingResponse {
    RefuseToDecode,
    DecodeError,
    DecoderInitializationError,
}


pub fn decode_asciihex(input: &Vec<u8>) -> Result<Vec<u8>, DecodingResponse> {
    match bare_hexadecimal_string(&input[..]) {
        Ok((remainder, PdfObject::String(hex_part))) => {
            let skip_ws = skip_whitespace(remainder);
            if remainder[skip_ws] != b'>' {
                return Err(DecodingResponse::DecodeError);
            }
            Ok(hex_part)
        }
        _ => {
            Err(DecodingResponse::DecodeError)
        }
    }
}

const C_85_4: u64 = 85 * 85 * 85 * 85;
const C_85_3: u64 = 85 * 85 * 85;
const C_85_2: u64 = 85 * 85;
const C_85_1: u64 = 85;

const ADDEND: u8 = 0x21;
const PADDING: u64 = (0x75 - ADDEND) as u64;


/// Tests if byte is ASCII85 digit: ! - u, z
#[inline]
pub fn is_ascii85_digit(chr: u8) -> bool {
    (chr >= ADDEND && chr <= 0x75) || (chr == 0x7A)
}

#[inline]
fn can_be_in_ascii85_string(chr: u8) -> bool {
    is_ascii85_digit(chr) ||
        is_pdf_whitespace(chr)
}


// we are trusting that what came before worked out...
fn byte_vec_from_ascii85_string(input: &[u8]) -> Result<Vec<u8>, nom::ErrorKind> {
    let mut result: Vec<u8> = Vec::new();
    let filtered: Vec<u8> = input.iter().filter(
        |&x| is_ascii85_digit(*x)
    ).map(|&x| x).collect();

    let mut pos = 0;
    let length = filtered.len();

    while pos < length {
        if filtered[pos] == 0x7A { // z special case
            result.push(0x0);
            result.push(0x0);
            result.push(0x0);
            result.push(0x0);
            pos += 1;
            continue;
        }

        let bytes_left = if (length - pos) > 5 { 5 } else { length - pos };
        if bytes_left < 2 {
            return Err(nom::ErrorKind::Custom(1));
        }

        let mut bytes_needed = 4;
        let mut v: u64 = ((filtered[pos] - ADDEND) as u64 * C_85_4) +
            ((filtered[pos + 1] - ADDEND) as u64 * C_85_3);

        match bytes_left {
            2 => {
                bytes_needed = 1;
                v += (PADDING * C_85_2) + (PADDING * C_85_1) + PADDING;
            }
            3 => {
                bytes_needed = 2;
                v += ((filtered[pos + 2] - ADDEND) as u64 * C_85_2) + (PADDING * C_85_1) + PADDING;
            }
            4 => {
                bytes_needed = 3;
                v += ((filtered[pos + 2] - ADDEND) as u64 * C_85_2) + ((filtered[pos + 3] - ADDEND) as u64 * C_85_1) + PADDING;
            }
            5 => {
                v += ((filtered[pos + 2] - ADDEND) as u64 * C_85_2) + ((filtered[pos + 3] - ADDEND) as u64 * C_85_1) + (filtered[pos + 4] - ADDEND) as u64;
            }
            _ => {
                return Err(nom::ErrorKind::Custom(2));
            }
        }

        let mut shift: u32 = 32;
        for _ in 0..bytes_needed {
            shift -= 8;
            result.push((v >> shift) as u8 & 0xff);
        }

        pos += bytes_needed + 1;
    }


    Ok(result)
}

named!(pub bare_ascii85_sequence<&[u8],Vec<u8>>,
    do_parse!(
        v: map_res!( take_while!( can_be_in_ascii85_string ), byte_vec_from_ascii85_string ) >>
        tag!(b"~>") >>
        ( v )
    )
);

pub fn decode_ascii85(input: &Vec<u8>) -> Result<Vec<u8>, DecodingResponse> {
    match bare_ascii85_sequence(&input[..]) {
        Ok((remainder, v)) => {
            Ok(v)
        }
        Err(_) => {
            Err(DecodingResponse::DecodeError)
        }
    }
}


trait LzwDecoder {
    fn dispatch_decode_bytes(&mut self, bytes: &[u8]) -> io::Result<(usize, &[u8])>;
}

impl LzwDecoder for lzw::Decoder<lzw::MsbReader> {
    fn dispatch_decode_bytes(&mut self, bytes: &[u8]) -> io::Result<(usize, &[u8])>
    {
        self.decode_bytes(bytes)
    }
}
impl LzwDecoder for lzw::DecoderEarlyChange<lzw::MsbReader> {
    fn dispatch_decode_bytes(&mut self, bytes: &[u8]) -> io::Result<(usize, &[u8])>
    {
        self.decode_bytes(bytes)
    }
}

pub fn decode_lzw(input: &Vec<u8>, early: bool) -> Result<Vec<u8>, DecodingResponse> {

    let compressed_length = input.len();

    let mut decoder: Box<LzwDecoder> = if early {
        Box::new(lzw::DecoderEarlyChange::new(lzw::MsbReader::new(), 8))
    } else {
        Box::new(lzw::Decoder::new(lzw::MsbReader::new(), 8))
    };

    let mut result: Vec<u8> = Vec::new();
    let mut bytes_read = 0;

    let mut compressed = &input[..];

    while compressed.len() > 0 {
        let (start, bytes) = decoder.dispatch_decode_bytes(&compressed).unwrap();
        compressed = &compressed[start..];
        result.extend_from_slice(bytes);
    }

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_decode_asciihex() {
        assert_eq!(Ok(b"@@"[..].to_owned()), decode_asciihex(&b"4040>"[..].to_owned()));
        assert_eq!(Ok(b"@@"[..].to_owned()), decode_asciihex(&b"4 0  4\n0\t>"[..].to_owned()));
    }

    #[test]
    fn test_decode_ascii85() {
        assert_eq!(Ok(b"\x00\x00\x00\x00"[..].to_owned()), decode_ascii85(&b"z~>"[..].to_owned()));

        assert_eq!(Ok(b"Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure."[..].to_owned()),
                   decode_ascii85(&b"9jqo^BlbD-BleB1DJ+*+F(f,q/0JhKF<GL>Cj@.4Gp$d7F!,L7@<6@)/0JDEF<G%<+EV:2F!,O<DJ+*.@<*K0@<6L(Df-\\0Ec5e;DffZ(EZee.Bl.9pF\"AGXBPCsi+DGm>@3BB/F*&OCAfu2/AKYi(DIb:@FD,*)+C]U=@3BN#EcYf8ATD3s@q?d$AftVqCh[NqF<G:8+EV:.+Cf>-FD5W8ARlolDIal(DId<j@<?3r@:F%a+D58'ATD4$Bl@l3De:,-DJs`8ARoFb/0JMK@qB4^F!,R<AKZ&-DfTqBG%G>uD.RTpAKYo'+CT/5+Cei#DII?(E,9)oF*2M7/c~>"[..].to_owned())
        )
    }

    #[test]
    fn test_decode_lzw() {
        let input: Vec<u8> = vec![0x80, 0x0b, 0x60, 0x50, 0x22, 0x0c, 0x0c, 0x85, 0x01];
        let data = b"-----A---B".to_vec();

        assert_eq!(
            Ok(data),
            decode_lzw(&input, true)
        );

        let data = include_bytes!("../assets/lzw.bitstream");
        assert_eq!(
            Ok(b"q\r660 0 0 907 0 0 cm\r/Im1 Do\rQ\r".to_vec()),
            decode_lzw(&data[..].to_vec(), true)
        );
    }
}