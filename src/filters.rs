extern crate inflate;
extern crate lzw;

use crate::parser::{recognize_asciihex_hexadecimal_string};
use crate::{recognize_ascii85_string, recognize_rle_sequence};

use std::io;
use nom::AsBytes;

quick_error! {
    #[derive(Debug)]
    pub enum DecodingResponse {
        NotImplementedYet {}
        RefuseToDecode {}
        DecodeError {}
        DecoderInitializationError {}
        Io(err: std::io::Error) {
            from()
        }
    }
}

pub fn decode_asciihex(input: &Vec<u8>) -> Result<Vec<u8>, DecodingResponse> {
    match recognize_asciihex_hexadecimal_string(&input[..]) {
        Ok((_rest, decoded)) => Ok(decoded),
        _ => Err(DecodingResponse::DecodeError),
    }
}

pub fn decode_ascii85(input: &Vec<u8>) -> Result<Vec<u8>, DecodingResponse> {
    match recognize_ascii85_string(&input[..]) {
        Ok((_rest, decoded)) => Ok(decoded),
        _ => Err(DecodingResponse::DecodeError),
    }
}

// this feels less clumsy than the full duplication, but still clumsy.
// can't i recognize some other class as implementing a trait merely via interface?
trait LzwDecoder {
    fn dispatch_decode_bytes(&mut self, bytes: &[u8]) -> io::Result<(usize, &[u8])>;
}

impl LzwDecoder for lzw::Decoder<lzw::MsbReader> {
    fn dispatch_decode_bytes(&mut self, bytes: &[u8]) -> io::Result<(usize, &[u8])> {
        self.decode_bytes(bytes)
    }
}

impl LzwDecoder for lzw::DecoderEarlyChange<lzw::MsbReader> {
    fn dispatch_decode_bytes(&mut self, bytes: &[u8]) -> io::Result<(usize, &[u8])> {
        self.decode_bytes(bytes)
    }
}

pub fn decode_lzw(input: &Vec<u8>, early: bool) -> Result<Vec<u8>, DecodingResponse> {

    let mut decoder: Box<LzwDecoder> = if early {
        Box::new(lzw::DecoderEarlyChange::new(lzw::MsbReader::new(), 8))
    } else {
        Box::new(lzw::Decoder::new(lzw::MsbReader::new(), 8))
    };

    let mut result: Vec<u8> = Vec::new();

    let mut compressed = &input[..];

    while compressed.len() > 0 {
        let (start, bytes) = decoder.dispatch_decode_bytes(&compressed)?;
        compressed = &compressed[start..];
        result.extend_from_slice(bytes);
    }

    Ok(result)
}

pub fn decode_flate(input: &Vec<u8>) -> Result<Vec<u8>, DecodingResponse> {
    match inflate::inflate_bytes_zlib(input.as_bytes()) {
        Ok(result) => { Ok(result) },
        Err(_s) => { Err(DecodingResponse::DecodeError) },
    }
}

pub fn decode_rle(input: &Vec<u8>) -> Result<Vec<u8>, DecodingResponse> {
    match recognize_rle_sequence(input.as_slice()) {
        Ok((_rest, v)) => { Ok(v) },
        Err(_err) => { Err(DecodingResponse::DecodeError) },
    }
}

fn refuse_to_decode() -> Result<Vec<u8>, DecodingResponse> {
    Err(DecodingResponse::RefuseToDecode)
}

pub fn decode_ccittfax(_input: &Vec<u8>) -> Result<Vec<u8>, DecodingResponse> {
    refuse_to_decode()
}

pub fn decode_jbig2(_input: &Vec<u8>) -> Result<Vec<u8>, DecodingResponse> {
    refuse_to_decode()
}

pub fn decode_dct(_input: &Vec<u8>) -> Result<Vec<u8>, DecodingResponse> {
    refuse_to_decode()
}

pub fn decode_jpx(_input: &Vec<u8>) -> Result<Vec<u8>, DecodingResponse> {
    refuse_to_decode()
}

pub fn decode_crypt(_input: &Vec<u8>) -> Result<Vec<u8>, DecodingResponse> {
    Err(DecodingResponse::NotImplementedYet)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_decode_asciihex() {
        match decode_asciihex(&b"4040>".to_vec()) {
            Ok(v) => { assert_eq!(b"@@".to_vec(), v); }
            _ => { assert_eq!(101, 0); }
        }

        match decode_asciihex(&b"4 0  4\n0\t>".to_vec()) {
            Ok(v) => { assert_eq!(b"@@".to_vec(), v); }
            _ => { assert_eq!(102, 0); }
        }
    }

    #[test]
    fn test_decode_ascii85() {

        match decode_ascii85(&b"z~>".to_vec()) {
            Ok(v) => { assert_eq!(b"\x00\x00\x00\x00".to_vec(), v); },
            _ => { assert_eq!(103,0); }
        }

        match decode_ascii85(&b"9jqo^BlbD-BleB1DJ+*+F(f,q/0JhKF<GL>Cj@.4Gp$d7F!,L7@<6@)/0JDEF<G%<+EV:2F!,O<DJ+*.@<*K0@<6L(Df-\\0Ec5e;DffZ(EZee.Bl.9pF\"AGXBPCsi+DGm>@3BB/F*&OCAfu2/AKYi(DIb:@FD,*)+C]U=@3BN#EcYf8ATD3s@q?d$AftVqCh[NqF<G:8+EV:.+Cf>-FD5W8ARlolDIal(DId<j@<?3r@:F%a+D58'ATD4$Bl@l3De:,-DJs`8ARoFb/0JMK@qB4^F!,R<AKZ&-DfTqBG%G>uD.RTpAKYo'+CT/5+Cei#DII?(E,9)oF*2M7/c~>".to_vec()) {
            Ok(v) => { assert_eq!(b"Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.".to_vec(), v); },
            _ => { assert_eq!(104,0); }
        }
    }


    #[test]
    fn test_decode_lzw() {
        // finding actual PDF examples of LZW on the ground difficult

        let input: Vec<u8> = vec![0x80, 0x0b, 0x60, 0x50, 0x22, 0x0c, 0x0c, 0x85, 0x01];
        let data = b"-----A---B".to_vec();

        match decode_lzw(&input, true) {
            Ok(v) => { assert_eq!(data, v); },
            _ => { assert_eq!(105,0); },
        }


        let input = include_bytes!("../assets/lzw.bitstream").to_vec();
        let data = b"q\r660 0 0 907 0 0 cm\r/Im1 Do\rQ\r".to_vec();
        match decode_lzw(&input, true) {
            Ok(v) => { assert_eq!(data, v); },
            _ => { assert_eq!(106,0); },
        }

    }

    #[test]
    fn test_decode_flate() {
        let input = include_bytes!("../assets/flate.bitstream").to_vec();
        let data = b"q\nBT\n36 806 Td\nET\nQ\nq 1 0 0 1 0 0 cm /Xf1 Do Q\n".to_vec();

        match decode_flate(&input) {
            Ok(v) => { assert_eq!(data, v); },
            _ => { assert_eq!(107,0); },
        }
    }

    #[test]
    fn decode_rle_test() {
        let input = b"\xffb\x00c\x80".to_vec();
        let data = b"bbc".to_vec();

        match decode_rle(&input) {
            Ok(v) => { assert_eq!(data, v); },
            _ => { assert_eq!(108,0); },
        }
    }
}
