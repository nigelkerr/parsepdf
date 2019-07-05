extern crate inflate;
extern crate lzw;

use crate::{recognize_asciihex_hexadecimal_string, recognize_ascii85_string, recognize_rle_sequence, NameMap, PdfObject};

use std::io;
use nom::{AsBytes};
use nom::error::ErrorKind;

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
        IndirectParametersSupplied {}
        InvalidFilterSpecification {}
        InvalidDecodeParamsSpecification {}

        Ugh {}
    }
}

/// attempt to decode input bytes with the data in stream_dictionary.
/// call needs to have made us a stream_dictionary where the things we
/// will need are all *direct*, by some means the caller will figure out.
/// our very first stream type to decode, the XRefStm, is mandated to only
/// have direct values for the things we will need, so that path gets to
/// not worry about it, yet.  subsequent stream decoding will need to have
/// a way of solving this problem before calling for decoding.
/// even if we didn't decode anything, return new Vec<u8>.
/// this also doesn't care about the stream having been external: caller brings
/// a stream they want decoded and its dictionary.
pub fn decode(input: &Vec<u8>, stream_dictionary: &NameMap) -> Result<Vec<u8>, DecodingResponse> {

    // TODO do us a sanity check on /Length

    // we might have zero or more filters applied.
    // decide immediately whether we care about Filter/DecodeParams or FFilter/FDecodeParams

    let mut filters: Vec<PdfObject> = Vec::new();
    let mut decode_params: Vec<PdfObject> = Vec::new();
    let mut filter_name = b"Filter".as_bytes();
    let mut decode_param_name = b"DecodeParams".as_bytes();
    if stream_dictionary.contains_key2(b"F".as_bytes()) {
        filter_name = b"FFilter".as_bytes();
        decode_param_name = b"FDecodeParams".as_bytes();
    }

    match stream_dictionary.get2(filter_name) {
        Some(PdfObject::Array(vec_of_filter_names)) => {
            filters.extend(vec_of_filter_names);
        }
        Some(PdfObject::Name(name_vec)) => {
            filters.push(PdfObject::Name(name_vec.clone()));
        }
        Some(anything_else) => { return Err(DecodingResponse::InvalidFilterSpecification); }
        None => {} // no filters, so we'll do an identity
    }

    // no filters: send it back.
    if filters.len() == 0 {
        return Ok(input.clone());
    }

    match stream_dictionary.get2(decode_param_name) {
        Some(PdfObject::Array(vec_of_decode_param_dictionaries)) => {
            decode_params.extend(vec_of_decode_param_dictionaries);
        }
        Some(PdfObject::Dictionary(decode_name_map)) => {
            decode_params.push(PdfObject::Dictionary(decode_name_map.clone()));
        }
        Some(anything_else) => { return Err(DecodingResponse::InvalidDecodeParamsSpecification); }
        None => {} // no decode params.
    }

    let mut current_result: Vec<u8> = input.clone();

    for (pos, filt) in filters.iter().enumerate() {
        let decode_param = decode_params.get(pos);

        match filt {
            PdfObject::Name(decode_name) => {

                match decode_name.as_slice() {
                    b"ASCIIHexDecode" => {
                        current_result = decode_asciihex(&current_result)?;
                    },
                    b"ASCII85Decode" => {
                        current_result = decode_ascii85(&current_result)?;
                    },
                    b"FlateDecode" => {
                        current_result = decode_flate(&current_result, decode_param)?;
                    },
                    _ => {
                        return Err(DecodingResponse::NotImplementedYet);
                    }
                }

            },
            wut => {
                println!("wut: here with {:#?}", wut);
                return Err(DecodingResponse::NotImplementedYet);
            },
        }
    }

    Ok(current_result)
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

// so that i dont care which it is ugh
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

pub fn decode_flate(input: &Vec<u8>, decode_params: Option<&PdfObject>) -> Result<Vec<u8>, DecodingResponse> {

    if let Some(PdfObject) = decode_params {
        return Err(DecodingResponse::NotImplementedYet);
    }

    match inflate::inflate_bytes_zlib(input.as_bytes()) {
        Ok(result) => { Ok(result) }
        Err(_s) => { Err(DecodingResponse::DecodeError) }
    }
}

pub fn decode_rle(input: &Vec<u8>) -> Result<Vec<u8>, DecodingResponse> {
    match recognize_rle_sequence(input.as_slice()) {
        Ok((_rest, v)) => { Ok(v) }
        Err(_err) => { Err(DecodingResponse::DecodeError) }
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
            Ok(v) => { assert_eq!(b"\x00\x00\x00\x00".to_vec(), v); }
            _ => { assert_eq!(103, 0); }
        }

        match decode_ascii85(&b"9jqo^BlbD-BleB1DJ+*+F(f,q/0JhKF<GL>Cj@.4Gp$d7F!,L7@<6@)/0JDEF<G%<+EV:2F!,O<DJ+*.@<*K0@<6L(Df-\\0Ec5e;DffZ(EZee.Bl.9pF\"AGXBPCsi+DGm>@3BB/F*&OCAfu2/AKYi(DIb:@FD,*)+C]U=@3BN#EcYf8ATD3s@q?d$AftVqCh[NqF<G:8+EV:.+Cf>-FD5W8ARlolDIal(DId<j@<?3r@:F%a+D58'ATD4$Bl@l3De:,-DJs`8ARoFb/0JMK@qB4^F!,R<AKZ&-DfTqBG%G>uD.RTpAKYo'+CT/5+Cei#DII?(E,9)oF*2M7/c~>".to_vec()) {
            Ok(v) => { assert_eq!(b"Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.".to_vec(), v); }
            _ => { assert_eq!(104, 0); }
        }
    }


    #[test]
    fn test_decode_lzw() {
        // finding actual PDF examples of LZW on the ground difficult

        let input: Vec<u8> = vec![0x80, 0x0b, 0x60, 0x50, 0x22, 0x0c, 0x0c, 0x85, 0x01];
        let data = b"-----A---B".to_vec();

        match decode_lzw(&input, true) {
            Ok(v) => { assert_eq!(data, v); }
            _ => { assert_eq!(105, 0); }
        }


        let input = include_bytes!("../assets/lzw.bitstream").to_vec();
        let data = b"q\r660 0 0 907 0 0 cm\r/Im1 Do\rQ\r".to_vec();
        match decode_lzw(&input, true) {
            Ok(v) => { assert_eq!(data, v); }
            _ => { assert_eq!(106, 0); }
        }
    }

    #[test]
    fn test_decode_flate() {
        let input = include_bytes!("../assets/flate.bitstream").to_vec();
        let data = b"q\nBT\n36 806 Td\nET\nQ\nq 1 0 0 1 0 0 cm /Xf1 Do Q\n".to_vec();

        match decode_flate(&input, None) {
            Ok(v) => { assert_eq!(data, v); }
            _ => { assert_eq!(107, 0); }
        }
    }

    #[test]
    fn decode_rle_test() {
        let input = b"\xffb\x00c\x80".to_vec();
        let data = b"bbc".to_vec();

        match decode_rle(&input) {
            Ok(v) => { assert_eq!(data, v); }
            _ => { assert_eq!(108, 0); }
        }
    }

    #[test]
    fn decode_dispatch_entry_point() {
        let input = b"hiya toots!\n".to_vec();
        let map = NameMap::of(vec![PdfObject::Name(b"Length".to_vec()), PdfObject::Integer(12)]).unwrap();

        match decode(&input, &map) {
            Ok(retval) => {
                assert_eq!(input, retval);
            }
            anything_else => {
                println!("got something horrible {:#?}", anything_else);
                assert_eq!(110, 0);
            }
        }

        let input = b"9jqo^BlbD-BleB1DJ+*+F(f,q/0JhKF<GL>Cj@.4Gp$d7F!,L7@<6@)/0JDEF<G%<+EV:2F!,O<DJ+*.@<*K0@<6L(Df-\\0Ec5e;DffZ(EZee.Bl.9pF\"AGXBPCsi+DGm>@3BB/F*&OCAfu2/AKYi(DIb:@FD,*)+C]U=@3BN#EcYf8ATD3s@q?d$AftVqCh[NqF<G:8+EV:.+Cf>-FD5W8ARlolDIal(DId<j@<?3r@:F%a+D58'ATD4$Bl@l3De:,-DJs`8ARoFb/0JMK@qB4^F!,R<AKZ&-DfTqBG%G>uD.RTpAKYo'+CT/5+Cei#DII?(E,9)oF*2M7/c~>".to_vec();
        let map = NameMap::of(vec![
            PdfObject::Name(b"Filter".to_vec()), PdfObject::Name(b"ASCII85Decode".to_vec()),
            PdfObject::Name(b"Length".to_vec()), PdfObject::Integer(341)]).unwrap();
        let data = b"Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.".to_vec();

        match decode(&input, &map) {
            Ok(retval) => {
                assert_eq!(data, retval);
            },
            anything_else => {
                println!("got something horrible {:#?}", anything_else);
                assert_eq!(111, 0);
            }
        }

    }
}
