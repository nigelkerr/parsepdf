extern crate inflate;
extern crate lzw;
extern crate nom;

use nom::*;

// given a filter, possibly some parameters, and some bytes,
// return either some decoded bytes, or an error.
// something over in the main library figures out how to get here
// from the pdf objects themselves.

// this wants nice chainability.

use crate::simple::bare_hexadecimal_string;
use crate::simple::is_pdf_whitespace;
use crate::simple::skip_whitespace;
use std::io;
use crate::structs::PdfObject;

#[derive(Debug, PartialEq, Eq)]
pub enum DecodingResponse {
    NotImplementedYet,
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
        _ => Err(DecodingResponse::DecodeError),
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
    is_ascii85_digit(chr) || is_pdf_whitespace(chr)
}

// we are trusting that what came before worked out...
fn byte_vec_from_ascii85_string(input: &[u8]) -> Result<Vec<u8>, nom::ErrorKind> {
    let mut result: Vec<u8> = Vec::new();
    let filtered: Vec<u8> = input
        .iter()
        .filter(|&x| is_ascii85_digit(*x))
        .map(|&x| x)
        .collect();

    let mut pos = 0;
    let length = filtered.len();

    while pos < length {
        if filtered[pos] == 0x7A {
            // z special case
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
        let mut v: u64 = ((filtered[pos] - ADDEND) as u64 * C_85_4)
            + ((filtered[pos + 1] - ADDEND) as u64 * C_85_3);

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
                v += ((filtered[pos + 2] - ADDEND) as u64 * C_85_2)
                    + ((filtered[pos + 3] - ADDEND) as u64 * C_85_1) + PADDING;
            }
            5 => {
                v += ((filtered[pos + 2] - ADDEND) as u64 * C_85_2)
                    + ((filtered[pos + 3] - ADDEND) as u64 * C_85_1)
                    + (filtered[pos + 4] - ADDEND) as u64;
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
        Ok((_remainder, v)) => Ok(v),
        Err(_) => Err(DecodingResponse::DecodeError),
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
        let (start, bytes) = decoder.dispatch_decode_bytes(&compressed).unwrap();
        compressed = &compressed[start..];
        result.extend_from_slice(bytes);
    }

    Ok(result)
}

pub fn decode_flate(input: &Vec<u8>) -> Result<Vec<u8>, DecodingResponse> {
    match inflate::inflate_bytes_zlib(input.as_bytes()) {
        Ok(result) => Ok(result),
        Err(_s) => Err(DecodingResponse::DecodeError),
    }
}

named!(decode_rle_literal<&[u8], Vec<u8>>,
    do_parse!(
        v: re_bytes_capture!(r"^([\x00-\x7f])") >>
        w: take!((v[0][0]+1) as u8) >>
        ( w.to_vec() )
    )
);

named!(decode_rle_copy<&[u8], Vec<u8>>,
        do_parse!(
            v: one_of!([0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f, 0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f, 0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7, 0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf, 0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7, 0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf, 0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7, 0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf, 0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7, 0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf, 0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef, 0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff].as_ref()) >>
            w: take!(1) >>
            ( vec![w[0]; ((257 as u16) - (v as u16)) as usize] )
        )
);

named!(decode_rle_main<&[u8], Vec<u8>>,
    do_parse!(
        v: fold_many0!(
            alt!(
                complete!(decode_rle_copy) |
                complete!(decode_rle_literal)
            ),
            Vec::new(),
            |mut accum: Vec<u8>, item: Vec<u8>| {
                accum.extend(&item);
                accum
            }
        ) >>
        tag!(b"\x80") >>
        ( v )
    )
);

pub fn decode_rle(input: &Vec<u8>) -> Result<Vec<u8>, DecodingResponse> {
    match decode_rle_main(&input[..]) {
        Ok((_, v)) => Ok(v),
        Err(_) => Err(DecodingResponse::DecodeError),
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
    refuse_to_decode()
}

pub fn apply_tiff_predictor_function(
    input: &Vec<u8>,
    colors: u32,
    bits_per_component: u32,
    columns: u32,
) -> Result<Vec<u8>, DecodingResponse> {
    let slinput = input.as_slice();
    let mut result: Vec<u8> = Vec::new();
    let bytes_per_row: usize = ((bits_per_component * colors * columns) as usize + 7) >> 3;
    let rows: usize = input.len() / bytes_per_row as usize;

    for row in 0..rows {
        let mut rinput = &slinput[(bytes_per_row * row)..(bytes_per_row * (row + 1))];
        let mut current_row_unpacked: Vec<u32> = Vec::new();
        let mut bits: usize = 0;
        // copy accross samples of the first pixel
        for _ in 0..colors {
            match take_bits!((rinput, bits), u32, bits_per_component as usize) {
                Ok(((ninput, nbits), value)) => {
                    current_row_unpacked.push(value);
                    rinput = ninput;
                    bits = nbits;
                }
                _ => {
                    return Err(DecodingResponse::DecodeError);
                }
            }
        }

        // deal with the rest of the pixels, referring to recently added
        for col in 0..((columns - 1) * colors) {
            match take_bits!((rinput, bits), u32, bits_per_component as usize) {
                Ok(((ninput, nbits), value)) => {
                    let prev = current_row_unpacked[col as usize];
                    // the modulus keeps the value within the range of bits_per_component
                    current_row_unpacked.push((value + prev) % (2_u32.pow(bits_per_component)));
                    rinput = ninput;
                    bits = nbits;
                }
                _ => {
                    return Err(DecodingResponse::DecodeError);
                }
            }
        }

        // now pack current row unpacked into the result vec<u8>

        if bits_per_component % 8 == 0 {
            let bytes_per_component = bits_per_component >> 3;
            for code in current_row_unpacked.iter() {
                for i in (0..bytes_per_component).rev() {
                    result.push(((code >> (i * 8)) & 0x000000ff) as u8);
                }
            }
        } else {
            let mut wip_byte: u8 = 0;
            let mut wip_bits: u32 = 0;

            'outer: for code in current_row_unpacked.iter() {
                let mut bits_left = bits_per_component;

                // eliminate wip if we can, possibly making new wip
                if wip_bits > 0 {
                    if bits_left < (8 - wip_bits) {
                        wip_byte = wip_byte | ((code << ((8 - wip_bits) - bits_left)) as u8);
                        wip_bits += bits_left;
                        continue 'outer; // because we need to fill up the byte
                    } else {
                        wip_byte = wip_byte
                            | (((code >> (bits_left - (8 - wip_bits))) & 0x000000ff) as u8);
                        result.push(wip_byte);
                        bits_left = bits_left - (8 - wip_bits);
                        wip_byte = 0;
                        wip_bits = 0;
                    }
                }

                while bits_left > 8 {
                    result.push(((code >> (bits_left - 8)) & 0x000000ff) as u8);
                    bits_left = bits_left - 8;
                }
                wip_byte = ((code << (8 - bits_left)) & 0x000000ff) as u8;
                wip_bits = bits_left as u32;
            }

            if wip_bits != 0 {
                result.push(wip_byte); // and her leftover bits are 0
            }
        }
    }

    Ok(result)
}

fn paeth_prediction( a: u8, b: u8, c: u8 ) -> u8 {

    let p: i16 = a as i16 + b as i16 - c as i16;
    let pa: u16 = (p - a as i16).abs() as u16;
    let pb: u16 = (p - b as i16).abs() as u16;
    let pc: u16 = (p - c as i16).abs() as u16;

    if pa <= pb && pa <= pc {
        return a;
    } else if pb <= pc {
        return b;
    }
    return c;
}

pub fn apply_png_predictor_function(
    input: &Vec<u8>,
    colors: u32,
    bits_per_component: u32,
    columns: u32,
) -> Result<Vec<u8>, DecodingResponse> {
    let slinput = input.as_slice();
    let mut result: Vec<u8> = Vec::new();
    let bytes_per_row: usize = ((bits_per_component * colors * columns) as usize + 7) >> 3;
    let bytes_per_pixel: usize = ((bits_per_component * colors) as usize + 7) >> 3;
    let bytes_per_row_with_predictor: usize = bytes_per_row + 1;
    let rows: usize = input.len() / (bytes_per_row_with_predictor) as usize;

    // we need a previous row, and the current row.
    // the initial previous row is 0's, not from the input.
    // there is always a pixel's worth of 0's to the left of each row.
    let mut prev: Vec<u8> = vec![0u8; bytes_per_row];
    let mut next: Vec<u8> = vec![0u8; bytes_per_row];

    for row in 0..rows {
        let start = bytes_per_row_with_predictor * row;
        let end = bytes_per_row_with_predictor * ( row + 1 );
        let predictor = &slinput[start..(start+1)];
        let mut rinput = &slinput[(start+1)..end];

        match predictor.get(0) {
            Some(1u8) => { // png sub
                for i in bytes_per_pixel .. bytes_per_row {
                    let rvalue = rinput[i];
                    let left_rvalue = next[i - bytes_per_pixel];
                    next[i] = rvalue.wrapping_add( left_rvalue );
                }
            },
            Some(2u8) => { // png up
                // we dont really need to worry about pix width here
                // since it is all up all the time.
                for i in 0..bytes_per_row {
                    let rvalue = rinput[i];
                    next[i] = rvalue.wrapping_add( prev[i]) ;
                }
            },
            Some(3u8) => { // png avg
                // the inline if expr might be slow, but it is clear to me on reading.
                for i in 0..bytes_per_row {
                    let up_value = prev[i];
                    let left_rvalue = if i < bytes_per_pixel { 0 } else { next[i - bytes_per_pixel] };
                    let rvalue = rinput[i];
                    let interm = ((up_value as u16).wrapping_add(left_rvalue as u16) >> 1) as u8;
                    next[i] = rvalue.wrapping_add(interm);
                }
            },
            Some(4u8) => { // png paeth
                for i in 0..bytes_per_row {

                    let up_value = prev[i];
                    let left_rvalue = if i < bytes_per_pixel { 0 } else { next[i - bytes_per_pixel] };
                    let left_up_rvalue = if i < bytes_per_pixel { 0 } else { prev[i - bytes_per_pixel] };
                    let rvalue = rinput[i];
                    next[i] = rvalue.wrapping_add(paeth_prediction(left_rvalue, up_value, left_up_rvalue));
                }
            }
            _ => {
                println!("not implemented yet, so just copying");
                for i in 0..bytes_per_row {
                    let rvalue = rinput[i];
                    next[i] = rvalue;
                }
            }
        }
        result.extend(next.clone());
        prev = next;
        next = vec![0u8; bytes_per_row];

    }

    Ok(result)
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_decode_asciihex() {
        assert_eq!(
            Ok(b"@@"[..].to_owned()),
            decode_asciihex(&b"4040>"[..].to_owned())
        );
        assert_eq!(
            Ok(b"@@"[..].to_owned()),
            decode_asciihex(&b"4 0  4\n0\t>"[..].to_owned())
        );
    }

    #[test]
    fn test_decode_ascii85() {
        assert_eq!(
            Ok(b"\x00\x00\x00\x00"[..].to_owned()),
            decode_ascii85(&b"z~>"[..].to_owned())
        );

        assert_eq!(Ok(b"Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure."[..].to_owned()),
                   decode_ascii85(&b"9jqo^BlbD-BleB1DJ+*+F(f,q/0JhKF<GL>Cj@.4Gp$d7F!,L7@<6@)/0JDEF<G%<+EV:2F!,O<DJ+*.@<*K0@<6L(Df-\\0Ec5e;DffZ(EZee.Bl.9pF\"AGXBPCsi+DGm>@3BB/F*&OCAfu2/AKYi(DIb:@FD,*)+C]U=@3BN#EcYf8ATD3s@q?d$AftVqCh[NqF<G:8+EV:.+Cf>-FD5W8ARlolDIal(DId<j@<?3r@:F%a+D58'ATD4$Bl@l3De:,-DJs`8ARoFb/0JMK@qB4^F!,R<AKZ&-DfTqBG%G>uD.RTpAKYo'+CT/5+Cei#DII?(E,9)oF*2M7/c~>"[..].to_owned())
        )
    }

    #[test]
    fn test_decode_lzw() {
        // finding actual PDF examples of LZW on the ground difficult

        let input: Vec<u8> = vec![0x80, 0x0b, 0x60, 0x50, 0x22, 0x0c, 0x0c, 0x85, 0x01];
        let data = b"-----A---B".to_vec();

        assert_eq!(Ok(data), decode_lzw(&input, true));

        let data = include_bytes!("../assets/lzw.bitstream");
        assert_eq!(
            Ok(b"q\r660 0 0 907 0 0 cm\r/Im1 Do\rQ\r".to_vec()),
            decode_lzw(&data[..].to_vec(), true)
        );
    }

    #[test]
    fn test_decode_flate() {
        let data = include_bytes!("../assets/flate.bitstream");
        assert_eq!(
            Ok(b"q\nBT\n36 806 Td\nET\nQ\nq 1 0 0 1 0 0 cm /Xf1 Do Q\n".to_vec()),
            decode_flate(&data[..].to_vec())
        );
    }

    macro_rules! decode_rle_l_test {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let (input, (expected_rest, expected_result)) = $value;
                match decode_rle_literal(input) {
                    Ok((rest, result)) => {
                        assert_eq!((expected_rest.as_bytes(), expected_result.to_vec()),
                            (rest, result));
                    },
                    _ => {
                        assert_eq!(1, 0);
                    }
                }
            }
        )*
        }
    }

    decode_rle_l_test! {
        drlt1: (b"\x00bc"[..].as_bytes(), (b"c", b"b")),
        drlt2: (b"\x01bcd"[..].as_bytes(), (b"d", b"bc")),
        drlt3: (b"\x02bcd"[..].as_bytes(), (b"", b"bcd")),
        drlt4: (b"\x10abcdefghijklmnopqrstuvwxyz"[..].as_bytes(), (b"rstuvwxyz", b"abcdefghijklmnopq")),
    }

    macro_rules! decode_rle_c_test {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let (input, (expected_rest, expected_result)) = $value;
                match decode_rle_copy(input) {
                    Ok((rest, result)) => {
                        assert_eq!((expected_rest.as_bytes(), expected_result.to_vec()),
                            (rest, result));
                    },
                    _ => {
                        assert_eq!(2, 0);
                    }
                }
            }
        )*
        }
    }

    decode_rle_c_test! {
        drct_1: (b"\xffbc"[..].as_bytes(), (b"c", b"bb")),
        drct_2: (b"\xf0bc"[..].as_bytes(), (b"c", b"bbbbbbbbbbbbbbbbb")),
    }

    #[test]
    fn decode_rle_test() {
        assert_eq!(
            Ok(b"bbc".to_vec()),
            decode_rle(&b"\xffb\x00c\x80"[..].to_vec())
        );
    }

    #[test]
    fn basic_tiff_predictor_test() {
        // 2 colors, 8 bits, 3 columns
        let input: Vec<u8> = vec![0x80, 0x00, 0x01, 0x00, 0x03, 0x00];
        let output: Vec<u8> = vec![0x80, 0x00, 0x81, 0x00, 0x84, 0x00];
        assert_eq!(Ok(output), apply_tiff_predictor_function(&input, 2, 8, 3));

        // 2 colors, 4 bits, 4 columns
        let input: Vec<u8> = vec![0x80, 0x00, 0xf0, 0x00];
        let output: Vec<u8> = vec![0x80, 0x80, 0x70, 0x70];
        assert_eq!(Ok(output), apply_tiff_predictor_function(&input, 2, 4, 4));

        // 2 colors, 4 bits, 2 columns (and there by 2 rows...)
        let input: Vec<u8> = vec![0x80, 0x00, 0xf0, 0x00];
        let output: Vec<u8> = vec![0x80, 0x80, 0xf0, 0xf0];
        assert_eq!(Ok(output), apply_tiff_predictor_function(&input, 2, 4, 2));

        // 2 colors, 7 bits, 3 columns
        // this one not strictly necessary (the spec only allows 1, 2, 4, 8, or 16 BitsPerComponent
        // but this demonstrates nicely that we're doing the unpacking and modulo arithmetic correct.
        // there probably needs to be a keyhole on the bits-per-component.
        let input: Vec<u8> = vec![0xfe, 0x00, 0x08, 0x00, 0x60, 0x00];
        let output: Vec<u8> = vec![0xfe, 0x00, 0x00, 0x00, 0x60, 0x00];
        assert_eq!(Ok(output), apply_tiff_predictor_function(&input, 2, 7, 3));

        // how do i test failure modes here?  what is a failure mode for
        // these predictor functions?
    }

    #[test]
    fn basic_png_predictor_test_xrefstm() {
        // these data given as  /DecodeParams <</Columns 4 /Predictor 12 >> /W [1 2 1]
        let flate_data = include_bytes!("../assets/flate_predictor12_4columns.bitstream").to_vec();
        let input_data = decode_flate(&flate_data).unwrap();
        let output_text = String::from_utf8(include_bytes!("../assets/flate_predictor12_4columns.decoded_values").to_vec()).unwrap();
        let output_array =
            output_text.lines().map(|l| l.split_whitespace().map(|w| w.parse::<u32>().unwrap() ).collect::<Vec<u32>>() ).collect::<Vec<Vec<_>>>();

        match apply_png_predictor_function(&input_data, 1, 8, 4) {
            Ok(decoded) => {
                let mut slice_decoded = decoded.as_slice();
                for row in output_array.iter() {
                    let (rest, x1) = be_u8(slice_decoded).unwrap();
                    slice_decoded = rest;
                    let (rest, x2) = be_u16(slice_decoded).unwrap();
                    slice_decoded = rest;
                    let (rest, x3) = be_u8(slice_decoded).unwrap();
                    slice_decoded = rest;

                    assert_eq!(x1 as u32, row[0]);
                    assert_eq!(x2 as u32, row[1]);
                    assert_eq!(x3 as u32, row[2]);
                }
            },
            Err(err) => {
                println!("got: {:?}", err);
                assert_eq!(1,6);
            },
        }
    }

    // am finding image examples with interesting /DecodeParams combinations thin on the ground for some reason.
}
