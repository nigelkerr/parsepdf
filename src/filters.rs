use crate::parser::{recognize_asciihex_hexadecimal_string, PdfObject};
use crate::recognize_ascii85_string;

quick_error! {
    #[derive(Debug, PartialEq, Clone)]
    pub enum DecodingResponse {
        NotImplementedYet {}
        RefuseToDecode {}
        DecodeError {}
        DecoderInitializationError {}
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

#[cfg(test)]
mod tests {
    use super::*;
    use nom::AsBytes;

    #[test]
    fn test_decode_asciihex() {
        assert_eq!(Ok(b"@@".to_vec()), decode_asciihex(&b"4040>".to_vec()));
        assert_eq!(
            Ok(b"@@".to_vec()),
            decode_asciihex(&b"4 0  4\n0\t>".to_vec())
        );
    }

    #[test]
    fn test_decode_ascii85() {
        assert_eq!(
            Ok(b"\x00\x00\x00\x00".to_vec()),
            decode_ascii85(&b"z~>".to_vec())
        );

        assert_eq!(Ok(b"Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.".to_vec()),
                   decode_ascii85(&b"9jqo^BlbD-BleB1DJ+*+F(f,q/0JhKF<GL>Cj@.4Gp$d7F!,L7@<6@)/0JDEF<G%<+EV:2F!,O<DJ+*.@<*K0@<6L(Df-\\0Ec5e;DffZ(EZee.Bl.9pF\"AGXBPCsi+DGm>@3BB/F*&OCAfu2/AKYi(DIb:@FD,*)+C]U=@3BN#EcYf8ATD3s@q?d$AftVqCh[NqF<G:8+EV:.+Cf>-FD5W8ARlolDIal(DId<j@<?3r@:F%a+D58'ATD4$Bl@l3De:,-DJs`8ARoFb/0JMK@qB4^F!,R<AKZ&-DfTqBG%G>uD.RTpAKYo'+CT/5+Cei#DII?(E,9)oF*2M7/c~>".to_vec())
        )
    }

}
