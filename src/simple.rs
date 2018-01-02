extern crate nom;

use nom::*;
use nom::digit;
use nom::ErrorKind;
use nom::IResult::*;
use std::str;
use std::str::FromStr;

use structs::ErrorCodes;
use structs::PdfObject;
use structs::PdfVersion;

named!(pub pdf_line_ending_by_macro<&[u8],&[u8]>,
    alt!(
        complete!(tag!(b"\r\n")) |
        complete!(tag!(b"\r")) |
        complete!(tag!(b"\n"))
    )
);

named!(pdf_version<&[u8],&[u8]>,
    re_bytes_find!("^(1\\.[01234567]|2\\.0)")
);

named!(pub pdf_magic<&[u8],PdfVersion>,
    do_parse!(
        tag!(b"%PDF-") >>
        ver_bytes: pdf_version >>
        pdf_line_ending_by_macro >>
        ( PdfVersion::Known{ ver: ver_bytes.to_vec() } )
    )
);

// § 7.5.2

named!(pub pdf_header<&[u8],PdfVersion>,
    do_parse!(
        ver: pdf_magic >>
        opt!( comment ) >>
        ( ver )
    )
);



#[inline]
pub fn is_not_line_end_chars(chr: u8) -> bool {
    (chr != b'\n' && chr != b'\r')
}

// § 7.3.2
named!(pub boolean<&[u8],PdfObject>,
    do_parse!(
        v: map_res!(map_res!(alt!(
                tag!(b"true") |
                tag!(b"false")
                ), str::from_utf8), FromStr::from_str)
        >>
        (PdfObject::Boolean ( v ) )
    )
);

// § 7.3.3

named!(maybe_signed_integer<&[u8],(Option<&[u8]>, &[u8])>,
    pair!(
        opt!(alt!(tag_s!("+") | tag_s!("-"))),
        nom::digit
    )
);
named!(recognize_signed_integer<&[u8],&[u8]>,
    recognize!(
        maybe_signed_integer
    )
);
named!(pub signed_integer<&[u8],PdfObject>,
    do_parse!(
        v: recognize_signed_integer
        >>
        (PdfObject::Integer ( FromStr::from_str(str::from_utf8(v).unwrap()).unwrap() ) )
    )
);


named!(maybe_signed_float_ap<&[u8],(Option<&[u8]>,&[u8],&[u8],Option<&[u8]>)>,
    tuple!(
        opt!(alt!(tag!(b"+") | tag!(b"-"))),
        digit,
        tag!(b"."),
        opt!(complete!(digit))
    )
);

named!(maybe_signed_float_pp<&[u8],(Option<&[u8]>,&[u8],&[u8],Option<&[u8]>)>,
    tuple!(
        opt!(alt!(tag!(b"+") | tag!(b"-"))),
        tag!(b"."),
        digit,
        opt!(complete!(digit))
    )
);

named!(recognize_signed_float<&[u8],&[u8]>,
    recognize!(
        alt!(
            complete!( maybe_signed_float_ap ) |
            complete!( maybe_signed_float_pp )
        )
    )
);

named!(pub signed_float<&[u8],PdfObject>,
    do_parse!(
        v: recognize_signed_float
        >>
        (PdfObject::Float ( FromStr::from_str(str::from_utf8(v).unwrap()).unwrap()))
    )
);

// comments § 7.2.4

// comments are going to by my undoing, given where all they can occur

named!(pub recognize_comment<&[u8],&[u8]>,
    recognize!(
        tuple!(
            tag!(b"%"),
            take_while!( is_not_line_end_chars ),
            pdf_line_ending_by_macro
        )
    )
);

fn byte_vec_from_comment(input: &[u8]) -> Result<Vec<u8>, nom::ErrorKind> {
    let mut result: Vec<u8> = Vec::new();

    // skip the single % starting the comment.
    for item in input.iter().skip(1) {
        if is_not_line_end_chars(*item) {
            result.push(*item);
        }
    }

    Ok(result)
}

// just the bytes of the comment itself, not the delimiting
named!(pub comment<&[u8],PdfObject>,
    do_parse!(
        v: map_res!( recognize_comment, byte_vec_from_comment )
        >>
        ( PdfObject::Comment ( v ) )
    )
);

// "Table 1 -- White space characters"
#[inline]
pub fn is_pdf_whitespace(chr: u8) -> bool {
    (chr == 0x00 ||
        chr == 0x09 ||
        chr == 0x0A ||
        chr == 0x0C ||
        chr == 0x0D ||
        chr == 0x20
    )
}

named!(pub recognize_some_ws<&[u8],&[u8]>,
    recognize!(
        take_while!(is_pdf_whitespace)
    )
);

pub fn skip_whitespace(input: &[u8]) -> usize {
    let ws_iresult = recognize_some_ws(input);
    match ws_iresult {
        Done(_, recognized) => {
            return recognized.len();
        }
        _ => {
            return 0;
        }
    }
}

// § 7.3.4.3 Hexadecimal Strings
// return here a slice of the final value

#[inline]
fn can_be_in_hexadecimal_string(chr: u8) -> bool {
    is_hex_digit(chr) ||
        is_pdf_whitespace(chr)
}

named!(maybe_hexadecimal_string<&[u8],&[u8]>,
    delimited!(
        tag!("<"),
        take_while!( can_be_in_hexadecimal_string ),
        tag!(">")
    )
);

#[inline]
pub fn from_hex(chr: u8) -> u8 {
    // heh
    if chr >= 0x30 && chr <= 0x39 {
        chr - 0x30
    } else if chr >= 0x41 && chr <= 0x46 {
        chr - 0x37
    } else {
        chr - 0x57
    }
}

// we are trusting that what came before worked out...
fn byte_vec_from_hexadecimal_string(input: &[u8]) -> Result<Vec<u8>, nom::ErrorKind> {
    let mut result: Vec<u8> = Vec::new();
    let filtered: Vec<u8> = input.iter().filter(
        |&x| nom::is_hex_digit(*x)
    ).map(|&x| from_hex(x)).collect();

    for pair in filtered.chunks(2) {
        match pair.len() {
            2 => { result.push((pair[0] << 4) + pair[1]); }
            1 => { result.push((pair[0] << 4)); }
            _ => return Err(nom::ErrorKind::Custom(ErrorCodes::UnexpectedHexDecodingSituation as u32)),
        }
    }

    Ok(result)
}

named!(pub hexadecimal_string<&[u8],PdfObject>,
    do_parse!(
        v: map_res!( maybe_hexadecimal_string, byte_vec_from_hexadecimal_string)
        >>
        ( PdfObject::String( v ) )
    )
);



// § 7.3.4.2 Literal Strings

/// given the current state, could we have more octal digits
/// if we'd had this much octal so far?
fn could_have_more_octal_digits(v: i32, digits: usize) -> bool {
    let mut retval: bool = false;

    if digits < 3 {
        retval = true;
    }

    if v > 31 {
        // which is to say, greater than octal 37
        retval = false;
    }

    retval
}

/// determine if we have a valid pdf literal string, and
/// return the full sequence of bytes from opening paren
/// to closing paren.
/// no treatment of leading or following whitespace, handle
/// that outside.
/// we're going to need to have a crassly similar loop for
/// taking this byte-sequence and understanding it as a
/// character sequence.


/// make a sequence of bytes from the raw byte sequence of
/// the PDF literal string.
fn recognize_literal_string(input: &[u8]) -> IResult<&[u8], Vec<u8> > {
    let mut result: Vec<u8> = Vec::new();

    let input_length = input.input_len();
    if input_length == 0 {
        return Incomplete(Needed::Unknown);
    }

    // what depth of balanced unescaped parens are we at?
    // starts our iteration at 0, and it ought to be 0
    // when we end (so, the delimiting paren is the first
    // and last we encounter and manipulate this variable
    // for...)
    let mut parens_depth: usize = 0;

    // did we just see an escaping backslash?
    let mut was_escape_char: bool = false;
    let mut was_escaped_carriage_return: bool = false;

    // we want to accept the
    // longest valid octal, even in the face of the escape
    // sequence being followed by other digits
    let mut current_octal_value: i32 = -1;
    let mut number_octal_digits: usize = 0;
    let mut index = 0;

    for (idx, item) in input.iter_indices() {
        let chr = *item;
        index = idx;

        if was_escaped_carriage_return {
            was_escaped_carriage_return = false;
            match chr {
                b'\n' => {
                    continue;
                }
                _ => {
                    // glam! this wasnt involved in line ending and can proceed
                }
            }
        }

        if was_escape_char {
            match chr {
                b'\n' => {
                    // glam! push nothing and carry on
                }
                b'\r' => {
                    // we have to worry about \r vs \r\n here,
                    // they both MUST be rendered into the result as \n
                    was_escaped_carriage_return = true;
                }
                b'n' => {
                    result.push(b'\n')
                }
                b'r' => {
                    result.push(b'\r')
                }
                b't' => {
                    result.push(b'\t')
                }
                b'b' => {
                    result.push(0x08 as u8)
                }
                b'f' => {
                    result.push(0x0c as u8)
                }
                b'(' | b')' | b'\\' => {
                    result.push(chr);
                }
                b'0' | b'1' | b'2' | b'3' | b'4' | b'5' | b'6' | b'7' => {
                    number_octal_digits = 1;
                    current_octal_value = (chr as u8 - '0' as u8) as i32;
                }
                _ => {
                    // anything outside the above gets us an error
                    return Error(error_position!(ErrorKind::Custom(ErrorCodes::UnrecognizedEscapeSequence as u32), input));
                }
            }
            was_escape_char = false;
            continue;
        }

        if number_octal_digits > 0 {
            if could_have_more_octal_digits(current_octal_value, number_octal_digits) &&
                (chr == b'0' || chr == b'1' || chr == b'2' || chr == b'3' || chr == b'4' || chr == b'5' || chr == b'6' || chr == b'7') {
                number_octal_digits += 1;
                current_octal_value = (current_octal_value << 3) + ((chr as u8 - '0' as u8) as i32);
                continue;
            } else {
                // glam! reset octal
                result.push(current_octal_value as u8);
                number_octal_digits = 0;
                current_octal_value = -1;
            }
        }

        match chr {
            b'(' => {
                if parens_depth > 0 {
                    result.push(chr);
                }
                parens_depth += 1;
            }
            b')' => {
                parens_depth -= 1;
                if parens_depth > 0 {
                    result.push(chr);
                }
                if parens_depth == 0 {
                    break;
                }
            }
            b'\\' => {
                was_escape_char = true;
            }
            _ => {
                result.push(chr);
            }
        }
    }

    // i can see how we could get here with the _ branch, but
    // not the 0 branch
    match parens_depth {
        0 => {}
        _ => { return Incomplete(Needed::Unknown); }
    }

    Done(input.slice(index..), result)
}

named!(pub literal_string<&[u8],PdfObject>,
    do_parse!(
        v: recognize_literal_string
        >>
        ( PdfObject::String( v ) )
    )
);

// null object § 7.3.9

named!(pub recognize_null_object<&[u8],&[u8]>,
    recognize!(tag!(b"null"))
);

named!(pub null_object<&[u8],PdfObject>,
    do_parse!(
        recognize_null_object >>
        (PdfObject::Null)
    )
);

// § 7.3.5 Name objects

/// recognize a Name object, returning a vec of the bytes of the decoded form of the name.
pub fn recognize_name_object(input: &[u8]) -> IResult<&[u8], Vec<u8>>
{
    let mut result: Vec<u8> = Vec::new();

    let input_length = input.input_len();
    if input_length == 0 {
        return Incomplete(Needed::Unknown);
    }

    //
    let mut first_iteration: bool = true;

    // was the previous character the number_sign, after which
    // we should expect exactly two hex digits ?
    let mut was_number_sign: bool = false;
    let mut count_hex_digits: usize = 0;
    let mut current_hex_value: u8 = 0;

    for (idx, item) in input.iter_indices() {
        let chr = *item;

        // did we start right?
        if first_iteration {
            match chr {
                b'/' => {
                    first_iteration = false;
                    continue;
                }
                _ => {
                    return Error(error_position!(ErrorKind::Custom(ErrorCodes::NameNotStartWithSlash as u32), input));
                }
            }
        }

        if was_number_sign {
            match count_hex_digits {
                0 | 1 => {
                    match is_hex_digit(chr as u8) {
                        true => {
                            current_hex_value = (current_hex_value << 4) + from_hex(chr as u8);
                            count_hex_digits += 1;

                            if count_hex_digits > 1 {
                                result.push(current_hex_value);
                                current_hex_value = 0;
                                count_hex_digits = 0;
                                was_number_sign = false;
                            }
                            continue;
                        }
                        false => {
                            return Error(error_position!(ErrorKind::Custom(ErrorCodes::ExpectedHexDigit as u32), input));
                        }
                    }
                }
                // how will we ever get here?
                _ => {
                    return Error(error_position!(ErrorKind::Custom(ErrorCodes::TooManyHexDigits as u32), input));
                }
            }
        }

        match chr {
            b'#' => {
                was_number_sign = true;
                continue;
            }
            b'\x00' => {
                return Error(error_position!(ErrorKind::Custom(ErrorCodes::HexStringIncludesZeroByte as u32), input));
            }
            b'\n' | b'\r' | b'\t' | b' ' | b'\x0C' | b'/' | b'(' | b')' | b'<' | b'>' | b'[' | b']' | b'{' | b'}' | b'%' => {
                // unescaped whitespace and unescaped delimiters end the name
                return Done(input.slice(idx..), result);
            }
            b'\x21' ... b'\x7e' => {
                result.push(chr);
                // glam!
            }
            _ => {
                // these ought to have been # encoded
                return Error(error_position!(ErrorKind::Custom(ErrorCodes::ByteValueOughtToHaveBeenHexEncoded as u32), input));
            }
        }
    }

    if was_number_sign {
        return Incomplete(Needed::Unknown);
    }

    // ... wut?  i guess we get here if the end of
    // current input is indistinguishable from
    // the end of a name possibly.
    Done(input.slice(input_length..), result)
}

/// return the name itself expanded to un-escaped form
named!(pub name_object<&[u8],PdfObject>,
    do_parse!(
        v: recognize_name_object
        >>
        ( PdfObject::Name( v ) )
    )
);

// indirect references § 7.3.10

named!(pub indirect_reference<&[u8],PdfObject>,
    do_parse!(
        num: map_res!(map_res!(re_bytes_find!(r"^[123456789]\d*"), str::from_utf8), FromStr::from_str) >>
        tag!(b" ") >>
        gen: map_res!(map_res!(recognize!(digit), str::from_utf8), FromStr::from_str) >>
        tag!(b" R") >>
        (
            PdfObject::IndirectReference{ number: num, generation: gen }
        )
    )
);



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn indirect_reference_test() {
        assert_eq!(PdfObject::IndirectReference { number: 95, generation: 1 },
                   indirect_reference(b"95 1 R ".as_bytes()).to_result().unwrap());
        assert_eq!(PdfObject::IndirectReference { number: 95, generation: 100 },
                   indirect_reference(b"95 100 R ".as_bytes()).to_result().unwrap());
        assert_eq!(Err(nom::Err::Code(nom::ErrorKind::RegexpFind)), indirect_reference(b"09 1 R".as_bytes()).to_result());
    }

    #[test]
    fn null_test() {
        match null_object(b"null").to_result().unwrap() {
            PdfObject::Null => {},
            _ => {
                assert_eq!(3, 0);
            }
        }
        match null_object(b"nul") {
            Incomplete(_) => {}
            _ => {
                assert_eq!(1, 0);
            }
        }
        match null_object(b"mul") {
            Error(_) => {}
            _ => {
                assert_eq!(2, 0);
            }
        }
    }

    #[test]
    fn boolean_test() {
        assert_eq!(PdfObject::Boolean(true), boolean(b"true ").to_result().unwrap());
        assert_eq!(PdfObject::Boolean(false), boolean(b"false").to_result().unwrap());
    }

    #[test]
    fn magic_test() {
        assert_eq!(PdfVersion::Known { ver: b"1.0".to_vec() }, pdf_magic(b"%PDF-1.0\r\n").to_result().unwrap());
        assert_eq!(nom::Err::Position(nom::ErrorKind::Tag, &[98u8, 108u8, 97u8, 104u8][..]),
                   pdf_magic(b"blah").to_result().unwrap_err());
        assert_eq!(nom::Err::Code(nom::ErrorKind::RegexpFind),
                   pdf_magic(b"%PDF-3.0\r").to_result().unwrap_err());
    }

    #[test]
    fn linendings_by_macro_test() {
        assert_eq!(b"\r".as_bytes(), pdf_line_ending_by_macro(b"\rdd".as_bytes()).to_result().unwrap());
        assert_eq!(b"\r\n".as_bytes(), pdf_line_ending_by_macro(b"\r\ndd".as_bytes()).to_result().unwrap());
        assert_eq!(b"\n".as_bytes(), pdf_line_ending_by_macro(b"\ndd".as_bytes()).to_result().unwrap());
    }

    #[test]
    fn comments_test() {
        match comment(b"%hiya\n").to_result().unwrap() {
            PdfObject::Comment(v) => {
                assert_eq!(v, "hiya".as_bytes());
            },
            _ => { assert_eq!(6, 0); }
        }
        match comment("%なななな\n".as_bytes()).to_result().unwrap() {
            PdfObject::Comment(v) => {
                assert_eq!(v, "なななな".as_bytes());
            },
            _ => { assert_eq!(6, 0); }
        }
    }

    #[test]
    fn header_test() {
        assert_eq!(nom::IResult::Done(b" ".as_bytes(), PdfVersion::Known { ver: b"1.0".to_vec() }),
                   pdf_header(b"%PDF-1.0\r ".as_bytes()));
        assert_eq!(nom::IResult::Done(b" ".as_bytes(), PdfVersion::Known { ver: b"2.0".to_vec() }),
                   pdf_header("%PDF-2.0\r%なななな\n ".as_bytes()));
    }

    #[test]
    fn integers_test() {
        assert_eq!(PdfObject::Integer(123), signed_integer(b"123").to_result().unwrap());
        assert_eq!(PdfObject::Integer(-123), signed_integer(b"-123").to_result().unwrap());
        assert_eq!(PdfObject::Integer(0), signed_integer(b"0").to_result().unwrap());
        assert_eq!(PdfObject::Integer(0), signed_integer(b"-0").to_result().unwrap()); // heh
    }
    macro_rules! float_test {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let (input, expected) = $value;
                match signed_float(input).to_result().unwrap() {
                    PdfObject::Float(v) => {
                        assert_relative_eq!(expected, v);
                    },
                    _ => {
                        assert_eq!(5, 0);
                    }
                }
            }
        )*
        }
    }

    float_test! {
        f1: (b"123.0",123.0),
        f2: (b"34.5",34.5),
        f3: (b"-3.62",-3.62),
        f4: (b"+123.6",123.6),
        f5: (b"4.",4.0),
        f6: (b"-.002",-0.002),
        f7: (b"0.0",0.0),
    }


    #[test]
    fn hexadecimal_string_test() {
        assert_eq!(
            nom::Err::Position(nom::ErrorKind::Tag, &[45u8, 62u8][..]),
            hexadecimal_string(b"<a->".as_bytes()).to_result().unwrap_err()
        );
    }
    macro_rules! hexst {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    match hexadecimal_string(input).to_result().unwrap() {
                        PdfObject::String(v) => {
                            assert_eq!( v, expected );
                        },
                        _ => {
                            assert_eq!(7,0);
                        }
                    }
                }
            )*
        }
    }
    hexst! {
        hx1: (b"<abcdef0123456789>", vec![0xab, 0xcd, 0xef, 0x01, 0x23, 0x45, 0x67, 0x89]),
        hx2: (b"<abc def0123\n456789 >", vec![0xab, 0xcd, 0xef, 0x01, 0x23, 0x45, 0x67, 0x89]),
        hx3: (b"<ab>", vec![0xab]),
        hx3a: (b"<a\rb>", vec![0xab]),
        hx4: (b"<a>", vec![0xa0]),
        hx5: (b"<ab cd\nef1>", vec![0xab, 0xcd, 0xef, 0x10]),
    }

    macro_rules! lsit {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    assert_eq!(expected,
                        literal_string(input.as_bytes()).unwrap_inc());
                }
            )*
        }
    }


    macro_rules! lset {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    assert_eq!(expected,
                        literal_string(input.as_bytes()));
                }
            )*
        }
    }

    lsit! {
        lsit_1: (b"(abcde", Needed::Unknown),
        lsit_2: (b"(abc()", Needed::Unknown),
        lsit_3: (b"(abc\\", Needed::Unknown),
    }

    lset! {
        lset_1: (b"(abc\\80)", Error(nom::Err::Position(ErrorKind::Custom(ErrorCodes::UnrecognizedEscapeSequence as u32), b"(abc\\80)".as_bytes()))),
    }

    macro_rules! tlsr {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    match literal_string(input.as_bytes()).to_result().unwrap() {
                        PdfObject::String(v) => {
                            assert_eq!(expected[..].to_owned(), v);
                        },
                        _ => {
                            assert_eq!(9,0);
                        }
                    }
                }
            )*
        }
    }

    tlsr! {
        tlsr_0: (b"(abcd)", b"abcd"),
        tlsr_1: (b"(\\247)", b"\xA7"),

        tlsr_2: (b"(a)", b"a"),
        tlsr_3: (b"(This is a string)", b"This is a string"),
        tlsr_4: (b"(Strings can contain newlines\nand such.)", b"Strings can contain newlines\nand such."),
        tlsr_5: (b"(Strings can contain balanced parentheses ()\nand special characters ( * ! & } ^ %and so on) .)",
                    b"Strings can contain balanced parentheses ()\nand special characters ( * ! & } ^ %and so on) ."),
        tlsr_6: (b"(The following is an empty string .)", b"The following is an empty string ."),
        tlsr_7: (b"()", b""),
        tlsr_8: (b"(It has zero (0) length.)", b"It has zero (0) length."),

        tlsr_9: (b"(These \\\rtwo strings \\\nare the same.)", b"These two strings are the same."),
        tlsr_a: (b"(These two strings are the same.)", b"These two strings are the same."),

        tlsr_b: (b"(This string has an end-of-line at the end of it.\n)", b"This string has an end-of-line at the end of it.\n"),
        tlsr_c: (b"(So does this one.\\n)", b"So does this one.\n"),
        tlsr_d: (b"(This string contains \\245two octal characters\\307.)", b"This string contains \xA5two octal characters\xC7."),
        tlsr_e: (b"(\\0053)", b"\x053"),
        tlsr_f: (b"(\\053)", b"+"),
        tlsr_g: (b"(\\53)", b"+"),
        tlsr_h: (b"(\\533)", b"+3"),
    }

    macro_rules! name_object_result_test {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    match name_object(input.as_bytes()).to_result().unwrap() {
                        PdfObject::Name( v ) => {
                            assert_eq!(expected[..].to_owned(), v);
                        },
                        _ => {
                            assert_eq!(10, 0);
                        }
                    }
                }
            )*
        }
    }

    name_object_result_test! {
        nort_1: (b"/Name1",b"Name1"),
        nort_2: (b"/ASomewhatLongerName",b"ASomewhatLongerName"),
        nort_3:(b"/A;Name_With-Various***Characters?",b"A;Name_With-Various***Characters?"),
        nort_5: (b"/1.2",b"1.2"),
        nort_6: (b"/$$",b"$$"),
        nort_7: (b"/@pattern",b"@pattern"),
        nort_8: (b"/.notdef",b".notdef"),
        nort_9: (b"/Lime#20Green",b"Lime Green"),
        nort_10: (b"/paired#28#29parentheses",b"paired()parentheses"),
        nort_11: (b"/The_Key_of_F#23_Minor",b"The_Key_of_F#_Minor"),
        nort_12: (b"/A#42",b"AB"),
        nort_13: (b"/#2F",b"/"),
        nort_14: (b"/",b""),
        nort_15: (b"/abcd[",b"abcd"),
    }

    #[test]
    fn test_ws_recognize() {
        assert_eq!(0, recognize_some_ws(b"a ".as_bytes()).to_result().unwrap().len());
        assert_eq!(0, recognize_some_ws(b"".as_bytes()).to_result().unwrap().len());
        assert_eq!(2, recognize_some_ws(b" \t".as_bytes()).to_result().unwrap().len());
        assert_eq!(9, recognize_some_ws(b" \t \r\n \n \r".as_bytes()).to_result().unwrap().len());
    }
}