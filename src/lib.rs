#[macro_use]
extern crate nom;
#[macro_use]
extern crate approx;

use nom::*;
use nom::digit;
use nom::ErrorKind;
use nom::IResult::*;
use std::str;
use std::str::FromStr;

// parse a pdf file, per ISO 32000-2_2017(en)

mod simple;
mod structs;

pub use simple::*;
pub use structs::*;

named!(pdf_version<&[u8],&[u8]>,
    alt!(
        tag!(b"1.0") |
        tag!(b"1.1") |
        tag!(b"1.2") |
        tag!(b"1.3") |
        tag!(b"1.4") |
        tag!(b"1.5") |
        tag!(b"1.6") |
        tag!(b"1.7") |
        tag!(b"2.0")
    )
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



// indirect references § 7.3.10

// surely there's a better way...
named!(non_zero_positive_int_not_padded<&[u8], &[u8] >,
    recognize!(
        pair!(
            alt!(
                tag!(b"1") |
                tag!(b"2") |
                tag!(b"3") |
                tag!(b"4") |
                tag!(b"5") |
                tag!(b"6") |
                tag!(b"7") |
                tag!(b"8") |
                tag!(b"9")
            ),
            opt!( complete!(nom::digit ) )
        )
    )
);

named!(maybe_indirect_reference<&[u8], (&[u8],&[u8],&[u8],&[u8]) >,
    tuple!(
        non_zero_positive_int_not_padded,
        tag!(b" "),
        complete!(nom::digit),
        tag!(b" R")
    )
);

named!(pub recognize_indirect_reference<&[u8],&[u8]>,
    recognize!(
        maybe_indirect_reference
    )
);

#[inline]
fn dec_u32(input: &[u8]) -> u32 {
    let mut res = 0u32;

    for &e in input {
        let digit = e as char;
        let value = digit.to_digit(10).unwrap_or(0);
        res = value + (res * 10);
    }
    res
}

named!(pub indirect_reference<&[u8],PdfObject>,
    do_parse!(
        number_bytes: non_zero_positive_int_not_padded >>
        tag!(b" ") >>
        version_bytes: recognize!(digit) >>
        tag!(b" R") >>
        (
            PdfObject::IndirectReference{ number: dec_u32(number_bytes), generation: dec_u32(version_bytes) }
        )
    )
);

named!(pub recognize_disambiguate_signed_integer_vs_indirect_reference<&[u8],&[u8]>,
    recognize!(
        alt!(
            complete!( indirect_reference ) |
            complete!( signed_integer )
        )
    )
);

// array object § 7.3.6





/*
pub fn recognize_array_object(input: &[u8]) -> IResult<&[u8], &[u8]>
{
    let input_length = input.len();

    if input_length == 0 {
        return Incomplete(Needed::Unknown);
    }


    let mut index: usize = 0;
    let mut inside: bool = false;

    let mut functions: Vec<fn(&[u8]) -> IResult<&[u8], &[u8]>> = Vec::new();
    functions.push(recognize_disambiguate_signed_integer_vs_indirect_reference);
    functions.push(recognize_signed_float);
    functions.push(recognize_null_object);
    functions.push(recognize_boolean);
    functions.push(recognize_name_object);
    functions.push(recognize_comment);
    functions.push(recognize_hexadecimal_string);
    functions.push(recognize_literal_string);
    functions.push(recognize_array_object);
    functions.push(recognize_dictionary_object);

    // move forward through input until we Complete an Array at this level,
    // or get an Error/Incomplete (which return)

    // item-by-item maybe not what we want, but by recognized clump ?
    // try to recognize what we can for starters

    'outer:
        while index < input_length {

        // consume optional whitespace preamble
        if !inside {
            index += skip_whitespace(&input[index..]);

            if input[index] == b'[' {
                inside = true;
                index += 1;
                continue 'outer;
            } else {
                // anything not whitespace and not [ isn't an array at this point,
                // we ought to consume it some other way.
                return Error(error_position!(ErrorKind::Custom(12345), input));
            }
        }

        index += skip_whitespace(&input[index..]);

        if input[index] == b']' {
            return Done(&input[index + 1..], &input[..index + 1]);
        }

        for recognizer in &functions {
            match (*recognizer)(&input[index..]) {
                Done(_, recognized) => {
                    index += recognized.len();
                    continue 'outer;
                }
                Incomplete(whatever) => {
                    return Incomplete(whatever);
                }
                _ => {
                    // i think we ignore these til we bail out
                    // of this regonizers function loop.  it will
                    // be an error eventually. (right?)
                }
            };
        }


        // so its not ending the array, not whitespace, its
        // not an int, its not a name, not a comment
        // its not an array.  must be error.
        // or is it incomplete?  could we complete from wherever we are?

        return Error(error_position!(ErrorKind::Custom(12347), input));
    }

    Incomplete(Needed::Unknown)
}

// FIXME: something that builds on recognizing to return an array structure.

// dictionary § 7.3.7
// recognizer first

pub fn recognize_dictionary_object(input: &[u8]) -> IResult<&[u8], &[u8]>
{
    let input_length = input.len();

    if input_length == 0 {
        return Incomplete(Needed::Unknown);
    }


    let mut index: usize = 0;
    let mut inside: bool = false;
    let mut expect_name: bool = true;

    let mut name_functions: Vec<fn(&[u8]) -> IResult<&[u8], &[u8]>> = Vec::new();
    name_functions.push(recognize_name_object);

    let mut functions: Vec<fn(&[u8]) -> IResult<&[u8], &[u8]>> = Vec::new();
    functions.push(recognize_disambiguate_signed_integer_vs_indirect_reference);
    functions.push(recognize_signed_float);
    functions.push(recognize_null_object);
    functions.push(recognize_boolean);
    functions.push(recognize_name_object);
    functions.push(recognize_hexadecimal_string);
    functions.push(recognize_literal_string);
    functions.push(recognize_array_object);
    functions.push(recognize_dictionary_object);

    // move forward through input until we Complete an Array at this level,
    // or get an Error/Incomplete (which return)

    // item-by-item maybe not what we want, but by recognized clump ?
    // try to recognize what we can for starters

    'outer:
        while index < input_length {

        // consume optional whitespace preamble
        if !inside {
            index += skip_whitespace(&input[index..]);
            ;

            if input[index] == b'<' && input[index + 1] == b'<' {
                inside = true;
                index += 2;
                continue 'outer;
            } else {
                // anything not whitespace and not < isn't a dict at this point,
                // we ought to consume it some other way.
                return Error(error_position!(ErrorKind::Custom(34567), input));
            }
        }

        index += skip_whitespace(&input[index..]);

        if input[index] == b'>' && input[index + 1] == b'>' {

            // do we expect the next object to be a name?
            // that is, have we consumed complete name-value pairs thus
            // far and would be looking to start a new pair if we
            // weren't ending here?
            if expect_name {
                return Done(&input[index + 2..], &input[..index + 2]);
            } else {
                // if we were expecting a value, we're error
                return Error(error_position!(ErrorKind::Custom(34569), input));
            }

        }

        match recognize_comment(&input[index..]) {
            Done(_, recognized) => {
                index += recognized.len();
                continue 'outer;
            }
            Incomplete(whatever) => {
                return Incomplete(whatever);
            }
            _ => {
                // keep going, we'll fall through all the options eventually.
            }
        }

        if expect_name {
            for recognizer in &name_functions {
                match (*recognizer)(&input[index..]) {
                    Done(_, recognized) => {
                        index += recognized.len();
                        expect_name = !expect_name;
                        continue 'outer;
                    }
                    Incomplete(whatever) => {
                        return Incomplete(whatever);
                    }
                    _ => {
                        // i think we ignore these til we bail out
                        // of this recognizers function loop.  it will
                        // be an error eventually. (right?)
                    }
                };
            }
        } else {
            for recognizer in &functions {
                match (*recognizer)(&input[index..]) {
                    Done(_, recognized) => {
                        index += recognized.len();
                        expect_name = !expect_name;
                        continue 'outer;
                    }
                    Incomplete(whatever) => {
                        return Incomplete(whatever);
                    }
                    _ => {
                        // i think we ignore these til we bail out
                        // of this recognizers function loop.  it will
                        // be an error eventually. (right?)
                    }
                };
            }
        }


        // so its not ending the dict, not whitespace, its
        // not an int, its not a name, not a comment
        // its not an array or another dict.  must be error.
        // or is it incomplete?  could we complete from wherever we are?

        return Error(error_position!(ErrorKind::Custom(34568), input));
    }

    Incomplete(Needed::Unknown)
}


// Stream objects § 7.3.8
// these are a Dictionary that must have a /Length value, and the stream..endstream after it

*/

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pdf_indirect_reference_test() {
        assert_eq!(b"95 0 R".as_bytes(), recognize_indirect_reference(b"95 0 R".as_bytes()).to_result().unwrap());
        assert_eq!(b"95 0 R".as_bytes(), recognize_indirect_reference(b"95 0 R ".as_bytes()).to_result().unwrap());
        assert_eq!(b"9 1 R".as_bytes(), recognize_indirect_reference(b"9 1 R".as_bytes()).to_result().unwrap());
        assert_eq!(Err(nom::Err::Position(nom::ErrorKind::Alt, [48, 57, 32, 49, 32, 82].as_bytes())), recognize_indirect_reference(b"09 1 R".as_bytes()).to_result());
        assert_eq!(PdfObject::IndirectReference { number: 95, generation: 0 },
                   indirect_reference(b"95 0 R ".as_bytes()).to_result().unwrap());
        assert_eq!(PdfObject::IndirectReference { number: 95, generation: 100 },
                   indirect_reference(b"95 100 R ".as_bytes()).to_result().unwrap());
    }

    #[test]
    fn pdf_null_test() {
        match null_object(b"null").to_result().unwrap() {
            PdfObject::Null => {},
            _ =>  {
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
    fn pdf_boolean_test() {
        assert_eq!(PdfObject::Boolean ( true ), boolean(b"true ").to_result().unwrap());
        assert_eq!(PdfObject::Boolean ( false ), boolean(b"false").to_result().unwrap());
    }

    #[test]
    fn pdf_magic_test() {
        assert_eq!(PdfVersion::Known { ver: b"1.0".to_vec() }, pdf_magic(b"%PDF-1.0\r\n").to_result().unwrap());
        assert_eq!(nom::Err::Position(nom::ErrorKind::Tag, &[98u8, 108u8, 97u8, 104u8][..]),
                   pdf_magic(b"blah").to_result().unwrap_err());
        assert_eq!(nom::Err::Position(nom::ErrorKind::Alt, &[51u8, 46u8, 48u8, 13u8][..]),
                   pdf_magic(b"%PDF-3.0\r").to_result().unwrap_err());
    }

    #[test]
    fn pdf_linendings_by_macro_test() {
        assert_eq!(b"\r".as_bytes(), pdf_line_ending_by_macro(b"\rdd".as_bytes()).to_result().unwrap());
        assert_eq!(b"\r\n".as_bytes(), pdf_line_ending_by_macro(b"\r\ndd".as_bytes()).to_result().unwrap());
        assert_eq!(b"\n".as_bytes(), pdf_line_ending_by_macro(b"\ndd".as_bytes()).to_result().unwrap());
    }

    #[test]
    fn pdf_comments_test() {
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
    fn pdf_header_test() {
        assert_eq!(nom::IResult::Done(b" ".as_bytes(), PdfVersion::Known { ver: b"1.0".to_vec() }),
                   pdf_header(b"%PDF-1.0\r ".as_bytes()));
        assert_eq!(nom::IResult::Done(b" ".as_bytes(), PdfVersion::Known { ver: b"2.0".to_vec() }),
                   pdf_header("%PDF-2.0\r%なななな\n ".as_bytes()));
    }

    #[test]
    fn integers_test() {
        assert_eq!(PdfObject::Integer ( 123 ), signed_integer(b"123").to_result().unwrap());
        assert_eq!(PdfObject::Integer ( -123 ), signed_integer(b"-123").to_result().unwrap());
        assert_eq!(PdfObject::Integer ( 0 ), signed_integer(b"0").to_result().unwrap());
        assert_eq!(PdfObject::Integer ( 0 ), signed_integer(b"-0").to_result().unwrap()); // heh
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
        lset_1: (b"(abc\\80)", Error(nom::Err::Position(ErrorKind::Custom(3333), b"(abc\\80)".as_bytes()))),
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

/*
    macro_rules! recognized_array_object_test {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    assert_eq!(recognize_array_object(input.as_bytes()).to_result().unwrap().len(),
                        expected);
                }
            )*
        }
    }
    recognized_array_object_test! {
        raot_1: (b"[1 2 3]", 7),
        raot_2: (b"[1 2 [3]]", 9),
        raot_3: (b"[1 2[3]]", 8),

        raot_5: (b"[/1 2[3]]", 9),
        raot_6: (b"[/12[3]] ]", 8),
        raot_7: (b"[/12 [3]]", 9),

        raot_8: (b"[       /12 \n1\r\r2    [ ] ]", 26),

        raot_9: (b"[1%yo\r %yo\n2%hiya\r\n [3]]", 24),
        raot_10: (b"[%yo\r1%yo\r %yo\n2%hiya\r\n [3]]", 28),

        raot_11: (b"[(hiya) /yo 1 2 3]", 18),
        raot_12: (b"[<09 ab> /yo 1 2 3]", 19),
        raot_13: (b"[99 0 R /yo 1 2 3]", 18),
        raot_14: (b"[99 0 R 100 0 R  ]", 18),
        raot_15: (b"[<</a[1 2 3]>> <</b 1 2 R>>]", 28),
    }

    macro_rules! recognized_dict_object_test {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    assert_eq!(recognize_dictionary_object(input.as_bytes()).to_result().unwrap().len(),
                        expected);
                }
            )*
        }
    }

    recognized_dict_object_test! {
        rd_1: (b"<< >>", 5),
        rd_2: (b"<</yo%yo\n1>>", 12),
        rd_3: (b"<</a<</a<</a [1]>>>>>>", 22),
        rd_4: (b"<</a<</a<</a 1>>>>>>", 20),
        rd_5: (b"<</a<</a<abcdef>>>>>", 20),
        rd_6: (b"<</a<</a%yo\n<abcdef>>>>>", 24),
    }

    #[test]
    fn test_recognized_dict_object_errors () {

        assert_eq!(
            nom::Err::Position(nom::ErrorKind::Custom(34569), b"<</yo%yo\n >>".as_bytes()),
            recognize_dictionary_object(b"<</yo%yo\n >>".as_bytes()).to_result().unwrap_err()
        );
        assert_eq!(
            nom::Err::Position(nom::ErrorKind::Custom(34569), b"<</yo>>".as_bytes()),
            recognize_dictionary_object(b"<</yo>>".as_bytes()).to_result().unwrap_err()
        );
    }

*/
}

