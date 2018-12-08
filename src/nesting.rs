extern crate nom;

use std::str;
use std::str::FromStr;

use nom::ErrorKind;
use nom::*;

use crate::structs::ErrorCodes;
use crate::structs::IndirectObject;
use crate::structs::NameKeyedMap;
use crate::structs::PdfObject;

use crate::simple::*;

// array object § 7.3.6

pub fn array_object(input: &[u8]) -> IResult<&[u8], PdfObject> {
    let mut result: Vec<PdfObject> = Vec::new();

    if input.len() == 0 {
        return Err(Err::Incomplete(Needed::Unknown));
    }

    let mut linput = input;

    let mut index: usize = 0;
    let mut inside: bool = false;

    let mut functions: Vec<fn(&[u8]) -> IResult<&[u8], PdfObject>> = Vec::new();
    functions.push(indirect_reference);
    functions.push(signed_integer);
    functions.push(signed_float);
    functions.push(null_object);
    functions.push(boolean);
    functions.push(name_object);
    functions.push(comment);
    functions.push(hexadecimal_string);
    functions.push(literal_string);
    functions.push(array_object);
    functions.push(dictionary_object);

    // move forward through input until we Complete an Array at this level,
    // or get an Error/Incomplete (which return).
    // we dodge needing to know where to start by re-assigning the linput
    // we are working through after recognizing most objects.  should
    // we treat whitespace the way we treat the others?

    'outer: loop {
        'inner: while index < linput.len() {
            if !inside {
                index += skip_whitespace(&linput[index..]);

                if linput[index] == b'[' {
                    inside = true;
                    index += 1;
                    continue 'inner;
                } else {
                    // anything not whitespace and not [ isn't an array at this point,
                    // we ought to consume it some other way.
                    return Err(Err::Error(error_position!(
                        input,
                        ErrorKind::Custom(ErrorCodes::ExpectedArrayStart as u32)
                    )));
                }
            }

            index += skip_whitespace(&linput[index..]);

            if linput[index] == b']' {
                return Ok((&linput[index + 1..], PdfObject::Array(result)));
            }

            for recognizer in &functions {
                match (*recognizer)(&linput[index..]) {
                    Ok((rest, obj)) => {
                        match obj {
                            PdfObject::Comment(_) => {
                                // should we retain these?
                            }
                            _ => {
                                result.push(obj);
                            }
                        }
                        // reset our input to the new start of input
                        linput = rest;
                        index = 0;
                        continue 'outer;
                    }
                    Err(Err::Incomplete(whatever)) => {
                        return Err(Err::Incomplete(whatever));
                    }
                    _ => {
                        // i think we ignore these til we bail out
                        // of this recognizers function loop.  it will
                        // be an error eventually. (right?)
                    }
                };
            }

            // we reached here without recognizing anything!
            return Err(Err::Error(error_position!(
                input,
                ErrorKind::Custom(ErrorCodes::NoValidArrayContents as u32)
            )));
        }
    }
}

// dictionary § 7.3.7

pub fn dictionary_object(input: &[u8]) -> IResult<&[u8], PdfObject> {
    let mut result: NameKeyedMap = NameKeyedMap::new();

    if input.len() == 0 {
        return Err(Err::Incomplete(Needed::Unknown));
    }

    let mut linput = input;

    let mut index: usize = 0;
    let mut inside: bool = false;
    let mut expect_name: bool = true; // this acts as a flip flop

    let mut current_key: PdfObject = PdfObject::Name(vec![]);
    let mut name_functions: Vec<fn(&[u8]) -> IResult<&[u8], PdfObject>> = Vec::new();
    name_functions.push(name_object);
    name_functions.push(comment);

    let mut functions: Vec<fn(&[u8]) -> IResult<&[u8], PdfObject>> = Vec::new();
    functions.push(comment);
    functions.push(indirect_reference);
    functions.push(signed_integer);
    functions.push(signed_float);
    functions.push(null_object);
    functions.push(boolean);
    functions.push(name_object);
    functions.push(hexadecimal_string);
    functions.push(literal_string);
    functions.push(array_object);
    functions.push(dictionary_object);

    // move forward through input until we Complete a dictionary at this level,
    // or get an Error/Incomplete (which return)

    'outer: loop {
        'inner: while index < linput.len() {
            // consume optional whitespace preamble
            if !inside {
                index += skip_whitespace(&linput[index..]);
                ;

                if linput[index] == b'<' && linput[index + 1] == b'<' {
                    inside = true;
                    index += 2;
                    continue 'inner;
                } else {
                    // anything not whitespace and not < isn't a dict at this point,
                    // we ought to consume it some other way.
                    return Err(Err::Error(error_position!(
                        input,
                        ErrorKind::Custom(ErrorCodes::ExpectedDictionaryStart as u32)
                    )));
                }
            }

            index += skip_whitespace(&linput[index..]);

            if linput[index] == b'>' && linput[index + 1] == b'>' {
                // do we expect the next object to be a name?
                // that is, have we consumed complete name-value pairs thus
                // far and would be looking to start a new pair if we
                // weren't ending here?
                if expect_name {
                    return Ok((&linput[index + 2..], PdfObject::Dictionary(result)));
                } else {
                    // if we were expecting a value, we're error
                    return Err(Err::Error(error_position!(
                        input,
                        ErrorKind::Custom(ErrorCodes::ExpectedDictionaryValue as u32)
                    )));
                }
            }

            if expect_name {
                for recognizer in &name_functions {
                    match (*recognizer)(&linput[index..]) {
                        Ok((rest, obj)) => {
                            match obj {
                                PdfObject::Name(_) => {
                                    current_key = obj;
                                    expect_name = !expect_name;
                                }
                                PdfObject::Comment(_) => {
                                    // skip these
                                }
                                _ => {
                                    // how did this happen? FIXME
                                }
                            }
                            linput = rest;
                            index = 0;
                            continue 'outer;
                        }
                        Err(Err::Incomplete(whatever)) => {
                            return Err(Err::Incomplete(whatever));
                        }
                        _ => {
                            // ulp FIXME?
                        }
                    };
                }
            } else {
                for recognizer in &functions {
                    match (*recognizer)(&linput[index..]) {
                        Ok((rest, obj)) => {
                            match obj {
                                PdfObject::Comment(_) => {
                                    // skip these
                                }
                                _ => {
                                    let _ = result.insert(current_key.clone(), obj);
                                    expect_name = !expect_name;
                                }
                            }
                            linput = rest;
                            index = 0;
                            continue 'outer;
                        }
                        Err(Err::Incomplete(whatever)) => {
                            return Err(Err::Incomplete(whatever));
                        }
                        _ => {
                            // ulp FIXME?
                        }
                    };
                }
            }

            return Err(Err::Error(error_position!(
                input,
                ErrorKind::Custom(ErrorCodes::NoValidDictionaryContents as u32)
            )));
        }
    }
}

// not quite the normal api, since we have an alleged /Length by which to
// measure the stream.

// but /Length can have an object reference, so ugh

fn recognize_stream(input: &[u8], len: usize) -> IResult<&[u8], Vec<u8>> {
    let mut index = 0;

    // rules about which ws?
    index += skip_whitespace(input);

    // the special case the len == 0, which could be an actual 0, or because
    // of an indirect reference pointing to the length value.
    if len == 0 {
        // § 7.3.8.1 tells "There should be an end-of-line marker after the data and before endstream"
        // and examples abound where this isnt the case in real PDFs.
        // need to be careful with these streams: trim to correct length in
        // a second pass ?
        match delimited!(
            &input[index..],
            re_bytes_find!("^stream(\r\n|\n)"),
            take_until!("endstream"),
            tag!("endstream")
        ) {
            Ok((rest, stream_bytes)) => {
                return Ok((rest, stream_bytes[..].to_owned()));
            }
            Err(Err::Incomplete(whatever)) => {
                return Err(Err::Incomplete(whatever));
            }
            Err(err) => {
                return Err(err);
            }
        }
    }

    match re_bytes_find!(&input[index..], r"^stream(\r\n|\n)") {
        // just these two line ending options
        Ok((rest, _stream)) => match take!(rest, len) {
            Ok((rest2, bytes_of_stream)) => match re_bytes_find!(rest2, r"^(\r\n|\r|\n)?endstream")
            {
                Ok((rest3, _endstream)) => {
                    return Ok((rest3, bytes_of_stream[..].to_owned()));
                }
                Err(Err::Incomplete(whatever)) => {
                    return Err(Err::Incomplete(whatever));
                }
                Err(err) => {
                    return Err(err);
                }
            },
            Err(Err::Incomplete(whatever)) => {
                return Err(Err::Incomplete(whatever));
            }
            Err(err) => {
                return Err(err);
            }
        },
        Err(Err::Incomplete(whatever)) => {
            return Err(Err::Incomplete(whatever));
        }
        Err(err) => {
            return Err(err);
        }
    }
}

// Stream objects § 7.3.8
// these are a Dictionary that must have a /Length value, direct or indirect,
// and the stream..endstream after it.
// this could be an xref stream, but we'll need somehow to check for /Type...

pub fn stream_object(input: &[u8]) -> IResult<&[u8], PdfObject> {
    match dictionary_object(input) {
        Ok((rest, PdfObject::Dictionary(n))) => match n.get(PdfObject::Name(
            b"Length"[..].to_owned(),
        )) {
            Ok(Some(PdfObject::Integer(x))) => {
                if x >= 0 {
                    match recognize_stream(rest, x as usize) {
                        Ok((rest2, v)) => {
                            return Ok((rest2, PdfObject::Stream(n, v)));
                        }
                        Err(Err::Incomplete(whatever)) => {
                            return Err(Err::Incomplete(whatever));
                        }
                        Err(err) => {
                            return Err(err);
                        }
                    }
                } else {
                    return Err(Err::Error(error_position!(
                        input,
                        ErrorKind::Custom(ErrorCodes::NegativeLengthInStreamDictionary as u32)
                    )));
                }
            }
            Ok(Some(PdfObject::IndirectReference {
                number: _n,
                generation: _g,
            })) => match recognize_stream(rest, 0 as usize) {
                Ok((rest2, v)) => {
                    return Ok((rest2, PdfObject::Stream(n, v)));
                }
                Err(Err::Incomplete(whatever)) => {
                    return Err(Err::Incomplete(whatever));
                }
                Err(err) => {
                    return Err(err);
                }
            },
            Ok(Some(_p)) => {
                return Err(Err::Error(error_position!(
                    input,
                    ErrorKind::Custom(ErrorCodes::NonIntegerLengthInStreamDictionary as u32)
                )));
            }
            Ok(None) => {
                return Ok((rest, PdfObject::Dictionary(n)));
            }
            Err(_err) => {
                return Err(Err::Error(error_position!(
                    input,
                    ErrorKind::Custom(ErrorCodes::SomethingHorribleAboutStreamDictionary as u32)
                )));
            }
        },
        Ok((_rest, _wut)) => {
            return Err(Err::Error(error_position!(
                input,
                ErrorKind::Custom(ErrorCodes::CalledDictionaryAndGotSomethingElse as u32)
            )));
        }
        Err(Err::Incomplete(whatever)) => {
            return Err(Err::Incomplete(whatever));
        }
        Err(err) => Err(err),
    }
}

// § 7.10 Indirect Objects
// this is more like arrays and dictionaries for the containment than otherwise,
// so i put them here.

// incomplete may fox us here

//pub fn indirect_object(input: &[u8]) -> IResult<&[u8], IndirectObject>
//{
named!(pub indirect_object<&[u8],IndirectObject>,
    do_parse!(
        num: map_res!(map_res!(re_bytes_find!(r"^[123456789]\d*"), str::from_utf8), FromStr::from_str) >>
        tag!(b" ") >>
        gen: map_res!(map_res!(recognize!(digit), str::from_utf8), FromStr::from_str) >>
        tag!(b" obj") >>
        take_while1!( is_pdf_whitespace ) >>
        o: alt!(
            stream_object |
            array_object |
            indirect_reference |
            name_object |
            hexadecimal_string |
            literal_string |
            signed_integer |
            signed_float |
            boolean |
            null_object
        ) >>
        take_while!( is_pdf_whitespace ) >>
        tag!(b"endobj") >>
        (
            IndirectObject{ obj: o, number: num, generation: gen }
        )
    )
);

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! recognized_array_object_test {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    match array_object(input.as_bytes()) {
                        Ok((_b, v)) => {
                            assert_eq!(v, expected);
                        },
                        _ => {
                            assert_eq!(11, 0);
                        }
                    }
                }
            )*
        }
    }
    recognized_array_object_test! {
        raot_1: (b"[1 2 3]",
            PdfObject::Array( vec![PdfObject::Integer(1), PdfObject::Integer(2),PdfObject::Integer(3) ])),
        raot_2: (b"[1 2 [3]]",
            PdfObject::Array( vec![PdfObject::Integer(1), PdfObject::Integer(2),
                                    PdfObject::Array( vec![PdfObject::Integer(3) ])
                                    ])),
        raot_3: (b"[1 2[3]]", PdfObject::Array( vec![PdfObject::Integer(1), PdfObject::Integer(2),
                                    PdfObject::Array( vec![PdfObject::Integer(3) ])
                                    ])),

        raot_5: (b"[/1 2[3]]", PdfObject::Array( vec![PdfObject::Name(b"1"[..].to_owned()), PdfObject::Integer(2),
                                    PdfObject::Array( vec![PdfObject::Integer(3) ])
                                    ])),
        raot_6: (b"[/12[3]] ]", PdfObject::Array( vec![PdfObject::Name(b"12"[..].to_owned()),
                                    PdfObject::Array( vec![PdfObject::Integer(3) ])
                                    ])),
        raot_7: (b"[/12 [3]]", PdfObject::Array( vec![PdfObject::Name(b"12"[..].to_owned()),
                                    PdfObject::Array( vec![PdfObject::Integer(3) ])
                                    ])),

        raot_8: (b"[       /12 \n1\r\r2    [ ] ]",
                                PdfObject::Array( vec![PdfObject::Name(b"12"[..].to_owned()),
                                PdfObject::Integer(1), PdfObject::Integer(2),
                                    PdfObject::Array( vec![ ])
                                    ])),

        raot_9: (b"[1%yo\r %yo\n2%hiya\r\n [3]]",
                                PdfObject::Array( vec![PdfObject::Integer(1), PdfObject::Integer(2),
                                    PdfObject::Array( vec![PdfObject::Integer(3) ])
                                    ])),
        raot_10: (b"[%yo\r1%yo\r %yo\n2%hiya\r\n [3]]",
                                PdfObject::Array( vec![PdfObject::Integer(1), PdfObject::Integer(2),
                                    PdfObject::Array( vec![PdfObject::Integer(3) ])
                                    ])),

        raot_12: (b"[<09 ab> /yo 1 2 3]",
                                PdfObject::Array( vec![
                                    PdfObject::String(b"\x09\xAB"[..].to_owned()),
                                    PdfObject::Name(b"yo"[..].to_owned()),
                                    PdfObject::Integer(1), PdfObject::Integer(2),PdfObject::Integer(3)
                                ])),
        raot_13: (b"[99 0 R /yo 1 2 3]",
                                PdfObject::Array( vec![
                                    PdfObject::IndirectReference { number: 99, generation: 0 },
                                    PdfObject::Name(b"yo"[..].to_owned()),
                                    PdfObject::Integer(1), PdfObject::Integer(2),PdfObject::Integer(3)
                                ])),
        raot_14: (b"[99 0 R 100 2 R  ]",
                                PdfObject::Array( vec![
                                    PdfObject::IndirectReference { number: 99, generation: 0 },
                                    PdfObject::IndirectReference { number: 100, generation: 2 },
                                ])),

        raot_11: (b"[(hiya) /yo 1 2 3]",
                                PdfObject::Array( vec![
                                    PdfObject::String(b"hiya"[..].to_owned()),
                                    PdfObject::Name(b"yo"[..].to_owned()),
                                    PdfObject::Integer(1), PdfObject::Integer(2),PdfObject::Integer(3)
                                ])),

        raot_15: (b"[<</a[1 2 3]>> <</b 1 2 R>>]",
            PdfObject::Array(vec![
                PdfObject::Dictionary( NameKeyedMap::of(vec![
                    PdfObject::Name( b"a"[..].to_owned()),
                    PdfObject::Array( vec![PdfObject::Integer(1), PdfObject::Integer(2),PdfObject::Integer(3) ])
                ]).unwrap().unwrap() ),

                PdfObject::Dictionary( NameKeyedMap::of(vec![
                    PdfObject::Name( b"b"[..].to_owned()),
                    PdfObject::IndirectReference { number: 1, generation: 2 }
                ]).unwrap().unwrap() ),
            ])
        ),
    }

    macro_rules! recognized_dict_object_test {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;

                    match dictionary_object(input.as_bytes()) {
                        Ok((_b, v)) => {
                            assert_eq!(v, expected);
                        },
                        _ => {
                            assert_eq!(11, 0);
                        }
                    }


                }
            )*
        }
    }

    recognized_dict_object_test! {
        rd_0: (b"<</Length 6 0 R/Filter /FlateDecode>>",
                PdfObject::Dictionary(
                    NameKeyedMap::of(
                        vec![
                            PdfObject::Name(b"Length"[..].to_owned()),
                            PdfObject::IndirectReference { number: 6, generation: 0},
                            PdfObject::Name(b"Filter"[..].to_owned()),
                            PdfObject::Name(b"FlateDecode"[..].to_owned()),
                        ]
                    ).unwrap().unwrap()
                )),
        rd_1: (b"<< >>", PdfObject::Dictionary( NameKeyedMap::new() )),
        rd_2: (b"<</yo%yo\n1>>", PdfObject::Dictionary( NameKeyedMap::of(
                                    vec![
                                        PdfObject::Name( b"yo"[..].to_owned()),
                                        PdfObject::Integer( 1 )
                                    ]
                                ).unwrap().unwrap() )),
        rd_3: (b"<</a<</a<</a [1]>>>>>>",

                    PdfObject::Dictionary(
                        NameKeyedMap::of(
                            vec![
                                PdfObject::Name( b"a"[..].to_owned()),
                                PdfObject::Dictionary(
                                    NameKeyedMap::of(
                                        vec![
                                            PdfObject::Name( b"a"[..].to_owned()),
                                            PdfObject::Dictionary(
                                                NameKeyedMap::of(
                                                    vec![
                                                        PdfObject::Name( b"a"[..].to_owned()),
                                                        PdfObject::Array( vec![PdfObject::Integer(1) ])
                                                    ]
                                                ).unwrap().unwrap()
                                            )
                                        ]
                                    ).unwrap().unwrap()
                                )
                            ]
                        ).unwrap().unwrap()
                    )

                ),
        rd_4: (b"<</a<</a<</a 1>>>>>>",
                    PdfObject::Dictionary(
                        NameKeyedMap::of(
                            vec![
                                PdfObject::Name( b"a"[..].to_owned()),
                                PdfObject::Dictionary(
                                    NameKeyedMap::of(
                                        vec![
                                            PdfObject::Name( b"a"[..].to_owned()),
                                            PdfObject::Dictionary(
                                                NameKeyedMap::of(
                                                    vec![
                                                        PdfObject::Name( b"a"[..].to_owned()),
                                                        PdfObject::Integer(1)
                                                    ]
                                                ).unwrap().unwrap()
                                            )
                                        ]
                                    ).unwrap().unwrap()
                                )
                            ]
                        ).unwrap().unwrap()
                    )
        ),
        rd_5: (b"<</a<</a<abcdef>>>>>",
                    PdfObject::Dictionary(
                        NameKeyedMap::of(
                            vec![
                                PdfObject::Name( b"a"[..].to_owned()),
                                PdfObject::Dictionary(
                                    NameKeyedMap::of(
                                        vec![
                                            PdfObject::Name( b"a"[..].to_owned()),
                                            PdfObject::String( b"\xAB\xCD\xEF"[..].to_owned() )
                                        ]
                                    ).unwrap().unwrap()
                                )
                            ]
                        ).unwrap().unwrap()
                    )

        ),
        rd_6: (b"<</a<</a%yo\n<abcdef>>>>>",
                    PdfObject::Dictionary(
                        NameKeyedMap::of(
                            vec![
                                PdfObject::Name( b"a"[..].to_owned()),
                                PdfObject::Dictionary(
                                    NameKeyedMap::of(
                                        vec![
                                            PdfObject::Name( b"a"[..].to_owned()),
                                            PdfObject::String( b"\xAB\xCD\xEF"[..].to_owned() )
                                        ]
                                    ).unwrap().unwrap()
                                )
                            ]
                        ).unwrap().unwrap()
                    )
        ),
    }

    #[test]
    fn test_recognized_dict_object_errors() {
        assert_eq!(
            Err(Err::Error(error_position!(
                b"<</yo%yo\n >>".as_bytes(),
                ErrorKind::Custom(ErrorCodes::ExpectedDictionaryValue as u32)
            ))),
            dictionary_object(b"<</yo%yo\n >>".as_bytes())
        );
        assert_eq!(
            Err(Err::Error(error_position!(
                b"<</yo>>".as_bytes(),
                ErrorKind::Custom(ErrorCodes::ExpectedDictionaryValue as u32)
            ))),
            dictionary_object(b"<</yo>>".as_bytes())
        );
    }

    #[test]
    fn test_recognize_stream() {
        // the indication from samples in the spec ( § 8.9.5.4 Alternate Images )
        // suggests that there is not necessarily an additional EOL in this instance.
        assert_eq!(
            recognize_stream(b" stream\r\nendstream", 0),
            Ok((b"".as_bytes(), b""[..].to_owned()))
        );

        assert_eq!(
            recognize_stream(b" stream\n__--__--__--__--\r\nendstream  ", 16),
            Ok((b"  ".as_bytes(), b"__--__--__--__--"[..].to_owned()))
        );
        assert_eq!(
            recognize_stream(b" stream\n__--__--__--__--endstream  ", 16),
            Ok((b"  ".as_bytes(), b"__--__--__--__--"[..].to_owned()))
        );

        assert_eq!(
            recognize_stream(b" stream\n__--__--", 16),
            Err(Err::Incomplete(Needed::Size(16)))
        );

        assert_eq!(
            recognize_stream(b" stream\n__--__--__--__--", 16),
            Err(Err::Error(Context::Code(&b""[..], ErrorKind::RegexpFind)))
        );
    }

    macro_rules! recognized_stream_object_test {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    match stream_object(input.as_bytes()) {
                        Ok((_rest, x)) => {
                            assert_eq!(x, expected);

                        },
                        x => {
                            println!("err is: {:?}", x);
                            assert_eq!(150, 0);
                        }
                    }
                }
            )*
        }
    }

    recognized_stream_object_test! {
        rsot_1: (b"<< >>", PdfObject::Dictionary( NameKeyedMap::new() )), // because if you dont have a Length you're just a dictionary
        rsot_2: (b"<< /Length 20 >>\nstream\n01234567890123456789\nendstream\n",
            PdfObject::Stream(
                NameKeyedMap::of( vec![PdfObject::Name(b"Length"[..].to_owned()), PdfObject::Integer(20)] ).unwrap().unwrap(),
                b"01234567890123456789"[..].to_owned()
            )
        ),
        rsot_3: (b"<< /Length 20 >>\r\nstream\r\n01234567890123456789\r\nendstream\r\n",
            PdfObject::Stream(
                NameKeyedMap::of( vec![PdfObject::Name(b"Length"[..].to_owned()), PdfObject::Integer(20)] ).unwrap().unwrap(),
                b"01234567890123456789"[..].to_owned()
            )
        ),
        rsot_4: (b"<</Length 10 0 R/Filter /FlateDecode>>\r\nstream\r\n01234567890123456789\r\nendstream\r\n",
            PdfObject::Stream(
                NameKeyedMap::of(
                    vec![
                        PdfObject::Name(b"Length"[..].to_owned()),
                        PdfObject::IndirectReference{ number: 10, generation: 0},
                        PdfObject::Name(b"Filter"[..].to_owned()),
                        PdfObject::Name(b"FlateDecode"[..].to_owned()),
                    ] ).unwrap().unwrap(),
                b"01234567890123456789\r\n"[..].to_owned()
            )
        ),
        rsot_5: (b"<</Length 10 0 R/Filter /FlateDecode>>\r\nstream\r\n0123456\n7890123456789endstream\r\n",
            PdfObject::Stream(
                NameKeyedMap::of(
                    vec![
                        PdfObject::Name(b"Length"[..].to_owned()),
                        PdfObject::IndirectReference{ number: 10, generation: 0},
                        PdfObject::Name(b"Filter"[..].to_owned()),
                        PdfObject::Name(b"FlateDecode"[..].to_owned()),
                    ] ).unwrap().unwrap(),
                b"0123456\n7890123456789"[..].to_owned()
            )
        ),
    }

    #[test]
    fn test_indirect_object() {
        assert_eq!(
            IndirectObject {
                obj: PdfObject::String(b"xyzzy"[..].to_owned()),
                number: 1 as u32,
                generation: 0 as u16
            },
            indirect_object(b"1 0 obj\n(xyzzy)endobj".as_bytes())
                .unwrap()
                .1
        );
    }
}
