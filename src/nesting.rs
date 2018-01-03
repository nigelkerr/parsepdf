extern crate nom;

use nom::*;
use nom::ErrorKind;
use nom::IResult::*;

use structs::ErrorCodes;
use structs::PdfObject;
use structs::NameKeyedMap;

use simple::*;

// array object § 7.3.6

pub fn array_object(input: &[u8]) -> IResult<&[u8], PdfObject>
{
    let mut result: Vec<PdfObject> = Vec::new();

    if input.len() == 0 {
        return Incomplete(Needed::Unknown);
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

    'outer:
        loop {
        'inner:
            while index < linput.len() {
            if !inside {
                index += skip_whitespace(&linput[index..]);

                if linput[index] == b'[' {
                    inside = true;
                    index += 1;
                    continue 'inner;
                } else {
                    // anything not whitespace and not [ isn't an array at this point,
                    // we ought to consume it some other way.
                    return Error(error_position!(ErrorKind::Custom(ErrorCodes::ExpectedArrayStart as u32), input));
                }
            }

            index += skip_whitespace(&linput[index..]);

            if linput[index] == b']' {
                return Done(&linput[index + 1..], PdfObject::Array(result));
            }

            for recognizer in &functions {
                match (*recognizer)(&linput[index..]) {
                    Done(rest, obj) => {
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

            // we reached here without recognizing anything!
            return Error(error_position!(ErrorKind::Custom(ErrorCodes::NoValidArrayContents as u32), input));
        }
    }
}


// dictionary § 7.3.7

pub fn dictionary_object(input: &[u8]) -> IResult<&[u8], PdfObject>
{
    let mut result: NameKeyedMap = NameKeyedMap::new();

    if input.len() == 0 {
        return Incomplete(Needed::Unknown);
    }

    let mut linput = input;

    let mut index: usize = 0;
    let mut inside: bool = false;
    let mut expect_name: bool = true; // this acts as a flip flop

    let mut current_key: PdfObject = PdfObject::Name(vec![]);
    let mut name_functions: Vec<fn(&[u8]) -> IResult<&[u8], PdfObject> > = Vec::new();
    name_functions.push(name_object);
    name_functions.push(comment);

    let mut functions: Vec<fn(&[u8]) -> IResult<&[u8], PdfObject> > = Vec::new();
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

    'outer:
        loop {
        'inner:
            while index < linput.len() {

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
                    return Error(error_position!(ErrorKind::Custom(ErrorCodes::ExpectedDictionaryStart as u32), input));
                }
            }

            index += skip_whitespace(&linput[index..]);

            if linput[index] == b'>' && linput[index + 1] == b'>' {

                // do we expect the next object to be a name?
                // that is, have we consumed complete name-value pairs thus
                // far and would be looking to start a new pair if we
                // weren't ending here?
                if expect_name {
                    return Done(&linput[index + 2..], PdfObject::Dictionary(result));
                } else {
                    // if we were expecting a value, we're error
                    return Error(error_position!(ErrorKind::Custom(ErrorCodes::ExpectedDictionaryValue as u32), input));
                }
            }

            if expect_name {
                for recognizer in &name_functions {
                    match (*recognizer)(&linput[index..]) {
                        Done(rest, obj) => {
                            match obj {
                                PdfObject::Name(_) => {
                                    current_key = obj;
                                    expect_name = !expect_name;
                                },
                                PdfObject::Comment(_) => {
                                    // skip these
                                },
                                _ => {
                                    // how did this happen? FIXME
                                }
                            }
                            linput = rest;
                            index = 0;
                            continue 'outer;
                        }
                        Incomplete(whatever) => {
                            return Incomplete(whatever);
                        }
                        _ => {
                            // ulp
                        }
                    };
                }
            } else {
                for recognizer in &functions {
                    match (*recognizer)(&linput[index..]) {
                        Done(rest, obj) => {
                            match obj {
                                PdfObject::Comment(_) => {
                                    // skip these
                                },
                                _ => {
                                    let _ = result.insert(current_key.clone(), obj);
                                    expect_name = !expect_name;
                                }
                            }
                            linput = rest;
                            index = 0;
                            continue 'outer;
                        }
                        Incomplete(whatever) => {
                            return Incomplete(whatever);
                        }
                        _ => {
                            // ugh
                        }
                    };
                }
            }

            return Error(error_position!(ErrorKind::Custom(ErrorCodes::NoValidDictionaryContents as u32), input));
        }
    }
}


// Stream objects § 7.3.8
// these are a Dictionary that must have a /Length value, and the stream..endstream after it






#[cfg(test)]
mod tests {
    use super::*;
    use nom::AsBytes;

    macro_rules! recognized_array_object_test {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    assert_eq!(array_object(input.as_bytes()).to_result().unwrap(),
                        expected);
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
                    assert_eq!(dictionary_object(input.as_bytes()).to_result().unwrap(),
                        expected);
                }
            )*
        }
    }

    recognized_dict_object_test! {
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
    fn test_recognized_dict_object_errors () {

        assert_eq!(
            nom::Err::Position(nom::ErrorKind::Custom(ErrorCodes::ExpectedDictionaryValue as u32), b"<</yo%yo\n >>".as_bytes()),
            dictionary_object(b"<</yo%yo\n >>".as_bytes()).to_result().unwrap_err()
        );
        assert_eq!(
            nom::Err::Position(nom::ErrorKind::Custom(ErrorCodes::ExpectedDictionaryValue as u32), b"<</yo>>".as_bytes()),
            dictionary_object(b"<</yo>>".as_bytes()).to_result().unwrap_err()
        );
    }
}

