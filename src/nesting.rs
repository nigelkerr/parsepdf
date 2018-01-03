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
                        // of this regonizers function loop.  it will
                        // be an error eventually. (right?)
                    }
                };
            }

            // we reached here without recognizing anything!
            return Error(error_position!(ErrorKind::Custom(ErrorCodes::NoValidArrayContents as u32), input));
        }
    }

    // she says we cant get here.
    Incomplete(Needed::Unknown)
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
                    return Error(error_position!(ErrorKind::Custom(34567), input));
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
                    return Error(error_position!(ErrorKind::Custom(34569), input));
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
                                    result.insert(current_key.clone(), obj);
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


            // so its not ending the dict, not not any other valid object.
            // must be error.
            // or is it incomplete?  could we complete from wherever we are?

            return Error(error_position!(ErrorKind::Custom(34568), input));
        }
    }

    Incomplete(Needed::Unknown)
}


// Stream objects § 7.3.8
// these are a Dictionary that must have a /Length value, and the stream..endstream after it


