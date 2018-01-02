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
//    functions.push(dictionary_object);

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


/*

// dictionary § 7.3.7

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
