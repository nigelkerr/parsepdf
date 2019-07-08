// in which we have ways to assert that a given parser-produced structure meets
// some specific validity constraint expressed in the standard, and, for that
// constraint, what are the values represented?

// i don't know what's right here, so let's get it wrong a few times.  In
// fact, let's *CHEAT* for a while.

// try 1: a struct per contract, that has an impl that tries to pull out the
// details.

// pattern: where i don't want to implement anything special yet, a PdfObject.

// i don't like any naming convention yet.

use crate::PdfObject;
use crate::NameMap;

quick_error! {
    #[derive(Debug, PartialEq)]
    pub enum ValidationError {
        Ugh {}
        ExceedsImplementationLimits {}
        EmptyDictionary {}
        NoLengthInXRefStreamDictionary {}
        NoGoodLengthInXRefStreamDictionary {}
        NoSizeInXRefStreamDictionary {}
        NoGoodSizeInXRefStreamDictionary {}
        NoGoodFilterInXRefStreamDictionary {}
        DecodeParamsExpectedToBeDictionaryOrAbsent {}
        DecodeParamsExpectedToBeArrayOfDictionaryOrAbsent {}
        FilterAndDecodeParamArrayLengthsMismatch {}
        DecodeParamValueExpectedToBeDictionaryOrNull {}
        IndexExpectedToBeArray {}
        IndexExpectedToBeArrayOfEvenLength {}
        IndexElementsOtherThanPositiveIntegers {}
        WExpectedToBeArrayOfLength3 {}
        WElementsMustBeNonNegative {}
        WExpectedToBeArray {}
        NoGoodPrevInXRefStreamDictionary {}
    }
}



/// § 7.5.8 Cross-reference Streams, are all of an indirect stream-object,
/// a file-trailer dictionary, and contain cross-reference data that can
/// describe free objects, objects in object-streams, and plain old objects.
#[derive(Debug, PartialEq, Clone)]
pub struct XRefStream {
    length: Option<u64>,
    filter: Option<Vec<PdfObject>>, // Names
    decode_params: Option<Vec<PdfObject>>, // parameter dictionaries
    dl: Option<u64>,

    size: Option<usize>,
    index: Option<Vec<(u32, usize)>>,
    prev: Option<u64>,
    w: Option<(usize, usize, usize)>,

    root: Option<PdfObject>,
    info: Option<PdfObject>,
    encrypt: Option<PdfObject>,
    id: Option<(Vec<u8>, Vec<u8>)>,
}

impl XRefStream {
    pub fn new() -> Self {
        XRefStream {
            length: None,
            filter: None,
            decode_params: None,
            dl: None,
            size: None,
            index: None,
            prev: None,
            w: None,
            root: None,
            info: None,
            encrypt: None,
            id: None,
        }
    }
}

impl Default for XRefStream {
    fn default() -> Self { Self::new() }
}

pub fn validate_xref_stream_dictionary(xref_stream_dict: &NameMap) -> Result<XRefStream, ValidationError> {

    let mut result = XRefStream::new();

    if xref_stream_dict.len() == 0 {
        return Err(ValidationError::EmptyDictionary);
    }

    match xref_stream_dict.get2(b"Length") {
        Some(PdfObject::Integer(value)) if value >= 0i64 => {
            result.length = Some(value as u64);
        },
        None => {
            return Err(ValidationError::NoLengthInXRefStreamDictionary);
        }
        _ => {
            return Err(ValidationError::NoGoodLengthInXRefStreamDictionary)
        }
    }

    match xref_stream_dict.get2(b"Size") {
        Some(PdfObject::Integer(value)) if value > 0i64 => {
            if value < i64::max_value() {
                result.size = Some(value as usize);
            } else {
                return Err(ValidationError::ExceedsImplementationLimits)
            }
        },
        None => {
            return Err(ValidationError::NoSizeInXRefStreamDictionary);
        }
        _ => {
            return Err(ValidationError::NoGoodSizeInXRefStreamDictionary)
        }
    }

    match xref_stream_dict.get2(b"Filter") {
        None => {},
        Some(PdfObject::Array(vec_of_pdf_objects)) => {
            let number_of_filters = vec_of_pdf_objects.len();

            let mut filter_vec: Vec<PdfObject> = Vec::new();
            for filter in &vec_of_pdf_objects {
                match filter {
                    PdfObject::Name(name_value) => { filter_vec.push(PdfObject::Name(name_value.clone())); },
                    _ => { return Err(ValidationError::NoGoodFilterInXRefStreamDictionary); }
                }
            }
            result.filter = Some(filter_vec);

            match xref_stream_dict.get2(b"DecodeParams") {
                Some(PdfObject::Array(vec_of_param_dicts)) => {
                    if number_of_filters != vec_of_param_dicts.len() {
                        return Err(ValidationError::FilterAndDecodeParamArrayLengthsMismatch);
                    }

                    let mut decode_params_vec: Vec<PdfObject> = Vec::new();
                    for decode_param in &vec_of_param_dicts {
                        match decode_param {
                            PdfObject::Dictionary(params) => {
                                decode_params_vec.push(PdfObject::Dictionary(params.clone()));
                            },
                            PdfObject::Null => {
                                decode_params_vec.push(PdfObject::Null);
                            },
                            _ => {
                                return Err(ValidationError::DecodeParamValueExpectedToBeDictionaryOrNull);
                            },
                        }
                    }
                    result.decode_params = Some(decode_params_vec);
                },
                None => {},
                _ => { return Err(ValidationError::DecodeParamsExpectedToBeArrayOfDictionaryOrAbsent)}
            }

        },
        Some(PdfObject::Name(name_value)) => {
            result.filter = Some(vec![PdfObject::Name(name_value.clone())]);
            match xref_stream_dict.get2(b"DecodeParams") {
                Some(PdfObject::Dictionary(decode_params_map)) => {
                    result.decode_params = Some(vec![PdfObject::Dictionary(decode_params_map.clone())]);
                },
                None => {},
                _ => { return Err(ValidationError::DecodeParamsExpectedToBeDictionaryOrAbsent)},
            }
        },
        _ => { return Err(ValidationError::NoGoodFilterInXRefStreamDictionary)}
    }

    match xref_stream_dict.get2(b"Index") {
        Some(PdfObject::Array(vec_of_possibly_integers)) => {
            if (vec_of_possibly_integers.len() % 2) != 0 {
                return Err(ValidationError::IndexExpectedToBeArrayOfEvenLength)
            }
            let mut index: Vec<(u32, usize)> = Vec::new();

            for chunk in vec_of_possibly_integers.chunks(2) {
                match chunk {
                    &[PdfObject::Integer(c0), PdfObject::Integer(c1)] if c0 >= 0 && c1 >= 0 => {
                        index.push( (c0 as u32, c1 as usize) );
                    },
                    _ => {
                        return Err(ValidationError::IndexElementsOtherThanPositiveIntegers)
                    },
                }
            }

            // TODO some more bounds checking here ugh
        },
        None => {
            let default_index: Vec<(u32, usize)> = vec![(0u32, result.size.unwrap())];
            result.index = Some(default_index);
        },
        _ => { return Err(ValidationError::IndexExpectedToBeArray)}
    }

    match xref_stream_dict.get2(b"W") {
        Some(PdfObject::Array(vec_of_possibly_3_integers)) => {
            if vec_of_possibly_3_integers.len() != 3 {
                return Err(ValidationError::WExpectedToBeArrayOfLength3)
            }

            let w_0 = match vec_of_possibly_3_integers[0] {
                PdfObject::Integer(v0) if v0 >= 0 => {
                    v0 as usize
                },
                _ => { return Err(ValidationError::WElementsMustBeNonNegative)}
            };
            let w_1 = match vec_of_possibly_3_integers[1] {
                PdfObject::Integer(v0) if v0 >= 0 => {
                    v0 as usize
                },
                _ => { return Err(ValidationError::WElementsMustBeNonNegative)}
            };
            let w_2 = match vec_of_possibly_3_integers[2] {
                PdfObject::Integer(v0) if v0 >= 0 => {
                    v0 as usize
                },
                _ => { return Err(ValidationError::WElementsMustBeNonNegative)}
            };

            result.w = Some((w_0, w_1, w_2));
        },
        _ => { return Err(ValidationError::WExpectedToBeArray)}
    }

    match xref_stream_dict.get2(b"Prev") {
        Some(PdfObject::Integer(value)) if value >= 0i64 => {
            result.prev = Some(value as u64);
        },
        None => {
        }
        _ => {
            return Err(ValidationError::NoGoodPrevInXRefStreamDictionary)
        }
    }

    Ok(result)

}



#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::recognize_pdf_dictionary;

    macro_rules! vxsdt {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    match recognize_pdf_dictionary(input) {
                        Ok((_b, PdfObject::Dictionary(name_map))) => {
                            let result = validate_xref_stream_dictionary(&name_map);
                            assert_eq!( expected, result );
                        },
                        _ => {
                            assert_eq!(310,0);
                        }
                    }
                }
            )*
        }
    }

    vxsdt! {
        a1: (b"<<>>", Err(ValidationError::EmptyDictionary)),
        a2: (b"<< /b/c >>", Err(ValidationError::NoLengthInXRefStreamDictionary)),
        a3: (b"<< /Length true /b/c >>", Err(ValidationError::NoGoodLengthInXRefStreamDictionary)),
        a4: (b"<< /Length 104 /b/c >>", Err(ValidationError::NoSizeInXRefStreamDictionary)),
        a5: (b"<< /Length -104 /b/c >>", Err(ValidationError::NoGoodLengthInXRefStreamDictionary)),
        a6: (b"<< /Length 104 /Size false /b/c >>", Err(ValidationError::NoGoodSizeInXRefStreamDictionary)),
        a7: (b"<< /Length 104 /Size 108 /b /c /Filter <</d -1.2>>>>", Err(ValidationError::NoGoodFilterInXRefStreamDictionary)),
        a8: (b"<< /Length 104 /Size 108 /b /c /Filter /ASCIIHexDecode /DecodeParams true>>",
            Err(ValidationError::DecodeParamsExpectedToBeDictionaryOrAbsent)),
        a9: (b"<< /Length 104 /Size 108 /b /c /Filter [/ASCIIHexDecode] /DecodeParams [true]>>",
            Err(ValidationError::DecodeParamValueExpectedToBeDictionaryOrNull)),
        a10: (b"<</Length 104 /Size 108 /Index true>>", Err(ValidationError::IndexExpectedToBeArray)),
        a11: (b"<</Length 104 /Size 108 /Index [1 2 3]>>", Err(ValidationError::IndexExpectedToBeArrayOfEvenLength)),
        a12: (b"<</Length 104 /Size 108 /Index [-1 2]>>", Err(ValidationError::IndexElementsOtherThanPositiveIntegers)),
        a13: (b"<</Length 104 /Size 108 /Index [1 2]>>", Err(ValidationError::WExpectedToBeArray)),

        a14: (b"<</Length 104 /Size 108 /W << /a/b>>>>", Err(ValidationError::WExpectedToBeArray)),
        a15: (b"<</Length 104 /Size 108 /W [0 1]>>", Err(ValidationError::WExpectedToBeArrayOfLength3)),
        a16: (b"<</Length 104 /Size 108 /W [0 -1 2]>>", Err(ValidationError::WElementsMustBeNonNegative)),

        a17: (b"<</Length 104 /Size 108 /W[1 2 1]>>",
            Ok(XRefStream {length: Some(104), size: Some(108), w: Some((1,2,1)),
            index: Some(vec![(0, 108)]), ..Default::default() })
        ),
    }

}
