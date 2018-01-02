#[macro_use]
extern crate nom;
#[macro_use]
extern crate approx;
extern crate regex;

use nom::*;
use nom::digit;
use nom::ErrorKind;
use nom::IResult::*;
use std::str;
use std::str::FromStr;

// parse a pdf file, per ISO 32000-2_2017(en)

mod structs;
mod simple;
mod nesting;

pub use structs::*;
pub use simple::*;
pub use nesting::*;


#[cfg(test)]
mod tests {
    use super::*;

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

