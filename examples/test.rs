extern crate parsepdf;

use parsepdf::signed_float;

fn main() {
    let v = signed_float(b"4.").to_result().unwrap();
    println!("{:?}", v);
}