# parsepdf

Try to learn nom, rust, and the PDF 2.0 spec by parsing PDFs.

## 2018-01-07

started describe-pdf, my intent being: use the library functions to parse then describe an input PDF, in the hopes of
uncovering better ways to implement the library functions, or ways to wrap the PdfObjects for more ergonomic use.

learning various things about rust, though this feels like i have a long row of coffee tables ahead of me to trip over.

i wonder about using nom 4.0-ɑ1.

i wonder when this is interesting enough to release.  the PDF 2.0 spec is a large document with a lot of specification in
it.

## 2018-01-04

I think all the Object types are covered at this point, even Streams (so, § 7.3 Objects).

There are some things I don't like, in no particular order:

* the strategy for capturing and being able to refer to the bytes of a stream isn't scalable, we make a Vec<u8>, and in my current rust naïveté we clone that around.
* i feel i've written a lot of match summat() { ... } where there's one branch that's success, and often (because of nom's IResult usually) two or more branches that aren't.  over in Java we'd throw an exception we'd ignore later.  but how do i do all this safety succinctly?  find more uses of nom and what they do.
* it would be grand to have the PdfObject enum be able to be Eq, but alas it is not to be (given that a member of f64 inside can't ever be Eq sensibly)
* probably want to demote PdfObject::Comment to something like what white-space is already.

Even though we don't understand the contents of streams (§ 7.4 Filters), moving on to "basic conforming PDF file" (§ 7.5.1 General).

## 2018-01-02

re-organized i think better.  taking up the recursive objects, for which i will need to figure out how to capture the offsets that sub-parsers finish at.

## 2017-12-08

Recognizers for many objects writ.  Streams need a recognizer.
Some parsers also writ, but I feel they are not fully thought out yet.
I need to understand and come to terms with the nom consumer.
I need to organize the code better.
I need to learn all the things.
