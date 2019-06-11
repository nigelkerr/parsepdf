# parsepdf

Try to learn nom5, rust, and the PDF 2.0 spec by parsing PDFs.

# Design

At the level of PDF objects, parser combinators are used.

Combinators are not used at the level of PDF file structure, where the validation of offsets in the PDF,
and the length and interpretation of streams are of paramount importance.  The object structure needs
to be valid, but the values conveyed must be validated and checked among each other.

