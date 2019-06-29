# parsepdf

Try to learn nom5, rust, and the PDF 2.0 spec by parsing PDFs.

# Design

At the level of PDF objects, parser combinators are used.

Combinators are not used at the level of PDF file structure, where the validation of offsets in the PDF,
and the length and interpretation of streams are of paramount importance.  The object structure needs
to be valid, but the values conveyed must be validated and checked among each other.

It is evident that a way to have facts learned about the contents of the PDF need to be compared to
statements the PDF makes, and a structure outside the PDF objects itself is needed

# 2019-06-29

describe-pdf binary can understand PDFs that have multiple updates, but only when all the updates
are plain old cross-reference tables, not XrefStm objects.  Filters and them XrefStm objects are next
on the file-api branch.

# 2019-06-23

describe-pdf binary can understand PDFs that only use 1.4 and earlier version cross-reference tables,
and only have one such table, as of this writing.

