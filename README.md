# parsepdf

Try to learn nom6, rust, and the PDF 2.0 spec by parsing and validating PDFs.

# Design

At the level of PDF objects, parser combinators are used.

Combinators are not used at the level of PDF file structure, where the validation of offsets in the PDF,
and the length and interpretation of streams are of paramount importance.  The object structure needs
to be valid, but the values conveyed must be validated and checked among each other.

We need to be able to compare what a PDF claims with what is learned about the PDF independently.

# 2021-06-02

nom6 again, and let's try to approach validation again somehow.

# 2020-12-24

nom6.  approach validation.

# 2019-06-29

describe-pdf binary can understand PDFs that have multiple updates, but only when all the updates
are plain old cross-reference tables, not XrefStm objects.  Filters and them XrefStm objects are next
on the file-api branch.

# 2019-06-23

describe-pdf binary can understand PDFs that only use 1.4 and earlier version cross-reference tables,
and only have one such table, as of this writing.

