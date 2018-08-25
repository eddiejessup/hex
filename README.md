# HeX

## Prerequisites

HeX outputs DVI format files, as per the TeX specification. Unless you have a DVI viewer available, you might want to convert these files to PDF.

The program `gs` (Ghostscript) can perform this conversion, with the command `dvipdf`. It should be available from your package manager.

## Example

Let's run the example script in the repository top-level, `test.tex`.

- Run `get_a_font.sh` fetches the font file used by the test file, `cmr10.tfm`, into the current directory.
- Run `hex test.tex --output=test.dvi` in the same directory, to obtain a DVI-formatted file called `test.dvi`.
- Run `dvipdf test.dvi test.pdf` to convert the DVI file into a PDF file
