# HeX

A reimplementation of the interpreter for the TeX programming language.

## Usage

### Prerequisites

HeX outputs DVI format files, as per the specification of TeX in the TeXbook. Unless you have a DVI viewer available, you might want to convert these files to PDF.

The program `gs` (Ghostscript) can perform this conversion, with the command `dvipdf`. It should be available from your package manager.

Run `hex --help` for usage information.

### Examples

There are example scripts in the `examples/` directory in the top-level of the repository. `test.tex` is intended mainly to test the program's functionality; the output isn't especially nice, so `example.tex` is recommended. To typeset this file,

- Change your working directory to `examples/`, so `hex` can easily find the font files
- Run `get_a_font.sh` to fetch the font file used by the test file, `cmr10.tfm`, into the current directory.
- Run `hex example.tex --output=example.dvi`, to generate a DVI-formatted file called `example.dvi`.
- Run `dvipdf example.dvi example.pdf` to convert the DVI file into a PDF file called `example.pdf`

## Rationale

TeX is a widely used program, but the program itself is written in a way that's very difficult to extend. This is just an artifact of its being written when structured programming was a fairly novel idea, and hardware constrained the extent to which design could dominate efficiency.

Because the original implementation is written as a monolithic program, it's hard to use parts of its logic, such as line- and page-breaking, and math typesetting, outside the original application.

With this project, I hope to hit a few targets:

- Maintain strict compatibility with the current specification, so that existing documents should compile the same with this implementation, as with `tex`.
- Expose TeX's logic in a way that's easier to understand and extend.
- Separate, as much as possible, the core typesetting logic, 'TeX the typesetter', from the language described by the specification, 'TeX the language'. This should let us develop ways to make beautiful documents, with interfaces that are more expressive and less bug-prone than the current language.

## Status

This is a work-in-progress, so many features are missing.

Features that are present:

- Line breaking
- Page breaking
- Macros without parameters
- Glue, kerns, rules
- Some syntactic commands (such as `\uppercase`, `csname`)
- Some typesetting commands (such as `\noindent`, `\penalty`)

Features that are absent:

- Hyphenation
- Math typesetting
- Macros with parameters
- Scoping
- Registers
- `\let` binding
- Many syntactic commands (such as `\expandafter`, `\string`)
- Many typesetting commands (such as `\accent`, `\lastbox`)
- Conditions

## Contributing

This is my first project using Haskell in anger, so contributions and advice are very welcome!

## See also

I've also written a more complete version of this same project, [implemented in Python](https://github.com/eddiejessup/nex). I came to realise that TeX was complicated enough that a type system was needed to maintain correctness and sanity, so I started this project.

## Acknowledgments

- [Donald Knuth](https://www-cs-faculty.stanford.edu/~knuth/vita.html), for TeX and the TeXbook
- [Fabrice Salvaire](https://www.fabrice-salvaire.fr/en/) for the [PyDVI](https://github.com/FabriceSalvaire/PyDVI) library, on which this project's TFM parsing logic is based.
- [Andrew Snowden](http://www-personal.umich.edu/~asnowden/) for the [description of the DVI language](https://web.archive.org/web/20070403030353/http://www.math.umd.edu/~asnowden/comp-cont/dvi.html), which was used to develop this project's DVI writing logic
