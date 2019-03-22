# The DVI file format

A DVI file is a stream of bytes, representing a sequence of commands. A command consists of a 1-byte operation code, then zero or more bytes specifying parameters. Each parameters may consist of several consecutive bytes; for example, the `set_rule` command takes two four-byte parameters. Integers occupying multiple bytes are encoded in big-endian format.

Parameters are usually non-negative integers; but four-byte-long parameters and parameters that denote distances are signed. Such parameters are given in two's complement notation. For example, a two-byte-long distance parameter has a value between `-2^15` and `2^15-1`.

A DVI file consists of a 'preamble', then a sequence of one or more 'pages', then a 'postamble'.

The preamble is simply a `pre` command, with its parameters that define the dimensions used in the file; this must come first.

Each 'page' consists of a `bop` command, followed by any number of other commands placing characters on a page, followed by an `eop` command. The pages appear in the order that they were generated, not in any particular numerical order.

Other than `nop` and `fnt_def` commands, which are allowed between any two commands, each `eop` command is immediately followed by a `bop` or `post` command; in the latter case, there are no more pages in the file, and the remaining bytes form the `postamble`.

Some parameters are 'pointers'. These occupy four bytes, and represent the location of some other byte in the file; the first byte is at 0, then comes location 1, and so on.

For example, one parameter of a `bop` command points to the previous `bop`; this eases reading the pages backwards, in case the results are for a device that stacks its output face up.

Suppose the preamble occupies bytes 0 to 99; the first page occupies bytes 100 to 999; the second page occupies bytes 1000 to 1999. The `bop` that starts at byte 1000 points to 100 and the `bop` that starts in byte 2000 points to 1000.

When a DVI-reading program reads the commands for a page, it should maintain several quantities:

- The current font number, `f`, an integer. This value is changed by `fnt` and `fnt_num` commands.
- The horizontal and vertical coordinates of the current position on the page, `h` and `v`. Both coordinates are zero at the upper left corner of the page.
- Spacing variables, `w`, `x`, `y`, and `z`. `w` and `x` are used for horizontal spacing, while `y` and `z` are used for vertical spacing.
- A stack of `(h, v, w, x, y, z)` values; the commands `push` and `pop` change the current level of operation. Note that the current font `f` is *not* pushed and popped, the stack contains only positional information.

`h`, `v`, `w`, `x`, `y`, and `z` are signed integers occupying up to 32 bits, including the sign. Since they represent physical distances, there is a small unit of measurement such that increasing h by 1 means moving a certain tiny distance to the right. The actual unit of measurement is variable, as explained below.

`w` sets the size of the the `h` units.

## Op-codes

The parameters are listed in the order they would appear in a file; the number in brackets indicates the number of bytes the parameter occupies.

### 0-127: `set_char_i`

Typeset character number `i` from font `f` such that the reference point of the character is at `(h, v)`. Then increase `h` by the width of that character. Note that a character may have zero or negative width, so one cannot be sure that h will advance after this command; but `h` usually increases.

### 128-131: `seti c[i]`

Same as `set_char_0`, except that character number `c` is typeset. `TeX82` uses the `set1` command for characters in the range `128 <= c < 256`. `TeX82` never uses the `set2` command, which is intended for processors that deal with oriental languages; but `DVItype` will allow character codes greater than 255, if they all have the same width as the character whose code is `c mod 256`.

### 132 `set_rule; a[4], b[4]`

Typeset a solid black rectangle of height `a` and width `b`, with its bottom left corner at `(h, v)`. Then increase `h` by `b`. If either `a <= 0` or `b <= 0`, nothing should be typeset. Note that if `b < 0`, the value of `h` will decrease even though nothing else happens.

Programs that typeset from DVI files should be careful to make the rules line up carefully with digitized characters, as explained in connection with the `rule_pixels` subroutine below.

### 133-136: `puti (1 <= i <= 4); c[i]`

Typeset character number `c` from font `f` such that the reference point of the character is at `(h, v)`. The `put` commands are exactly like the set commands, except that they output a character or rule without moving the reference point.

### 137: `put_rule; a[4], b[4]`

Same as `set_rule`, except that `h` is not changed.

### 138: `nop`

Do nothing. Any number of `nop`s may occur between DVI commands, but a `nop` cannot be inserted between a command and its parameters, or between two parameters.

### 139: `bop; c_0[4]..c_9[4], p[4]`

The ten `c_i` parameters can be used to identify pages, if a user wants to print only part of a DVI file; `TeX82` gives them the values of `\count0...\count9` at the time `\shipout` was invoked for this page.

`p` points to the previous `bop` command in the file, where the first `bop` has `p=-1`.

The command should:

- Set `(h, v, w, x, y, z) := (0, 0, 0, 0, 0, 0)`, and empty the stack.
- Set the current font `f` to an undefined value.

### 140: `eop`

End the page: Print what you have read since the previous `bop`. At this point the stack should be empty. (The DVI-reading programs that drive most output devices will have kept a buffer of the material that appears on the page that has just ended. This material is largely, but not entirely, in order by `v` coordinate and (for fixed `v`) by `h` coordinate; so it usually needs to be sorted into some order that is appropriate for the device in question. `DVItype` does not do such sorting.)

### 141: `push`

Push the current values of `(h, v, w, x, y, z)` onto the top of the stack; do not change any of these values. Note that f is not pushed.

### 142: `pop`

Pop the top of the stack and assign its values to `(h, v, w, x, y, z)`. The number of pops should never exceed the number of pushes.

### 143-146: `righti (1 <= i <= 4); b[i]`

Set `h := h + b`, i.e., move right `b` units. `b` is signed; if `b < 0`, the reference point should move left.

### 147-151: `wi (0 <= i <= 4); b[i]`

The `w0` command sets `h := h + w`; i.e., moves right `w` units. With luck, this parameterless command will usually suffice, because the same kind of motion will occur several times in succession.

The other `w` commands set `w := b` and `h := h + b`. `b` is signed.

### 152-156: `xi (0 <= i <= 4); b[i]`

Like `wi`, but involving `x`.

### 157-160: `downi (1 <= i <= 4); a[i]`

Like `righti`, except involving `v`

### 161-165: `yi (0 <= i <= 4); a[i]`

Like `wi`, but involving `v` and `y`.

### 166-170: `zi (0 <= i <= 4); a[i]`

Like `yi`, but involving `z`.

### 171-234: `fnt_num_i (0 <= i <= 63)`

Set `f := i`. Font `i` must previously have been defined by a `fnt_def` command.

### 235-238: `fnti (1 <= i <= 4); k[i]`

Set `f := k`. `TeX82` uses the `fnt1` command for font numbers in the range `64 <= k < 256`. `TeX82` never generates the `fnt2` command.

### 239-242: `xxxi (1 <= i <= 4); k[i], x[k]`

These commands are undefined; it functions as a `k + i + 1`-byte `nop` command unless special DVI-reading programs are being used. `TeX82` generates `xxx1` when a short enough `\special` appears, setting `k` to the number of bytes being sent. It is recommended that `x` be a string having the form of a keyword followed by parameters.

### 243-246: `fnt_defi (1 <= i <= 4); k[i], c[4], s[4], d[4], a[1], l[1], n[a+l]`

- `c`: the check-sum that `TeX` (or whatever program generated the DVI file) found in the TFM file for this font; `c` should match the check-sum of the font found by programs that read this DVI file.
- `s`: a fixed-point scale factor, which is applied to the character dimensions in font `k`. Font dimensions in TFM files are relative to this quantity, which is positive and less than `2^27`.
- `d`: similar to `s`; the `design size`. Thus, font `k` is to be used at `(mag * s)/(1000 * d)` times its normal size. `d` and `s` are given in the same units as other dimensions.
- `a`, `l`, `n`: a specification of the path to the font, which is an ASCII string of length `a + l`. `a` is the length of the `area` or directory, and `l` is the length of the font name itself; the standard local system font area should be used when `a = 0`. `n` contains the area in its first `a` bytes.

Font definitions must appear before the first use of a particular font number. Once font `k` is defined, it must not be defined again outside the postamble.

### 247: `pre; i[1], num[4], den[4], mag[4], k[1], x[k]`

The preamble contains basic information about the file as a whole and must come at the very beginning of the file.

- `i`: identifies the DVI format; currently this byte is usually set to 2. The value `i = 3` is used for an extended format that allows a mixture of right-to-left and left-to-right typesetting.
- `num` and `den`: positive integers that define the units of measurement; they are the numerator and denominator of a fraction by which all dimensions in the DVI file should be multiplied to get lengths in units of `10 ^ (-7)` meters. For example, there are exactly 7227 `TeX` points in 254 centimeters, and `TeX82` works with scaled points where there are 2^16 sp in a point, so `TeX82` sets `num = 25400000` and `den = 7227`. (`2^16 = 473628672`.)
- `mag`: what `TeX82` calls `\mag`, i.e., 1000 times the desired magnification. The actual fraction by which dimensions are multiplied is therefore `(m * n)/(1000 * d)`.
- `k` and `x`: a comment, of length `k` contained in `x`, where `0 <= k < 256`.

### 248: `post; p[4], num[4], den[4], mag[4], l[4], u[4], s[2], t[2]; <font-definitions>`

The last page in a DVI file is followed by `post`; this command introduces the postamble, which summarizes important facts that `TeX` has accumulated about the file, making it possible to print subsets of the data with reasonable efficiency.

- `p`: a pointer to the final `bop`. `num`, `den`, and `mag`, are duplicates of the quantities that appeared in the preamble.
- `l`, `u`:, respectively, the height-plus-depth of the tallest page and the width of the widest page, in the same units as other dimensions of the file. These numbers might be used by a DVI-reading program to position individual pages on large sheets of film or paper; however, the standard convention for output on normal size paper is to position each page so that the upper left-hand corner is exactly one inch from the left and the top. Experience has shown that it is unwise to design DVI-to-printer software that attempts cleverly to center the output; a fixed position of the upper left corner is easiest for users to understand and to work with. Therefore `l` and `u` are often ignored.
- `s`: the maximum stack depth (i.e., the largest excess of `push` commands over `pop` commands) needed to process this file.
- `t`: the total number of `bop` commands.
- `<font-definitions>`: any number of `fnt_def` commands as described above, possibly interspersed with `nop` commands. Each font number that is used in the DVI file must be defined exactly twice: Once before it is first selected by a `fnt` command, and once in the postamble.

### 249: `post_post; q[4], i[1]; <223's>`

The last part of the postamble, following the `post_post` byte that signifies the end of the font definitions.

- `q`: a pointer to the post command that started the postamble
- `i`: an identification byte. This is currently set to 2, as in the preamble.
- `<223's>`: Four or more bytes equal to 223.

## Acknowledgments

https://github.com/tmanderson/dvi-parser/wiki/DVI-Specification-Explained
