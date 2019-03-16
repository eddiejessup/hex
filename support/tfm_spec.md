# The TFM File Format

A TFM file is a sequence of bytes. When two or more 8-bit bytes are combined to form an integer of 16 or more bits, the most significant bytes appear first. This is called 'Big-Endian' order. All integers are unsigned.

# File Preamble

A TFM file begins with twelve 16-bit integers indicating the lengths of various subsequent portions of the file:

| Name |  Description                                           |
|------|--------------------------------------------------------|
| `lf` | The length of the entire file, in words                |
| `lh` | The length of the header data, in words                |
| `bc` | The smallest character code in the font                |
| `ec` | The largest character code in the font                 |
| `nw` | The number of words in the width table                 |
| `nh` | The number of words in the height table                |
| `nd` | The number of words in the depth table                 |
| `ni` | The number of words in the italic correction table     |
| `nl` | The number of words in the lig/kern table              |
| `nk` | The number of words in the kern table                  |
| `ne` | The number of words in the extensible character table  |
| `np` | The number of font parameter words                     |

We must have,

```
bc-1 <= ec <= 255
ne <= 256
lf = 6 + lh + (ec - bc + 1) + nw + nh + nd + ni + nl + nk + ne + np
```

Note that a font may contain as many as 256 characters (if `bc = 0` and `ec = 255`), and as few as 0 characters (if `bc = ec + 1`).

# File Body

The rest of the TFM file may be regarded as a sequence of ten data arrays of the form,

| Name        | Range         | Type                |
|-------------|---------------|---------------------|
| `header`    | [0 .. lh - 1] | miscellaneous       |
| `char_info` | [bc .. ec]    | `char_info_word`    |
| `width`     | [0 .. nw - 1] | `fix_word`          |
| `height`    | [0 .. nh - 1] | `fix_word`          |
| `depth`     | [0 .. nd - 1] | `fix_word`          |
| `italic`    | [0 .. ni - 1] | `fix_word`          |
| `lig_kern`  | [0 .. nl - 1] | `lig_kern_command`  |
| `kern`      | [0 .. nk - 1] | `fix_word`          |
| `exten`     | [0 .. ne - 1] | `extensible_recipe` |
| `param`     | [1 .. np]     | `fix_word`          |

A `fix_word` is a 32-bit representation of a signed binary fraction, whose negation is indicated by the two's complement of the entire word. 12 of the bits are to the left of the binary point. This implies that the largest `fix_word` value is 2048-2^{-20}, and the smallest is -2048. We will see below, however, that all but one of the `fix_word` values will lie between -16 and +16.

## `header` array

The header contains general facts about the font. It must contain at least two words. Further words may be needed to work with different kinds of devices.

`header[0,int32]` is a check-sum that TeX will copy into the DVI output file whenever it uses the font. When the DVI file is printed, the actual font that gets used is supposed to have the same check-sum. However, if the check-sum is zero in either the font file or the TFM file, no check is made.

`header[1,fw]` contains the font's design size, in units of TeX points (7227 TeX points = 254 cm). This number must be at least 1; usually the design size is 10 for a '10-point' font (a font that was designed to look best at a 10-point size). When a TeX user asks for a font at delta pt, the effect is to scale the coordinates of the points in the font image by (delta / design_size). Other than this value, `header[1]`, and `param[1]`, all dimensions are specified in design-size units, and must have an absolute value below 16. For example, `param[6]` (1 em, a.k.a. `quad`), is often the `fix_word` value `2^{20} = 1`, since many fonts have a design size equal to 1 em. Thus, `header[1]` and `param[1]` are the only `fix_word` entries whose first byte might not equal 0 or 255.

`header[2..11, BCPL]`, if present, contains 40 bytes that identify the character coding scheme. The first byte, which must be between 0 and 39, is the number of subsequent ASCII bytes in the string, which is intended to specify what character-code-to-symbol convention is present in the font. Examples are,

- "ASCII": standard ASCII
- "TeX text": for fonts like cmr10 and cmti9
- "TeX math extension": for cmex10
- "TeX math symbols"
- "TeX math italic"
- "euler substitutions only"
- "XEROX text": Xerox fonts
- "GRAPHIC": for special-purpose non-alphabetic fonts
- "UNSPECIFIED": for the default case when there is no information

The name should not contain parentheses. Such a string is said to be in "BCPL" format.

`header[12..16, BCPL]`, if present, names the font family (e.g., "CMR" or "HELVETICA"). This field is also known as the "font identifier".

`header[17]`, if present, contains a first byte called the `seven_bit_safe_flag`, then two bytes that are ignored, and a fourth byte called the `face`. If `face < 18`, it specifies the "weight, slope, and expansion": it is the sum of,

- 0 (medium), 2 (bold), 4 (light)
- 0 (roman), 1 (italic)
- 0 (regular), 6 (condensed), 12 (extended)

For example, 13 is 0 + 1 + 12, so it represents "medium-italic-extended". A three-letter code such as "MIE" may be used for such `face` data.

`header[18..]` might also be present; each word is simply called `header[18]`, `header[19]` and so on.

## `char_info` array

This contains one `char_info_word` per character, each containing six fields packed into four bytes:

Byte | Name           | Size (bits)
-----|----------------|-----------
1    | `width_index`  | 8
2    | `height_index` | 4
"    | `depth_index`  | 4
3    | `italic_index` | 6
"    | `tag`          | 2
4    | `remainder`    | 8

A character's width is `width[width_index]`, in design-size units. The index size for each dimension indicates how many different values a file may specify for that dimension.

The relation `width[0] = height[0] = depth[0] = italic[0] = 0` should hold, so that an index of zero implies a value of zero. A character is valid if and only if it lies between `bc` and `ec` and has a nonzero `width_index`.

The `tag` field in a `char_info_word` explains how to interpret the `remainder` field:

Value | Name        | Meaning
------|-------------|---------------------
0     |  `no_tag`   | Remainder is unused.
1     |  `lig_tag`  | Character has a ligature/kerning program starting at `lig_kern[remainder]`.
2     |  `list_tag` | Character is part of a chain of characters of ascending sizes, and not the largest in the chain. The `remainder` field gives the character code of the next larger character.
3     |  `ext_tag`  | Character represents an extensible character, i.e., a character that is built up of smaller pieces so that it can be made arbitrarily large. The pieces are specified in `exten[remainder]`.

## `lig_kern` array

Each element of this array is a `lig_kern_command`: a four-byte word describing how to handle special letter pairs in a simple programming language.

Byte |  Field Name | Meaning
-----|-------------|--------------------
1    | `skip_byte` | If `skip_byte >= 128`, this is the final program step; otherwise, skip `skip_byte` intervening steps to reach the next step.
2    | `next_char` | If `next_char` follows the current character, perform the operation and stop; otherwise, continue.
3    | `op_byte`   | If `op_byte` < 128, specifies a ligature step; otherwise, specifies a kern step.
4    | `remainder` | Depends if `op_byte` specifies a ligature or kern operation; see below.

In a kern step, insert space of `kern[256 * (op_byte - 128) + remainder]` between the current character and `next_char`. The space length may be negative or positive.

In a ligature step, the `op_byte` has structure `4a + 2b + c`, where `0 <= a <= b + c` and `0 <= b,c <= 1`. These values mean:

- Insert the character with code `remainder` between the current character and `next_char`
- If `b = 0`, delete the current character
- If `c = 0`, delete `next_char`
- Pass over `a` characters to reach the next current character (which may have a ligature/kerning program of its own)

For example, if `a = 0` and `b = 1`, do not change the current character; if `a = b` and `c = 1`, change the current character but do not change the next character.

If the first instruction of the array has `skip_byte = 255`, the instruction's `next_char` is the so-called right boundary character of this font; in this case `next_char` may lie outside `bc` to `ec`. If the last instruction of the array has `skip_byte = 255`, there is a special ligature/kerning program for a left boundary character, beginning at location `256 * op_byte + remainder`. TeX puts implicit boundary characters before and after each consecutive string of characters from the same font. These implicit characters do not appear in the output, but can affect ligatures and kerning.

If the first instruction of a character's `lig_kern` program has `skip_byte > 128`, the program actually begins in location `256 * op_byte + remainder`. This allows access to large `lig_kern` arrays, because the first instruction must otherwise appear in a location <= 255.

An instruction with `skip_byte > 128` in the `lig_kern` array must have `256 * op_byte + remainder < nl`. Such an instruction denotes an unconditional halt; no ligature command should be performed.

## `exten` array

Each element of this array is an `extensible_recipe`: four bytes called `top`, `mid`, `bot`, and `rep`, in this order, representing character codes to build a large symbol. If `top`, `mid`, or `bot` are zero, they are absent from the result. For example, an extensible vertical line is like an extensible bracket, except that the top and bottom pieces are missing.

## `param` array

Each `fix_word` has a particular meaning:

- `param[1]`: `slant`, the amount of italic slant, which is used to help position accents. For example, `slant = 0.25` means that when you go up one unit, you also go `0.25` units to the right. The slant is a pure number; it's the only `fix_word` other than the design size itself that is not scaled by the design size.
- `param[2]`: `space`, the normal spacing between words in text. Note that character " " in the font need not have anything to do with blank spaces.
- `param[3]`: `space_stretch`, the amount of glue stretching between words.
- `param[4]`: `space_shrink`, the amount of glue shrinking between words.
- `param[5]`: `x_height`, the height of letters for which accents don't have to be raised or lowered.
- `param[6]`: `quad`, the size of 1 em in the font.
- `param[7]`: `extra_space`, the amount added to `param[2]` at the ends of sentences.

When the character coding scheme is "TeX math symbols", the font should have 15 more parameters:

- `num1`
- `num2`
- `num3`
- `denom1`
- `denom2`
- `sup1`
- `sup2`
- `sup3`
- `sub1`
- `sub2`
- `supdrop`
- `subdrop`
- `delim1`
- `delim2`
- `axis_height`

When the character coding scheme is "TeX math extension", the font should have 6 more parameters:

- `default_rule_thickness`
- `big_op_spacing1` through `big_op_spacing5`.

# Acknowledgments

https://web.archive.org/web/20120722013525/http://www-users.math.umd.edu/~asnowden/comp-cont/tfm.html
