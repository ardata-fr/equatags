# equatags (development version)

## New features

- `equation_size()` measures equations (width, height and depth) without
drawing them and without a graphic device, for example to size a table
cell or an image before the document is written. The results are the same
on every machine and close to what Word displays.
- `equation_grob()` draws an equation in a 'grid' graphic, for example in a
'ggplot2' plot (R >= 4.3).
- Both use the font "STIX Two Math", shipped with 'gdtools', by default;
another OpenType math font can be given by its name or its file.
- Most constructs of 'KaTeX' are supported. The known differences with Word
and browsers are listed in `?equation_size`. An equation that can not be
measured gives `NA` with a warning.

## Changes that can modify your documents

- In Word and PowerPoint, equations are now inline by default, as
documented: the argument `display` of `transform_mathjax()` was ignored and
every equation was displayed as a block (limits above and below sums,
larger fractions). Use `display = TRUE` to get the previous result.

## Bug fixes

- In Word and PowerPoint:
  - `|x|`, `\lvert x \rvert`, `\left| x \right|` and `\|x\|` are displayed
    as delimiters, no longer spaced as the relation "divides";
  - line breaks (`\\`, `\newline`) are kept: `a + b \\ c` was displayed as
    `a + bc`;
  - `\overlinesegment` and `\underlinesegment` no longer show the word
    "undefined";
  - integrals and sums without limits (`\int x\,dx`, `\sum x`) are larger
    in an equation displayed alone, as with limits;
  - `\big|`, `\Big|`... are no longer spaced as the relation "divides".
- Texts made of ASCII characters only, such as `\text{ab}`, no longer fail.
- `\label{...}`, as written by `equatiomatic::extract_eq(label = )`, is
ignored instead of causing an error.

## Performance

- Documents with many equations are created faster and use much less
memory: each equation kept about 1.7 MB until the end of the R session.

## Dependencies

- 'equatags' now needs 'gdtools' (>= 0.6.0) and 'systemfonts' (>= 1.2.0).

# equatags 0.3.0

## New features

- `transform_mathjax()` has a new argument `rpr` to set the run properties of
'Office Open XML Math' equations, such as their size or color: a `<w:rPr>`
element for 'Microsoft Word' or an `<a:rPr>` element for 'Microsoft
PowerPoint'. Without it, equations get the default size of the document.

## Bug fixes

These fixes concern the 'Office Open XML Math' output (`to = "mml"`) used in
'Microsoft Word' and 'Microsoft PowerPoint':

- The argument of sums, products and integrals is now written inside the
operator. It was written after it, and Word showed an empty placeholder in the
operator.
- Delimiters, such as the parentheses of a matrix or `\left( \right)`, now
stretch to the height of their content. They kept the height of a single line.
- `\quad` and `\qquad` are now written as spaces. They were dropped.
- Spaces such as `\,` or `\;` are no longer written as normal text.

## Performance

- The XSL stylesheet is read once per session instead of once per equation.

# equatags 0.2.2

## New features

- `transform_mathjax()` has a new argument `strict` (`"ignore"` by default) to
turn off the warnings issued when equations contain accented characters.

# equatags 0.2.1

## New features

- `transform_mathjax()` has a new argument `display` to generate display or
inline equations.

## Changes

- Equations are now generated inline by default (`display = FALSE`). They used
to be generated in display mode only.
- Unused tools related to 'mathjax-npm' are removed.

# equatags 0.2.0

## Changes

- The 'node.js' dependency is dropped in favor of the package 'katex'.

# equatags 0.1.1

## Changes

- `tools::R_user_dir()` is now used instead of `rappdirs::user_data_dir()`, as
required by the CRAN policy.
