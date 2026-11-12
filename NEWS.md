# equatags (development version)

## Changes that can modify your documents

- In Word and PowerPoint (`transform_mathjax(to = "mml")`), equations are now
displayed inline by default, as documented: the argument `display` was
ignored and every equation was displayed as a block (limits of sums above and
below, larger fractions). Use `display = TRUE` to keep the previous result.

## New features

- `equation_size()` gives the width, height and depth of equations written in
LaTeX, without drawing them and without a graphic device. Use it to size a
cell, a column or an image before the document is opened. The result is the
same on every machine, and close to what Word displays with the same font
(see "Differences with Word and browsers" in `?equation_size`).
- `equation_grob()` draws an equation in a 'grid' graphic, for example in a
'ggplot2' plot (R >= 4.3).
- Both functions use the font "STIX Two Math" by default, shipped with
'gdtools'; another math font can be given by its name or its file.
- Fractions, scripts, sums and integrals with their limits, roots, matrices
and arrays, delimiters such as `\left( \right)`, accents, braces, arrows,
frames, colours and most other constructs of 'KaTeX' are supported (see
`?equation_size`). An equation that can not be measured gives `NA` with a
warning.

## Bug fixes

- In Word, vertical bars around an expression (`|x|`, `\lvert x \rvert`,
`\left| x \right|`, `\|x\|`) were spaced as the relation "divides": they are
now displayed as delimiters.
- `transform_mathjax(to = "mml")` failed on texts made of ASCII characters
only, such as `\text{ab}`.

## Performance

- Documents with many equations are created faster and use much less memory:
each converted equation kept about 1.7 MB of memory until the end of the R
session.

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
