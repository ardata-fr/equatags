# equatags (development version)

## New features

- New function `equation_size()`: width, height and depth of equations
written in LaTeX. The equations are converted to MathML with 'KaTeX', then
laid out with the rules of TeX and the 'MATH' table of an OpenType math font.
The default font is the file of "STIX Two Math" shipped with 'gdtools', so
that the results are the same on every machine; another font can be given by
its file or its family name. The size does not depend on a graphic device or
an application, and is close to the one of 'Microsoft Word' with the same font
(see the section "Differences with Word and browsers" of `?equation_size`).
- New function `equation_grob()`: draws an equation with 'grid' (requires
R >= 4.3), with the same layout.
- Supported constructs: fractions and binomials, scripts and limits of large
operators, radicals, stretchy delimiters (`\left`, `\middle`, `\right`, `\big`),
matrices, arrays (column alignment, lines between columns and rows, frames)
and other environments of 'KaTeX', accents, horizontal braces and arrows
(`\overbrace`, `\xrightarrow`), `\overline` and `\underline`, `\overset` and
`\stackrel`, frames and strikes (`\boxed`, `\cancel`), colours and backgrounds
(`\color`, `\colorbox`), rules (`\rule`), spaces and phantoms, bold, italic and
other math alphabets.
- The layout of an equation is computed once per session and font, for all
sizes.

## Bug fixes

- In 'Office Open XML Math' (`to = "mml"`), vertical bars used as delimiters
(`|x|`, `\lvert`, `\lVert`, `\left|`, `\|x\|`) are written as delimiters:
Word displayed them with the spaces of a relation, such as `\mid`.
- `transform_mathjax(to = "mml")` now uses the argument `display`: the
equations were always in display style (limits of sums above and below
them). As documented, the default is the inline style: use `display = TRUE`
to get the previous result.
- `transform_mathjax(to = "mml")` failed on a text made of ASCII characters
only, such as `\text{ab}` ("character code point value in \x{} or \o{} is
too large").

## Performance

- `transform_mathjax(to = "mml")` converts all its equations with a single
XSL transformation. The package 'xslt' keeps in memory each stylesheet it
compiles until the end of the session (about 1.7 MB per transformation), and
equations were converted one by one: a document with 500 equations used
about 900 MB more. The namespace of MathML is no longer written in the
result.

## Dependencies

- 'equatags' now imports 'gdtools' (>= 0.6.0), for the font "STIX Two Math",
'systemfonts' (>= 1.2.0), to read the glyphs of fonts, and 'grid'.

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
