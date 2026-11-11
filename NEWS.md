# equatags (development version)

## Bug fixes

- In 'Office Open XML Math' (`to = "mml"`), vertical bars used as delimiters
(`|x|`, `\lvert`, `\lVert`, `\left|`, `\|x\|`) are written as delimiters:
Word displayed them with the spaces of a relation, such as `\mid`.
- `transform_mathjax(to = "mml")` now uses the argument `display`: the
equations were always in display style (limits of sums above and below
them). As documented, the default is the inline style: use `display = TRUE`
to get the previous result.

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
