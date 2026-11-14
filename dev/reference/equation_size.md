# Size of equations

Compute the size of equations written in LaTeX, as they are displayed
with an OpenType math font.

The equations are converted to MathML with 'KaTeX' (the MathML used by
[`transform_mathjax()`](https://ardata-fr.github.io/equatags/dev/reference/transform_mathjax.md)),
then laid out with the rules of TeX and the constants of the 'MATH'
table of the font: spaces between operators, fractions, scripts, limits
of large operators, radicals, stretchy delimiters, matrices, accents.
Words and spaces have the widths stored in the font file.

The result does not depend on a graphic device or an application, only
on the font file. It is close to what 'Microsoft Word' displays with the
same font (a few percent on usual equations), see the section
"Differences with Word and browsers".

## Usage

``` r
equation_size(x, font = gdtools::stixmath_file(), size = 12, display = FALSE)
```

## Arguments

- x:

  a character vector of LaTeX math expressions.

- font:

  an OpenType math font, i.e. a font with a 'MATH' table: the path of a
  font file, or a family name such as "STIX Two Math", "Latin Modern
  Math", "Libertinus Math" or "Cambria Math", found with 'systemfonts'
  (fonts registered with
  [`systemfonts::register_font()`](https://systemfonts.r-lib.org/reference/register_font.html)
  can be used). The default is the file of 'STIX Two Math' shipped with
  'gdtools'
  ([`gdtools::stixmath_file()`](https://davidgohel.github.io/gdtools/reference/stixmath_file.html)):
  the results are then the same on every machine. Use
  `font = "STIX Two Math"` for the version installed on the system, if
  any.

- size:

  font size in points.

- display:

  if TRUE, use the display style (large operators with limits above and
  below, larger fractions), otherwise the inline style.

## Value

A data.frame with one row per equation and columns `width`, `height`
(above the baseline) and `depth` (below the baseline), in inches.
Equations using a construct that is not supported give `NA` values, with
a warning.

## Differences with Word and browsers

The layout follows the rules of TeX, as 'KaTeX' and LaTeX do. Compared
with 'Microsoft Word' displaying the equation in display style with the
same font, the median difference of width is about 1% on a set of usual
equations. The known differences are:

- Word spaces binary operators and fractions a little less than TeX:
  about 0.17 em less around each binary operator (2 points at 12 points,
  as in `a + b`), and no space of the null delimiters around fractions
  (0.12 em on each side). Equations with such operators or fractions are
  a few percent wider than in Word;

- some equations are given a little more space than in Word, which is
  harmless when the size is used to set the width of a column: the
  height of matrices in parentheses (up to 20%), the width of matrices
  in brackets (about 9%), the height of names of functions with an
  exponent, such as `\sin^2 x` (about 20%), the width of
  `\left( ... \right)` around a fraction (up to 15%);

- the size of scripts is an upper bound: Word rounds it down to a half
  point and displays equations with scripts up to about 4% narrower;
  'Microsoft PowerPoint' does not use the script style glyphs of the
  font and displays them up to about 12% narrower;

- Word uses the font 'Cambria Math' unless the document sets another
  math font: the equations are then about 5% wider;

- browsers displaying the HTML output of 'KaTeX' use the fonts of
  'KaTeX': the sizes differ by about 7%. The environment `array` is also
  1 em wider in browsers (and in LaTeX), which add half the space
  between columns before its first column and after its last one; Word
  does not, and neither does this function;

- a line break (`\\` or `\newline`) in the row of the equation starts a
  new line, aligned on the left, as in the HTML output of 'KaTeX'; the
  baseline of the equation is the baseline of its first line. In a group
  (`{a \\ b}`) or a fraction, it is ignored, as in 'KaTeX'. LaTeX
  ignores these line breaks in math mode;

- in display style, limits asked with `\limits` in a fraction or a
  script become scripts, as the limits of `\sum`: 'KaTeX' writes both in
  the same way.

## See also

[`equation_grob()`](https://ardata-fr.github.io/equatags/dev/reference/equation_grob.md)
to draw an equation with 'grid'.

## Examples

``` r
equation_size(c("\\frac{a+b}{c}", "x^2 + \\sqrt{y_i}"))
#>       width    height      depth
#> 1 0.2701833 0.1845333 0.09913333
#> 2 0.6187407 0.1465000 0.06433333
equation_size("\\sum_{i=1}^{n} x_i", display = TRUE)
#>       width    height     depth
#> 1 0.3531278 0.2637333 0.1801333
```
