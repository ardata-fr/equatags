# Draw an equation with 'grid'

Create a grob drawing an equation written in LaTeX with an OpenType math
font. The equation is laid out as by
[`equation_size()`](https://ardata-fr.github.io/equatags/dev/reference/equation_size.md);
the glyphs are drawn with
[`grid::glyphGrob()`](https://rdrr.io/r/grid/grid.glyph.html), fraction
bars and other rules with rectangles, so the size of the drawing is the
size computed.

The graphic device must support the drawing of glyphs (R \>= 4.3, see
`dev.capabilities()$glyphs`): 'ragg', 'svglite',
[`pdf()`](https://rdrr.io/r/grDevices/pdf.html) and
[`cairo_pdf()`](https://rdrr.io/r/grDevices/cairo.html) do.

The size of the grob is computed when it is created, from the font file
only: [`widthDetails()`](https://rdrr.io/r/grid/widthDetails.html),
[`heightDetails()`](https://rdrr.io/r/grid/widthDetails.html),
[`ascentDetails()`](https://rdrr.io/r/grid/widthDetails.html) and
[`descentDetails()`](https://rdrr.io/r/grid/widthDetails.html) return it
in "bigpts" without measuring anything, and no graphic device is needed
until the grob is drawn. To get the size without a device, use
[`equation_size()`](https://ardata-fr.github.io/equatags/dev/reference/equation_size.md)
(inches) or the numeric value of these units:
[`grid::convertWidth()`](https://rdrr.io/r/grid/grid.convert.html) opens
a device when none is open.

## Usage

``` r
equation_grob(
  x,
  font = gdtools::stixmath_file(),
  size = 12,
  display = FALSE,
  x0 = 0.5,
  y0 = 0.5,
  hjust = 0.5,
  vjust = 0.5,
  colour = "black",
  default.units = "npc",
  name = NULL,
  vp = NULL
)
```

## Arguments

- x:

  a LaTeX math expression (a single string).

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

- x0, y0:

  position of the equation, numeric values (in `default.units`) or
  units.

- hjust, vjust:

  justification of the equation box relative to its position (0 is left
  or bottom, 1 is right or top).

- colour:

  colour of the equation.

- default.units:

  units of `x0` and `y0` when they are numeric.

- name, vp:

  name and viewport of the grob.

## Value

A grob of class `equation_grob`.

## See also

[`equation_size()`](https://ardata-fr.github.io/equatags/dev/reference/equation_size.md)

## Examples

``` r
if (getRversion() >= "4.3.0") {
  library(grid)
  grid.newpage()
  grid.draw(equation_grob("\\frac{a+b}{c} = \\sqrt{x^2 + y^2}", size = 24))
}
```
