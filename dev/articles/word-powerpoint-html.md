# Equations in Word, PowerPoint and HTML

``` r

library(equatags)
```

[`transform_mathjax()`](https://ardata-fr.github.io/equatags/dev/reference/transform_mathjax.md)
converts equations written in LaTeX into the formats of documents:

- `to = "mml"`: ‘Office Open XML Math’, the native format of equations
  in Word and PowerPoint. The equations stay editable in Word or
  PowerPoint.
- `to = "html"`: the HTML of ‘KaTeX’, displayed with the CSS of ‘KaTeX’.

The equations are read by ‘KaTeX’: the [functions supported by
KaTeX](https://katex.org/docs/supported.html) can be used (fractions,
scripts, sums and integrals, roots, matrices and arrays,
`\left( \right)`, accents, arrows, `\text{}`, colours…). An equation
that ‘KaTeX’ can not read is an error.

## HTML

``` r

x <- transform_mathjax("x = \\frac{-b \\pm \\sqrt{b^2-4ac}}{2a}", to = "html")
substr(x, 1, 120)
#> [1] "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.css\" data-external"
```

The result starts with a link to the CSS of ‘KaTeX’ and can be written
as is in an HTML page or an R Markdown document (chunk option
`results = "asis"`):

``` r

cat(x)
```

$`x
= `$

## Inline and display style

Equations are inline by default, as `$...$` in LaTeX. With
`display = TRUE`, they use the display style, as `$$...$$`: larger
fractions, limits of sums above and below the operator.

``` r

cat(transform_mathjax("\\sum_{i=1}^{n} \\frac{x_i}{n}", to = "html"))
```

$`_{i=1}^{n}
`$

``` r

cat(transform_mathjax("\\sum_{i=1}^{n} \\frac{x_i}{n}", to = "html", display = TRUE))
```

``` math
_{i=1}^{n}
```

## Word

With `to = "mml"`, each equation is an `<m:oMath>` element:

``` r

eq <- transform_mathjax("x = \\frac{-b \\pm \\sqrt{b^2-4ac}}{2a}", to = "mml", display = TRUE)
cat(substr(eq, 1, 300))
#> 
#> <m:oMath xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math">
#>   <m:r>
#>     <m:t>x=</m:t>
#>   </m:r>
#>   <m:f>
#>     <m:fPr>
#>       <m:type m:val="bar"/>
#>     </m:fPr>
#>     <m:num>
#>       <m:r>
#>         <m:t>−b±</m:t>
#>       </m:r>
#>       <m:rad>
#>         <m:radPr>
#>           <m:degHide m:val="on"/
```

It can be inserted in a Word document with ‘officer’, in a paragraph
(`w:p`) written with
[`officer::body_add_xml()`](https://davidgohel.github.io/officer/reference/body_add_xml.html).
An equation alone in its paragraph is written in a math paragraph
(`m:oMathPara`), as Word does; an equation in a sentence is written next
to the runs of text:

``` r

library(officer)

ns_w <- "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
ns_m <- "http://schemas.openxmlformats.org/officeDocument/2006/math"

# an equation alone in its paragraph
alone <- sprintf(
  '<w:p xmlns:w="%s"><m:oMathPara xmlns:m="%s">%s</m:oMathPara></w:p>',
  ns_w, ns_m, eq
)
# an equation in a sentence
euler <- transform_mathjax("e^{i\\pi} + 1 = 0", to = "mml")
sentence <- sprintf(
  '<w:p xmlns:w="%s"><w:r><w:t xml:space="preserve">Euler: </w:t></w:r>%s</w:p>',
  ns_w, euler
)

doc <- read_docx()
doc <- body_add_par(doc, "The quadratic formula:")
doc <- body_add_xml(doc, str = alone)
doc <- body_add_xml(doc, str = sentence)
print(doc, target = tempfile(fileext = ".docx"))
```

Equations get the default font size of the document and the math font of
the document (‘Cambria Math’ unless another one is chosen in Word). The
argument `rpr` gives them run properties, for example a size of 16
points (`w:sz` is in half points) and a colour:

``` r

eq <- transform_mathjax(
  "a^2 + b^2 = c^2",
  to = "mml",
  rpr = '<w:rPr><w:sz w:val="32"/><w:color w:val="C0392B"/></w:rPr>'
)
```

`rpr` has one value for all equations or one value per equation.

## PowerPoint

In PowerPoint, the same equations are given run properties of PowerPoint
(`<a:rPr>`, size in hundredths of a point) and are embedded in a text
box. The simplest way is ‘flextable’, which writes the equations of its
cells in Word, PowerPoint and HTML, with the size and the colour of the
cell:

``` r

library(flextable)

dat <- data.frame(
  name = c("Quadratic formula", "Normal density"),
  formula = c(
    "x = \\frac{-b \\pm \\sqrt{b^2-4ac}}{2a}",
    "f(x) = \\frac{1}{\\sigma\\sqrt{2\\pi}} e^{-\\frac{(x-\\mu)^2}{2\\sigma^2}}"
  )
)
ft <- flextable(dat)
ft <- mk_par(ft, j = "formula", value = as_paragraph(as_equation(formula)))
ft <- autofit(ft)
ft
```

| name | formula |
|----|----|
| Quadratic formula |  $`x = \frac{-b \pm \sqrt{b^2-4ac}}{2a}`$ |
| Normal density |  $`f(x) = \frac{1}{\sigma\sqrt{2\pi}} e^{-\frac{(x-\mu)^2}{2\sigma^2}}`$ |

The same table goes to a Word document with
[`save_as_docx()`](https://davidgohel.github.io/flextable/reference/save_as_docx.html)
and to a PowerPoint presentation with
[`save_as_pptx()`](https://davidgohel.github.io/flextable/reference/save_as_pptx.html).
[`autofit()`](https://davidgohel.github.io/flextable/reference/autofit.html)
uses the size of the equations computed by
[`equation_size()`](https://ardata-fr.github.io/equatags/dev/reference/equation_size.md)
(see the article “Size and drawing of equations”).

## Accented characters

‘KaTeX’ warns about characters it does not know in math mode, such as
accented letters. With `strict = "ignore"` (the default), they are
accepted silently; `strict = "warn"` and `strict = "error"` report them.
