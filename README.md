
<!-- README.md is generated from README.Rmd. Please edit that file -->

# equatags

<!-- badges: start -->

[![R build
status](https://github.com/davidgohel/equatags/workflows/R-CMD-check/badge.svg)](https://github.com/davidgohel/equatags/actions)
<!-- badges: end -->

The goal of the package is to provide a tool to transform equations
expressed in `latex`, `MathML` or `ASCIIMathML` into `SVG` format
(i.e. as an image) and `Office Open XML Math` format (which can be used
in a Word or PowerPoint document).

## Installation

You can install the development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("davidgohel/equatags")
```

{equatags} relies heavily on npm package “mathjax-node”, a node binding
for MathJax. It is necessary to have “node.js” installed on your
computer to use this package.

### Setup

First ‘node.js’ must be available on your machine, please visit
<https://nodejs.org/> and follow the installation instructions. Then use
the following code to make sure everything is set up correctly. You need
a Internet access during this operation.

``` r
library(locatexec)
library(equatags)
if(exec_available("node", error = TRUE) && # check that node.js is available
   !mathjax_available()){ # check that "mathjax-node" is available
  mathjax_install()
}
```

## Example

### Simple usage

``` r
library(equatags)
x <- c("(ax^2 + bx + c = 0)",
       "x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}")
z <- transform_mathjax(x = x, to = "svg")

writeLines(z[1], "man/figures/eq1.svg", useBytes = TRUE)
writeLines(z[2], "man/figures/eq2.svg", useBytes = TRUE)
```

![equation 1](man/figures/eq1.svg)

![equation 2](man/figures/eq2.svg)

### Add an equation in package manual

This function could be used to add equations in package documentation.
See `@section illustration:` that illustrates its usage.

``` r
#' @export
#' @title add equations in R manuals
#' @description a sugar function to let you add
#' an equation in your roxygen tags.
#'
#' The formula is written in two files, a pdf and a svg version,
#' one for the PDF manual, one for the HTML help files.
#' @param str the equation
#' @param dir default to `man/figures` where images are expected to be
#' in R packages.
#' @param baseout the name of the result (with svg extension)
#' @section illustration:
#' This `"x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}."` is transformed to this:
#'
#' `r mathjax_rd("x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}", baseout = "mathjax-example.svg")`
#' @importFrom rsvg rsvg_pdf
#' @return a character vector containing Rd content
mathjax_rd <- function(str, dir = "man/figures", baseout = "mathjaxeq.svg"){

  svg_str <- transform_mathjax(x = str, to = "svg")
  path_svg <- file.path(dir, baseout)
  file_ <- file(path_svg, "w")
  writeLines(svg_str, file_, useBytes = TRUE)
  close(file_)
  path_pdf <- gsub("\\.svg$", ".pdf", path_svg)
  rsvg::rsvg_pdf(path_svg, path_pdf, width = if(is.null(width)) NULL else width*72)
  sprintf("\\if{html}{\\figure{%s}}\\if{latex}{\\figure{%s}}",
          basename(path_svg), basename(path_pdf))
}
```

## Related work

  - Packages [texPreview](https://CRAN.R-project.org/package=texPreview)
    written by Jonathan Sidi, “compile snippets of ‘LaTeX’ directly into
    images from the R console to view in the ‘RStudio’ viewer pane,
    Shiny apps and ‘RMarkdown’ documents.”. With this package, you can
    get images from your ‘latex’ code. The tool offers a wider
    functional spectrum than just equation processing and focuses on
    ‘latex’ instead of only ‘MathJax’ equations.
