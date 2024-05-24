
<!-- README.md is generated from README.Rmd. Please edit that file -->

# equatags

<!-- badges: start -->

[![R build
status](https://github.com/ardata-fr/equatags/workflows/R-CMD-check/badge.svg)](https://github.com/ardata-fr/equatags/actions)
<!-- badges: end -->

The goal of the package is to provide a tool to transform latex math
expressions into `HTML` format and `Office Open XML Math` format (which
can be used in a Word or PowerPoint document).

## Installation

You can install the development version from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("davidgohel/equatags")
```

## Example with flextable

``` r
library(flextable)

eqs <- c(
  "(ax^2 + bx + c = 0)",
  "a \\ne 0",
  "x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}")
df <- data.frame(formula = eqs)
df


ft <- flextable(df)
ft <- compose(
  x = ft, j = "formula",
  value = as_paragraph(as_equation(formula)))
ft <- align(ft, align = "center", part = "all")
ft <- width(ft, width = 2, j = "formula")
ft
```

## Related work

- Packages [texPreview](https://CRAN.R-project.org/package=texPreview)
  written by Jonathan Sidi, “compile snippets of ‘LaTeX’ directly into
  images from the R console to view in the ‘RStudio’ viewer pane, Shiny
  apps and ‘RMarkdown’ documents”. With this package, you can get images
  from your ‘latex’ code. The tool offers a wider functional spectrum
  than just equation processing and focuses on ‘latex’ instead of only
  ‘MathJax’ equations.

- Package [mathjaxr](https://cran.r-project.org/package=mathjaxr)
  written by Wolfgang Viechtbauer, “Using ‘Mathjax’ in Rd Files”. This
  package let you add equations in the manual pages of your package in a
  very convenient way.

- Package [katex](https://cran.r-project.org/package=katex) written by
  Jeroen Ooms, “Rendering Math to HTML, ‘MathML’, or R-Documentation
  Format”. ‘equatags’ mainly uses its functions.
