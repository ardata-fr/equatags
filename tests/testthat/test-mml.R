test_that("the argument of n-ary operators is in the n-ary element", {
  x <- transform_mathjax(
    c(
      "\\sum_{i=1}^{n} x_i",
      "\\int_0^\\infty e^{-x^2}\\,dx",
      "\\prod_i x_i + 1"
    ),
    to = "mml"
  )
  for (omml in x) {
    expect_match(omml, "<m:nary>", fixed = TRUE)
    expect_no_match(omml, "<m:e/>", fixed = TRUE)
  }
  doc <- xml2::read_xml(x[1])
  ns <- c(m = "http://schemas.openxmlformats.org/officeDocument/2006/math")
  expect_equal(
    xml2::xml_text(xml2::xml_find_first(doc, "//m:nary/m:e", ns = ns)),
    "xi"
  )
})

test_that("fences are stretchy delimiters", {
  x <- transform_mathjax(
    c(
      "\\begin{pmatrix} a & b \\\\ c & d \\end{pmatrix}",
      "\\left[ \\frac{a}{b} \\right]"
    ),
    to = "mml"
  )
  for (omml in x) {
    expect_match(omml, "<m:d>", fixed = TRUE)
  }
  doc <- xml2::read_xml(x[2])
  ns <- c(m = "http://schemas.openxmlformats.org/officeDocument/2006/math")
  expect_equal(
    xml2::xml_attr(
      xml2::xml_find_first(doc, "//m:d/m:dPr/m:begChr", ns = ns),
      "m:val",
      ns = ns
    ),
    "["
  )
  expect_length(xml2::xml_find_all(doc, "//m:d/m:e/m:f", ns = ns), 1)
})

test_that("other equations are not changed", {
  x <- transform_mathjax("x^2 + \\sqrt{y_i}", to = "mml")
  expect_match(x, "<m:rad>", fixed = TRUE)
  expect_no_match(x, "<m:d>", fixed = TRUE)
  expect_no_match(x, "<m:nary>", fixed = TRUE)
})

test_that("spaces are written with the math font", {
  x <- transform_mathjax(c("a\\,b", "a\\;b", "a\\ b"), to = "mml")
  for (omml in x) {
    expect_no_match(omml, "<m:nor/>", fixed = TRUE)
  }
  doc <- xml2::read_xml(x[1])
  ns <- c(m = "http://schemas.openxmlformats.org/officeDocument/2006/math")
  expect_match(
    paste(xml2::xml_text(xml2::xml_find_all(doc, "//m:t", ns = ns)), collapse = ""),
    "\u2009",
    fixed = TRUE
  )
})

test_that("text keeps the font of the text", {
  x <- transform_mathjax("x \\text{ if } y", to = "mml")
  expect_match(x, "<m:nor/>", fixed = TRUE)
})

test_that("quad spaces are kept", {
  x <- transform_mathjax(c("a\\quad b", "a\\qquad b"), to = "mml")
  expect_match(x[1], "a\u2003b", fixed = TRUE)
  expect_match(x[2], "a\u2003\u2003b", fixed = TRUE)
})

test_that("widths in em are converted to space characters", {
  expect_equal(equatags:::em_to_spaces("1em"), "\u2003")
  expect_equal(equatags:::em_to_spaces("2.5em"), "\u2003\u2003\u2002")
  expect_equal(equatags:::em_to_spaces("0.1667em"), "\u2006")
  expect_equal(equatags:::em_to_spaces("-0.1667em"), "")
  expect_equal(equatags:::em_to_spaces("3pt"), "")
})

test_that("vertical bars used as delimiters are written as delimiters for Word", {
  bar <- transform_mathjax(c("|x|", "\\lvert x \\rvert", "\\left| x \\right|"), to = "mml")
  expect_false(any(grepl("\u2223", bar)))
  expect_true(all(grepl("|", bar, fixed = TRUE)))
  dbar <- transform_mathjax("\\lVert x \\rVert", to = "mml")
  expect_false(grepl("\u2225", dbar))
  expect_match(dbar, "\u2016")
  # \mid is a relation
  expect_match(transform_mathjax("a \\mid b", to = "mml"), "\u2223")
})

test_that("the argument display sets the style of Office Open XML Math", {
  inline <- transform_mathjax("\\sum_{i=1}^{n} x_i", to = "mml")
  display <- transform_mathjax("\\sum_{i=1}^{n} x_i", to = "mml", display = TRUE)
  expect_match(inline, "<m:limLoc m:val=\"subSup\"/>", fixed = TRUE)
  expect_match(display, "<m:limLoc m:val=\"undOvr\"/>", fixed = TRUE)
})

test_that("several equations are converted as each one alone", {
  x <- c("\\frac{a}{b}", "\\sum_{i=1}^n x_i", "|x| + \\color{red}{y}")
  together <- transform_mathjax(x, to = "mml")
  alone <- vapply(x, transform_mathjax, "", to = "mml", USE.NAMES = FALSE)
  expect_identical(together, alone)
  expect_identical(transform_mathjax(character(), to = "mml"), character())
  # the namespace of MathML is not written in the result
  expect_false(any(grepl("xmlns:mml", together, fixed = TRUE)))
})
