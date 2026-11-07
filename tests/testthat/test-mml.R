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
