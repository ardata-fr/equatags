ns <- c(
  m = "http://schemas.openxmlformats.org/officeDocument/2006/math",
  w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main",
  a = "http://schemas.openxmlformats.org/drawingml/2006/main"
)
word_rpr <- "<w:rPr><w:sz w:val=\"32\"/><w:szCs w:val=\"32\"/></w:rPr>"

test_that("without rpr, the output is not changed", {
  x <- c("\\frac{a+b}{c}", "\\sum_{i=1}^{n} x_i")
  expect_identical(
    transform_mathjax(x, to = "mml", rpr = NULL),
    transform_mathjax(x, to = "mml")
  )
})

test_that("Word run properties are in runs and control properties", {
  x <- transform_mathjax("\\frac{a+b}{c}", to = "mml", rpr = word_rpr)
  doc <- xml2::read_xml(x)
  runs <- xml2::xml_find_all(doc, "//m:r", ns = ns)
  expect_gt(length(runs), 0)
  expect_length(xml2::xml_find_all(doc, "//m:r/w:rPr/w:sz", ns = ns), length(runs))
  ctrl <- xml2::xml_find_all(doc, "//m:f/m:fPr/m:ctrlPr/w:rPr/w:sz", ns = ns)
  expect_length(ctrl, 1)
  expect_equal(xml2::xml_attr(ctrl[[1]], "val"), "32")
  # m:ctrlPr is the last child of m:fPr
  last <- xml2::xml_find_first(doc, "//m:fPr/*[last()]", ns = ns)
  expect_equal(xml2::xml_name(last, ns = ns), "m:ctrlPr")
})

test_that("PowerPoint run properties are in runs", {
  x <- transform_mathjax("x_i", to = "mml", rpr = "<a:rPr sz=\"1600\"/>")
  doc <- xml2::read_xml(x)
  runs <- xml2::xml_find_all(doc, "//m:r", ns = ns)
  rpr <- xml2::xml_find_all(doc, "//m:r/a:rPr", ns = ns)
  expect_length(rpr, length(runs))
  expect_equal(unique(xml2::xml_attr(rpr, "sz")), "1600")
})

test_that("run properties come after m:rPr and before m:t", {
  x <- transform_mathjax("\\infty", to = "mml", rpr = word_rpr)
  doc <- xml2::read_xml(x)
  run <- xml2::xml_find_first(doc, "//m:r[m:rPr]", ns = ns)
  expect_equal(
    xml2::xml_name(xml2::xml_children(run), ns = ns),
    c("m:rPr", "w:rPr", "m:t")
  )
})

test_that("a structure without properties gets them", {
  x <- transform_mathjax("x^2", to = "mml", rpr = word_rpr)
  doc <- xml2::read_xml(x)
  first <- xml2::xml_find_first(doc, "//m:sSup/*[1]", ns = ns)
  expect_equal(xml2::xml_name(first, ns = ns), "m:sSupPr")
  expect_length(
    xml2::xml_find_all(doc, "//m:sSup/m:sSupPr/m:ctrlPr/w:rPr", ns = ns),
    1
  )
})

test_that("rpr can be given per equation", {
  x <- transform_mathjax(
    c("a", "b", "c"),
    to = "mml",
    rpr = c(word_rpr, NA, "<w:rPr><w:sz w:val=\"20\"/></w:rPr>")
  )
  size_of <- function(omml) {
    doc <- xml2::read_xml(omml)
    xml2::xml_attr(xml2::xml_find_first(doc, "//m:r/w:rPr/w:sz", ns = ns), "val")
  }
  expect_equal(size_of(x[1]), "32")
  expect_true(is.na(size_of(x[2])))
  expect_equal(size_of(x[3]), "20")
  expect_identical(x[2], transform_mathjax("b", to = "mml"))
})

test_that("an invalid rpr gives an error", {
  expect_error(transform_mathjax("a", to = "mml", rpr = "<w:sz/>"), "rpr")
  expect_error(transform_mathjax("a", to = "mml", rpr = "not xml"), "rpr")
  expect_error(
    transform_mathjax(c("a", "b", "c"), to = "mml", rpr = c(word_rpr, word_rpr)),
    "rpr"
  )
})
