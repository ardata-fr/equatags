# Regression tests on a corpus of 457 equations (fixtures/corpus.csv, see
# fixtures/README.md): models written by equatiomatic, tests of KaTeX and
# matplotlib, classic formulas.
#
# - robustness: each equation is measured and converted to Word, or fails,
#   as expected (columns size_ok and word_ok); on CRAN, two equations of
#   each category;
# - reference results: size of each equation (0.01 point, compared within
#   0.02 point) and digest of its Word equation, compared with
#   _snaps/corpus/corpus-reference.csv. After an
#   intended change, the diff of this file lists the equations that change;
#   accept it with testthat::snapshot_accept("corpus").
#
# The results do not depend on the system: the layout reads the font
# STIX Two Math of 'gdtools' and needs no graphic device.

corpus <- corpus_read()

test_that("each equation of the corpus is measured and converted as expected", {
  # as skip_on_cran(): CRAN does not set NOT_CRAN
  on_cran <- !identical(Sys.getenv("NOT_CRAN"), "true") && !interactive()
  rows <- if (on_cran) which(corpus$cran) else seq_len(nrow(corpus))
  got <- corpus_check_all(corpus[rows, ])
  for (k in seq_along(rows)) {
    i <- rows[k]
    label <- sprintf("%s (%s)", corpus$id[i], corpus$tex[i])
    expect_identical(got$size_ok[k], corpus$size_ok[i],
      label = paste("equation_size() of", label, got$message[k]))
    expect_identical(got$word_ok[k], corpus$word_ok[i],
      label = paste("transform_mathjax() of", label))
  }
})

test_that("sizes and Word equations of the corpus do not change", {
  skip_on_cran()
  ok <- corpus$size_ok & corpus$word_ok
  got <- corpus_check_all(corpus[ok, ])
  reference <- data.frame(
    id = corpus$id[ok],
    width = sprintf("%.2f", got$width),
    height = sprintf("%.2f", got$height),
    depth = sprintf("%.2f", got$depth),
    word_md5 = got$word_md5
  )
  path <- tempfile(fileext = ".csv")
  utils::write.csv(reference, path, row.names = FALSE, quote = FALSE)
  expect_snapshot_file(path, "corpus-reference.csv", compare = compare_corpus_reference)
})
