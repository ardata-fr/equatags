# Corpus of equations (fixtures/corpus.csv, built by data-raw/corpus/build.R):
# reading and checking, shared by test-corpus.R and the build script.

corpus_read <- function() {
  path <- testthat::test_path("fixtures", "corpus.csv")
  if (!file.exists(path)) {
    # sourced by data-raw/corpus/build.R
    path <- "../../tests/testthat/fixtures/corpus.csv"
  }
  utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")
}

# Checks of an equation, at 12 points:
# - size_ok: equation_size() gives a size, without warning (KaTeX warnings
#   and errors are warnings of equation_size());
# - width, height, depth: the size, in points, rounded to 0.01 point;
# - word_ok: transform_mathjax(to = "mml") writes a non-empty equation;
# - word_md5: digest of this equation.
corpus_check <- function(tex, display) {
  msg <- NULL
  s <- withCallingHandlers(
    tryCatch(
      equation_size(tex, display = display),
      error = function(e) {
        msg <<- conditionMessage(e)
        data.frame(width = NA_real_, height = NA_real_, depth = NA_real_)
      }
    ),
    warning = function(w) {
      msg <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  omml <- tryCatch(
    transform_mathjax(tex, to = "mml", display = display),
    error = function(e) NA_character_
  )
  word_ok <- length(omml) == 1 && !is.na(omml) && nzchar(omml)
  md5 <- NA_character_
  if (word_ok) {
    # digest of the bytes of the equation, with "\n" line endings: a file
    # written in text mode would have "\r\n" on Windows
    f <- tempfile()
    writeBin(charToRaw(enc2utf8(gsub("\r\n", "\n", omml, fixed = TRUE))), f)
    md5 <- unname(tools::md5sum(f))
    unlink(f)
  }
  data.frame(
    size_ok = !is.na(s$width) && is.null(msg),
    width = round(s$width * 72, 2),
    height = round(s$height * 72, 2),
    depth = round(s$depth * 72, 2),
    word_ok = word_ok,
    word_md5 = md5,
    message = if (is.null(msg)) "" else substr(gsub("\\s+", " ", msg), 1, 160),
    stringsAsFactors = FALSE
  )
}

corpus_check_all <- function(corpus) {
  out <- do.call(rbind, Map(corpus_check, corpus$tex, corpus$display))
  rownames(out) <- NULL
  out
}

# Comparison of two reference files (argument `compare` of
# expect_snapshot_file()): same equations, sizes within 0.02 point, same
# digests. A tolerance and not an equality of the text: a size close to a
# limit of rounding (58.035 points) is rounded to 58.04 or 58.03 depending on
# the processor.
compare_corpus_reference <- function(old, new) {
  a <- utils::read.csv(old, colClasses = "character")
  b <- utils::read.csv(new, colClasses = "character")
  if (!identical(a$id, b$id)) {
    return(FALSE)
  }
  sizes <- c("width", "height", "depth")
  gap <- abs(
    as.numeric(as.matrix(a[sizes])) - as.numeric(as.matrix(b[sizes]))
  )
  all(gap <= 0.02 + 1e-9) && identical(a$word_md5, b$word_md5)
}
