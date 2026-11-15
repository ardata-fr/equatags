# Equations of equatiomatic 0.4.9, read in its sources:
# 1. the expected outputs of its tests (testthat snapshots, _snaps/*.md);
# 2. the equations written by extract_eq() when running its vignettes and
#    the examples of its help pages (equatiomatic and the packages of the
#    models must be installed: the result depends on their versions).
# Writes data-raw/corpus/sources/raw/equatiomatic.csv (source, ref, tex),
# kept in git and read by build.R: this script is the record of how it was
# made, run again only to take the equations of another version of
# equatiomatic. Its argument is the folder of the sources of equatiomatic
# (https://github.com/datalorax/equatiomatic).
# Run from the root of the package:
#   Rscript data-raw/corpus/sources/equatiomatic.R path/to/equatiomatic
stopifnot("run from the root of the package" = file.exists("DESCRIPTION") && dir.exists("data-raw"))
src <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(src) || !file.exists(file.path(src, "DESCRIPTION")) ||
  !identical(read.dcf(file.path(src, "DESCRIPTION"), "Package")[[1]], "equatiomatic")) {
  stop("give the folder of the sources of equatiomatic as argument", call. = FALSE)
}
# the examples draw plots: no file (Rplots.pdf) in the package
options(device = function(...) grDevices::pdf(file = NULL))

# extract_eq() writes "$$ ... $$" (display); the delimiters are removed
strip_dollars <- function(x) {
  x <- trimws(paste(x, collapse = "\n"))
  trimws(sub("\\$\\$$", "", sub("^\\$\\$", "", x)))
}

# 1. snapshots ----------------------------------------------------------------
snap_rows <- list()
for (f in list.files(file.path(src, "tests/testthat/_snaps"), "\\.md$", full.names = TRUE)) {
  lines <- readLines(f, warn = FALSE)
  title <- ""
  i <- 1L
  k <- 0L
  while (i <= length(lines)) {
    if (grepl("^# ", lines[i])) title <- sub("^# ", "", lines[i])
    if (grepl("^    \\$\\$\\s*$", lines[i])) {
      j <- i + 1L
      while (j <= length(lines) && !grepl("^    \\$\\$\\s*$", lines[j])) j <- j + 1L
      body <- sub("^    ", "", lines[(i + 1L):(j - 1L)])
      k <- k + 1L
      snap_rows[[length(snap_rows) + 1L]] <- data.frame(
        source = "equatiomatic-tests",
        ref = sprintf("tests/testthat/_snaps/%s: %s #%d", basename(f), title, k),
        tex = strip_dollars(body),
        stringsAsFactors = FALSE
      )
      i <- j
    }
    i <- i + 1L
  }
}
snaps <- do.call(rbind, snap_rows)

# 2. vignettes and examples, with an extract_eq() that records its outputs ---
recorded <- new.env()
recorded$rows <- list()
recorded$where <- ""
record_eq <- function(...) {
  out <- equatiomatic::extract_eq(...)
  recorded$rows[[length(recorded$rows) + 1L]] <- data.frame(
    source = "equatiomatic-docs",
    ref = recorded$where,
    tex = strip_dollars(as.character(out)),
    stringsAsFactors = FALSE
  )
  out
}
run_code <- function(code, where) {
  recorded$where <- where
  env <- new.env(parent = globalenv())
  suppressPackageStartupMessages(library(equatiomatic))
  assign("extract_eq", record_eq, envir = env)
  # each expression on its own: an error (missing package...) skips it only
  exprs <- tryCatch(parse(text = code, keep.source = FALSE), error = function(e) expression())
  for (e in exprs) {
    tryCatch(
      suppressWarnings(suppressMessages(utils::capture.output(eval(e, env)))),
      error = function(err) NULL
    )
  }
}
for (f in list.files(file.path(src, "vignettes"), "\\.Rmd$", full.names = TRUE)) {
  r <- tempfile(fileext = ".R")
  knitr::purl(f, output = r, quiet = TRUE, documentation = 0)
  run_code(readLines(r, warn = FALSE), paste0("vignettes/", basename(f)))
}
for (f in list.files(file.path(src, "man"), "\\.Rd$", full.names = TRUE)) {
  r <- tempfile(fileext = ".R")
  tools::Rd2ex(f, r, commentDontrun = FALSE, commentDonttest = FALSE)
  if (file.exists(r)) run_code(readLines(r, warn = FALSE), paste0("man/", basename(f)))
}
docs <- do.call(rbind, recorded$rows)
grDevices::graphics.off()

out <- rbind(snaps, docs)
out <- out[nzchar(out$tex), ]
cat("snapshots:", nrow(snaps), " docs:", nrow(docs), "\n")
print(table(out$ref[out$source == "equatiomatic-docs"]))
utils::write.csv(out, "data-raw/corpus/sources/raw/equatiomatic.csv",
  row.names = FALSE, fileEncoding = "UTF-8")
