# Equations of equatiomatic (0.4.9, sources in ~/gitprojects/opensource/equatiomatic):
# 1. the expected outputs of its tests (testthat snapshots, _snaps/*.md);
# 2. the equations written by extract_eq() when running its vignettes and
#    the examples of its help pages.
# Writes sources/equatiomatic.rds: data.frame(id, source, ref, tex).
# Run from data-raw/corpus: Rscript sources/equatiomatic.R
src <- path.expand("~/gitprojects/opensource/equatiomatic")

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
saveRDS(out, "sources/equatiomatic.rds")
