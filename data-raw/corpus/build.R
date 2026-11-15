# Builds the corpus of equations used by the tests
# (tests/testthat/fixtures/corpus.csv, see tests/testthat/test-corpus.R)
# from its sources, then checks each equation with this version of equatags:
# - size_ok: equation_size() gives a size, without warning;
# - word_ok: transform_mathjax(to = "mml") writes a non-empty equation.
# The result of the checks is the expected result of the tests; the
# equations that fail have a reason. Also writes corpus.rds (all columns).
# Run from the root of the package, after the sources:
#   Rscript data-raw/corpus/sources/classics.R
#   Rscript data-raw/corpus/sources/probes.R
#   python3 -I data-raw/corpus/sources/matplotlib.py
#   Rscript data-raw/corpus/build.R
# The equations of equatiomatic are read in sources/raw/equatiomatic.csv,
# kept in git (see sources/equatiomatic.R to make it again).
stopifnot("run from the root of the package" = file.exists("DESCRIPTION") && dir.exists("data-raw"))
pkgload::load_all(quiet = TRUE)

rows <- list()
add <- function(source, ref, category, display, tex, license, note = "") {
  rows[[length(rows) + 1L]] <<- data.frame(
    source = source, ref = ref, category = category, display = display,
    tex = tex, license = license, note = note, stringsAsFactors = FALSE
  )
}

# equatiomatic: tests and documentation (CC BY 4.0) --------------------------
eqm <- utils::read.csv("data-raw/corpus/sources/raw/equatiomatic.csv",
  stringsAsFactors = FALSE, encoding = "UTF-8")
for (i in seq_len(nrow(eqm))) {
  add(eqm$source[i], eqm$ref[i], "models (equatiomatic)", TRUE, eqm$tex[i], "CC BY 4.0 (equatiomatic)")
}

# KaTeX screenshot tests (MIT) -----------------------------------------------
ss <- yaml::read_yaml("data-raw/corpus/sources/raw/katex_ss_data.yaml")
for (nm in names(ss)) {
  v <- ss[[nm]]
  if (is.character(v)) {
    add("katex-tests", paste0("ss_data.yaml: ", nm), "katex features", FALSE, v, "MIT (KaTeX)")
  } else if (!is.null(v$tex)) {
    note <- paste(c(
      if (!is.null(v$macros)) "needs macros",
      if (!is.null(v$pre) || !is.null(v$post)) "html around",
      if (!is.null(v$styles)) "css styles"
    ), collapse = ", ")
    add("katex-tests", paste0("ss_data.yaml: ", nm), "katex features",
      isTRUE(as.logical(v$display %||% FALSE)), v$tex, "MIT (KaTeX)", note)
  }
}

# matplotlib mathtext tests (matplotlib license, BSD style) ------------------
# each $...$ of an expression (text outside is dropped); \$ is a dollar
mpl <- utils::read.delim("data-raw/corpus/sources/matplotlib.tsv", quote = "", stringsAsFactors = FALSE, encoding = "UTF-8")
for (i in seq_len(nrow(mpl))) {
  x <- gsub("\\\\\\$", "", mpl$expression[i])
  seg <- regmatches(x, gregexpr("\\$[^$]*\\$", x))[[1]]
  seg <- gsub("", "\\\\$", substring(seg, 2, nchar(seg) - 1))
  for (k in seq_along(seg)) {
    add("matplotlib-tests", sprintf("test_mathtext.py: math_tests[%d]%s", mpl$index[i],
      if (length(seg) > 1) paste0(" part ", k) else ""),
      "mathtext features", FALSE, seg[k], "matplotlib license (BSD style)",
      "mathtext dialect: some commands are not LaTeX")
  }
}

# classic formulas (written for the corpus) ---------------------------------
cl <- readRDS("data-raw/corpus/sources/classics.rds")
for (i in seq_len(nrow(cl))) {
  add("classics", cl$ref[i], cl$category[i], cl$display[i], cl$tex[i], "written for the corpus")
}

# equations already compared with Word (plans/equatags-equation-size.md) ----
word <- c(
  matrix = r"(\begin{matrix}a&b\\c&d\end{matrix})",
  smallmatrix = r"(\begin{smallmatrix}a&b\\c&d\end{smallmatrix})",
  pmatrix = r"(\begin{pmatrix}a&b\\c&d\end{pmatrix})",
  substack = r"(\sum_{\substack{i<n\\j<m}} x_{ij})",
  text_AV = r"(\text{AVAVAVAV})",
  mathrm_AV = r"(\mathrm{AVAVAVAV})",
  x_2prime = r"(x'')",
  f_prime = r"(f'(x))",
  x_sup_abc = r"(x^{abcdef})",
  e_sup = r"(e^{-x^2})",
  x_supsup = r"(x^{2^{2}})"
)
for (nm in names(word)) {
  add("equatags-word", paste0("compared with Word: ", nm), "word comparison", TRUE, word[[nm]], "written for the corpus")
}

# constructs compared with the HTML output of KaTeX, inline and display ---
pr <- readRDS("data-raw/corpus/sources/probes.rds")
for (display in c(FALSE, TRUE)) {
  for (i in seq_len(nrow(pr))) {
    add("equatags-probes", pr$ref[i], "constructs (probes)", display, pr$tex[i], "written for the corpus")
  }
}

corpus <- do.call(rbind, rows)
corpus$tex <- trimws(corpus$tex)
corpus <- corpus[nzchar(corpus$tex), ]
# the same equation from several places is kept once (first source)
corpus <- corpus[!duplicated(paste(corpus$display, corpus$tex)), ]
corpus$id <- sprintf("%s-%03d", corpus$source, stats::ave(seq_len(nrow(corpus)), corpus$source, FUN = seq_along))

# checks: the same functions as the tests ----------------------------------
source("tests/testthat/helper-corpus.R")
checks <- do.call(rbind, Map(corpus_check, corpus$tex, corpus$display))
corpus <- cbind(corpus, checks)
rownames(corpus) <- NULL

# reasons of the expected failures
reason_of <- function(ref, source, display) {
  if (source == "equatags-probes") {
    r <- pr[[if (display) "display" else "inline"]][pr$ref == ref]
    return(if (nzchar(r)) r else "unknown")
  }
  name <- sub("^ss_data.yaml: ", "", ref)
  if (source == "matplotlib-tests") return("mathtext dialect of matplotlib, not LaTeX")
  if (name %in% c("HorizontalBrackets", "Mapsfrom", "MathSfIt", "Reflectbox",
                  "StrikeThrough", "StrikeThroughColor")) return("command added to KaTeX after 0.16.9")
  if (name == "GroupMacros") return("macros defined by the KaTeX test")
  if (name == "UnsupportedCmds") return("invalid commands, on purpose")
  "unknown"
}
failing <- !corpus$size_ok | !corpus$word_ok
corpus$reason <- ""
corpus$reason[failing] <- mapply(reason_of, corpus$ref[failing], corpus$source[failing], corpus$display[failing])
if (any(corpus$reason == "unknown")) {
  print(corpus[corpus$reason == "unknown", c("source", "ref", "display", "tex")])
  stop("expected failures without a reason")
}
# a probe given a reason must fail
probes <- corpus$source == "equatags-probes"
expected <- mapply(function(ref, display) {
  nzchar(pr[[if (display) "display" else "inline"]][pr$ref == ref])
}, corpus$ref[probes], corpus$display[probes])
stopifnot(identical(unname(expected), failing[probes]))

# equations also tested on CRAN: the first two of each category that pass
ok <- !failing
corpus$cran <- FALSE
for (cat in unique(corpus$category)) {
  corpus$cran[which(ok & corpus$category == cat)[seq_len(2)]] <- TRUE
}
corpus$cran[is.na(corpus$cran)] <- FALSE

saveRDS(corpus, "data-raw/corpus/corpus.rds")
fixture <- corpus[, c("id", "source", "ref", "category", "display", "tex",
  "size_ok", "word_ok", "reason", "cran")]
utils::write.csv(fixture, "tests/testthat/fixtures/corpus.csv",
  row.names = FALSE, fileEncoding = "UTF-8")

cat("equations:", nrow(corpus), " expected failures:", sum(failing),
  " on CRAN:", sum(corpus$cran), "\n")
print(table(corpus$source))
print(table(corpus$reason[failing]))
