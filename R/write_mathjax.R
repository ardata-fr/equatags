# MathML to Office Open XML Math. All the equations are transformed by a
# single call to xml_xslt(), under a common root element "equatags" (see the
# template of "/" in mml2omml.xsl): each call of xml_xslt() compiles the
# stylesheet and keeps the memory of the compiled stylesheet until the end
# of the session (xslt 1.5.1).
#' @importFrom xslt xml_xslt
#' @importFrom xml2 xml_new_root
mml_to_ooml <- function(x) {
  if (!length(x)) {
    return(character())
  }
  doc <- read_xml(paste0(xml_header, "<equatags>", paste0(x, collapse = ""), "</equatags>"))
  doc <- prepare_mathml(doc)
  res <- xml_xslt(doc, mml2omml_stylesheet())
  ooml <- vapply(xml_children(res), function(node) as.character(xml_new_root(node)), "")
  gsub(xml_header, "", ooml, fixed = TRUE)
}
xml_header <- "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"

# The stylesheet is read once per session.
equatags_cache <- new.env(parent = emptyenv())
mml2omml_stylesheet <- function() {
  if (is.null(equatags_cache$stylesheet)) {
    equatags_cache$stylesheet <- read_xml(
      system.file(package = "equatags", "mml2omml.xsl")
    )
  }
  equatags_cache$stylesheet
}

mml_ns <- c(mml = "http://www.w3.org/1998/Math/MathML")

# A text made of space characters only.
# The characters are written in the R string, not as "\\x{...}" in the pattern:
# PCRE refuses code points above 255 when the text is ASCII (no UTF-8 mode).
space_chars_regex <- "^[ \u00a0\u2000-\u200b\u202f\u205f\u2060-\u2064\u3000]+$"

# Space characters for a width in em: em spaces (U+2003), then the closest of
# the en space (1/2 em), three-per-em (1/3), four-per-em (1/4) and six-per-em
# (1/6) spaces for the rest. Other units and negative widths give "".
em_to_spaces <- function(width) {
  if (!grepl("^[0-9.]+em$", width)) {
    return("")
  }
  w <- as.numeric(sub("em$", "", width))
  if (is.na(w) || w <= 0) {
    return("")
  }
  n_em <- floor(w + 1e-6)
  rest <- w - n_em
  spaces <- strrep("\u2003", n_em)
  if (rest >= 0.08) {
    sizes <- c("\u2002" = 1 / 2, "\u2004" = 1 / 3, "\u2005" = 1 / 4, "\u2006" = 1 / 6)
    spaces <- paste0(spaces, names(sizes)[which.min(abs(sizes - rest))])
  }
  spaces
}

# The stylesheet (MML2OMML.XSL from Microsoft) expects the MathML written by
# Word. KaTeX writes some constructs differently, they are rewritten here:
#
# - delimiters `<mo fence="true">` around content become `<mfenced>`, so that
#   they are converted to stretchy delimiters (`m:d`);
# - the argument of an n-ary operator (sum, integral...) is wrapped in an
#   `<mrow>`, so that it is written in the n-ary element (`m:e`) and not left
#   empty;
# - spaces (`\,`, `\;`, `\ `...) are written by KaTeX as text (`<mtext>`),
#   converted to normal text runs (`m:nor`) displayed with the font of the
#   text: they become `<mo>`, displayed with the math font;
# - `<mspace>` (`\quad`, `\qquad`), ignored by the stylesheet, becomes an
#   `<mo>` that contains space characters of the same width.
#
# Nodes are processed from the last to the first one, so that a node is
# rewritten before the node that contains it is copied. New nodes are created
# without namespace: the document is parsed again after each pass.
#' @importFrom xml2 xml_find_all xml_find_first xml_add_sibling xml_add_child
#' @importFrom xml2 xml_remove xml_name xml_text xml_set_attrs xml_attr
#' @importFrom xml2 xml_set_attr xml_name<- xml_text<-
prepare_mathml <- function(doc) {
  # vertical bars used as delimiters (|x|, \lvert, \left|): KaTeX writes
  # them with the characters of the relations \mid and \parallel, that Word
  # spaces as relations; the ASCII bar and the double vertical line are
  # delimiters for Word. \mid and \parallel (mo without these attributes)
  # are kept.
  bars <- c("\u2223" = "|", "\u2225" = "\u2016")
  for (node in xml_find_all(doc, "//mml:mi | //mml:mo", ns = mml_ns)) {
    ch <- xml_text(node)
    if (!ch %in% names(bars)) next
    if (xml_name(node) == "mi" ||
      identical(xml_attr(node, "stretchy"), "false") ||
      identical(xml_attr(node, "fence"), "true")) {
      xml_text(node) <- bars[[ch]]
    }
  }
  for (node in xml_find_all(doc, "//mml:mtext", ns = mml_ns)) {
    if (grepl(space_chars_regex, xml_text(node), perl = TRUE)) {
      xml_name(node) <- "mo"
    }
  }
  for (node in xml_find_all(doc, "//mml:mspace[@width]", ns = mml_ns)) {
    spaces <- em_to_spaces(xml_attr(node, "width"))
    if (nzchar(spaces)) {
      xml_name(node) <- "mo"
      xml_set_attr(node, "width", NULL)
      xml_text(node) <- spaces
    }
  }

  fenced <- xml_find_all(
    doc,
    "//mml:mrow[count(*) >= 2][*[1][self::mml:mo][@fence='true']][*[last()][self::mml:mo][@fence='true']]",
    ns = mml_ns
  )
  for (mrow in rev(fenced)) {
    children <- xml_children(mrow)
    mfenced <- xml_add_sibling(mrow, "mfenced", .where = "before")
    xml_set_attrs(
      mfenced,
      c(
        open = xml_text(children[[1]]),
        close = xml_text(children[[length(children)]]),
        separators = ""
      )
    )
    inner <- xml_add_child(mfenced, "mrow")
    for (child in children[-c(1, length(children))]) {
      xml_add_child(inner, child)
    }
    xml_remove(mrow)
  }
  doc <- read_xml(as.character(doc))

  scripts <- xml_find_all(
    doc,
    "//mml:*[self::mml:munder or self::mml:mover or self::mml:munderover or self::mml:msub or self::mml:msup or self::mml:msubsup][*[1][self::mml:mo]]",
    ns = mml_ns
  )
  for (script in rev(scripts)) {
    argument <- xml_find_first(script, "following-sibling::*[1]")
    if (
      inherits(argument, "xml_missing") ||
        xml_name(argument) %in% c("mrow", "mstyle")
    ) {
      next
    }
    mrow <- xml_add_sibling(argument, "mrow", .where = "before")
    xml_add_child(mrow, argument)
    xml_remove(argument)
  }
  read_xml(as.character(doc))
}

omml_ns <- c(m = "http://schemas.openxmlformats.org/officeDocument/2006/math")
rpr_ns <- c(
  w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main",
  a = "http://schemas.openxmlformats.org/drawingml/2006/main"
)

# OMML structures, each has a property element (`m:fPr` for `m:f`...) whose
# last child can be `m:ctrlPr`.
omml_structures <- c(
  "acc", "bar", "borderBox", "box", "d", "eqArr", "f", "func", "groupChr",
  "limLow", "limUpp", "m", "nary", "phant", "rad", "sPre", "sSub", "sSubSup",
  "sSup"
)

# Parse run properties given as a string, `<w:rPr>` (Word) or `<a:rPr>`
# (PowerPoint), the namespace declaration can be omitted.
#' @importFrom xml2 xml_child xml_ns
parse_rpr <- function(rpr) {
  doc <- tryCatch(
    read_xml(sprintf(
      "<root xmlns:w=\"%s\" xmlns:a=\"%s\">%s</root>",
      rpr_ns[["w"]], rpr_ns[["a"]], rpr
    )),
    error = function(e) NULL
  )
  if (!is.null(doc) && length(xml_children(doc)) == 1L) {
    node <- xml_child(doc, 1)
    prefix <- names(rpr_ns)[match(xml_ns_uri(node), rpr_ns)]
    if (xml_name(node) == "rPr" && !is.na(prefix)) {
      return(list(node = node, prefix = prefix))
    }
  }
  stop(
    "`rpr` must be a single `<w:rPr>` or `<a:rPr>` element: ",
    shQuote(rpr),
    call. = FALSE
  )
}

#' @importFrom xml2 xml_ns
xml_ns_uri <- function(node) {
  prefix <- sub(":.*$", "", xml_name(node, ns = rpr_ns))
  if (prefix %in% names(rpr_ns)) rpr_ns[[prefix]] else NA_character_
}

# Apply run properties to an OMML equation: in each run (`m:r`), after `m:rPr`,
# and in the control properties (`m:ctrlPr`) of each structure. Word needs
# both, the size of the symbols of a structure (fraction bar, sum sign,
# radical) comes from `m:ctrlPr`; PowerPoint only needs the runs.
#' @importFrom xml2 xml_root
set_run_properties <- function(omml, rpr) {
  props <- parse_rpr(rpr)
  doc <- read_xml(omml)
  root <- xml_root(doc)
  xml_set_attr(
    root,
    paste0("xmlns:", props$prefix),
    rpr_ns[[props$prefix]]
  )

  for (run in xml_find_all(doc, "//m:r", ns = omml_ns)) {
    first <- xml_child(run, 1)
    if (
      !inherits(first, "xml_missing") &&
        xml_name(first, ns = omml_ns) == "m:rPr"
    ) {
      xml_add_sibling(first, props$node, .where = "after")
    } else {
      xml_add_child(run, props$node, .where = 0)
    }
  }

  for (structure in omml_structures) {
    pr_name <- paste0("m:", structure, "Pr")
    nodes <- xml_find_all(doc, paste0("//m:", structure), ns = omml_ns)
    for (node in nodes) {
      pr <- xml_find_first(node, pr_name, ns = omml_ns)
      if (inherits(pr, "xml_missing")) {
        pr <- xml_add_child(node, pr_name, .where = 0)
      }
      ctrl <- xml_add_child(pr, "m:ctrlPr")
      xml_add_child(ctrl, props$node)
    }
  }

  # parse again to remove the namespace declarations repeated on each copy of
  # the run properties, then format as the equations without run properties
  doc <- read_xml(as.character(doc), options = c("NSCLEAN", "NOBLANKS"))
  gsub(xml_header, "", as.character(doc), fixed = TRUE)
}


#' @export
#' @title 'MathJax' equation as 'HTML' or 'MathML'.
#' @description Get 'HTML' or 'MathML' XML codes corresponding to the rendering
#' of 'MathJax' equations.
#' @param x MathJax equations
#' @param to output format, one of 'html' or 'mml'
#' @param display should the equation be in display (`TRUE`) or inline mode
#' (`FALSE`, by default)
#' @param strict KaTeX option.If `"ignore"` (default), allow features that make
#' writing LaTeX convenient. Other options are `"warn"` or `"error"`. This
#' happens E;G;, when you use accented characters in your equations.
#' @param rpr run properties to apply to the 'Office Open XML Math' equations,
#' used only when `to = "mml"`: a `<w:rPr>` element for 'Microsoft Word' or an
#' `<a:rPr>` element for 'Microsoft PowerPoint', as a string; the namespace
#' declaration can be omitted. It sets for example the size or the color of
#' the equations, that otherwise get the default values of the document. One
#' value for all equations or one value per equation, `NA` for none. With
#' `NULL` (default), no run properties are written.
#' @return a character vector that contains 'html' or 'mml'
#' codes corresponding to the equations.
#' @examples
#' x <- c("(ax^2 + bx + c = 0)",
#'        "x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.")
#' z <- transform_mathjax(x = x, to = "html")
#' cat(z, sep = "\n\n")
#' z <- transform_mathjax(x = x, to = "mml")
#' cat(z, sep = "\n\n")
#'
#' # equations at 16 points in a Word document
#' z <- transform_mathjax(
#'   x = x, to = "mml",
#'   rpr = "<w:rPr><w:sz w:val=\"32\"/><w:szCs w:val=\"32\"/></w:rPr>"
#' )
#' cat(z, sep = "\n\n")
#' @importFrom xml2 read_xml xml_children
#' @importFrom katex katex_mathml katex_html
transform_mathjax <- function(
  x,
  to = "html",
  display = FALSE,
  strict = "ignore",
  rpr = NULL
) {
  if (length(x) < 1) {
    return(character(0))
  }
  to <- match.arg(to, choices = c("html", "mml", "svg"), several.ok = FALSE)
  strict <- match.arg(
    strict,
    choices = c("ignore", "warn", "error"),
    several.ok = FALSE
  )
  if (!is.null(rpr)) {
    if (!is.character(rpr) || !length(rpr) %in% c(1L, length(x))) {
      stop(
        "`rpr` must be a character vector of length 1 or of the length of `x`.",
        call. = FALSE
      )
    }
    rpr <- rep_len(rpr, length(x))
  }

  if ("mml" %in% to) {
    info <- vapply(
      x,
      katex_mathml,
      "",
      displayMode = display,
      preview = FALSE,
      strict = strict
    )
    names(info) <- NULL
    info <- gsub("</span>", "", info, fixed = TRUE)
    info <- gsub("<span class=\"katex\">", "", info, fixed = TRUE)
    info <- mml_to_ooml(info)
    if (!is.null(rpr)) {
      with_rpr <- !is.na(rpr)
      info[with_rpr] <- mapply(
        set_run_properties,
        info[with_rpr],
        rpr[with_rpr],
        USE.NAMES = FALSE
      )
    }
  } else {
    info <- vapply(
      x,
      katex_html,
      "",
      preview = FALSE,
      include_css = TRUE,
      displayMode = display,
      strict = strict
    )
    names(info) <- NULL
  }

  info
}
