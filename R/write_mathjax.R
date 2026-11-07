#' @importFrom xslt xml_xslt
mml_to_ooml <- function(x) {
  z <- c(xml_header, x)

  doc <- read_xml(paste0(z, collapse = ""))
  doc <- prepare_mathml(doc)
  ooml <- xml_xslt(doc, mml2omml_stylesheet())
  ooml <- gsub(xml_header, "", as.character(ooml), fixed = TRUE)
  ooml
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
space_chars_regex <- "^[\\x{0020}\\x{00A0}\\x{2000}-\\x{200B}\\x{202F}\\x{205F}\\x{2060}-\\x{2064}\\x{3000}]+$"

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
  spaces <- strrep(" ", n_em)
  if (rest >= 0.08) {
    sizes <- c(" " = 1 / 2, " " = 1 / 3, " " = 1 / 4, " " = 1 / 6)
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
#' @return a character vector that contains 'html' or 'mml'
#' codes corresponding to the equations.
#' @examples
#' x <- c("(ax^2 + bx + c = 0)",
#'        "x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.")
#' z <- transform_mathjax(x = x, to = "html")
#' cat(z, sep = "\n\n")
#' z <- transform_mathjax(x = x, to = "mml")
#' cat(z, sep = "\n\n")
#' @importFrom xml2 read_xml xml_children
#' @importFrom katex katex_mathml katex_html
transform_mathjax <- function(
  x,
  to = "html",
  display = FALSE,
  strict = "ignore"
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

  if ("mml" %in% to) {
    info <- vapply(x, katex_mathml, "", preview = FALSE, strict = strict)
    names(info) <- NULL
    info <- gsub("</span>", "", info, fixed = TRUE)
    info <- gsub("<span class=\"katex\">", "", info, fixed = TRUE)
    info <- vapply(
      info,
      mml_to_ooml,
      FUN.VALUE = character(1L),
      USE.NAMES = FALSE
    )
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
