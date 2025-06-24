#' @importFrom xslt xml_xslt
mml_to_ooml <- function(x){

  z <- c(xml_header, x)

  style_doc <- read_xml(system.file(package = "equatags", "mml2omml.xsl"))

  doc <- read_xml(paste0(z, collapse = ""))
  ooml <- xml_xslt(doc, style_doc)
  ooml <- gsub(xml_header, "", as.character(ooml), fixed = TRUE)
  ooml

}
xml_header <- "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"


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
transform_mathjax <- function(x, to = "html", display = FALSE, strict = "ignore"){

  if(length(x) < 1) return(character(0))
  to <- match.arg(to, choices = c("html", "mml", "svg"), several.ok = FALSE)
  strict <- match.arg(strict, choices = c("ignore", "warn", "error"), several.ok = FALSE)

  if ("mml" %in% to) {
    info <- vapply(x, katex_mathml, "", preview = FALSE, strict = strict)
    names(info) <- NULL
    info <- gsub("</span>", "", info, fixed = TRUE)
    info <- gsub("<span class=\"katex\">", "", info, fixed = TRUE)
    info <- vapply(info, mml_to_ooml, FUN.VALUE = character(1L), USE.NAMES = FALSE)
  } else {
    info <- vapply(x, katex_html, "", preview = FALSE, include_css = TRUE,
      displayMode = display, strict = strict)
    names(info) <- NULL
  }

  info
}

