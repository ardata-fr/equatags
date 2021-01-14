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
#' @title "MathJax" equation as "SVG" or "MathML".
#' @description Get "SVG" or "MathML" XML codes corresponding to the rendering
#' of "MathJax" equations.
#'
#' This function can only be used after executing the command
#' [install_mathjax_npm()] which installs a set of "npm" packages
#' on your machine.
#' @param x MathJax equations
#' @param to output format, one of 'svg' or 'mml'
#' @return a character vector that contains 'svg' or 'mml'
#' codes corresponding to the equations.
#' @examples
#' if(node_available() && mathjax_npm_available()){
#'   x <- c("(ax^2 + bx + c = 0)",
#'          "x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.")
#'   z <- transform_mathjax(x = x, to = "svg")
#'   cat(z, sep = "\n\n")
#'   z <- transform_mathjax(x = x, to = "mml")
#'   cat(z, sep = "\n\n")
#' }
#' @importFrom xml2 read_xml xml_children
transform_mathjax <- function(x, to = "svg"){

  if(length(x) < 1) return(character(0))
  to <- match.arg(to, choices = c("svg", "mml"), several.ok = FALSE)

  equatags_dir <- user_data_dir("equatags", "ardata")

  if(!dir_exists(equatags_dir)){
    stop("'mathjax-node' is not in your user data directory,",
            " run `install_mathjax_npm()` to install it")
  }
  if(!node_available()){
    stop("'node-js' is not available or cannot be found.")
  }

  if("svg" %in% to) {
    args <- c("svg.js", x)
  } else {
    args <- c("mml.js", x)
  }

  syscall <- run(node(), args = args,
                 wd = equatags_dir, encoding = "UTF-8",
                 spinner = FALSE, echo_cmd = FALSE)

  content <- paste0(
    c(xml_header, "<set>", syscall$stdout, "</set>"),
    collapse = "")
  content <- read_xml(content)

  out <- as.character(xml_children(content))

  if("mml" %in% to){
    out <- vapply(out, mml_to_ooml, FUN.VALUE = character(1L), USE.NAMES = FALSE)
  }

  out
}

