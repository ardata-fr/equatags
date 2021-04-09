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
#' @title 'MathJax' equation as 'SVG' or 'MathML'.
#' @description Get 'SVG' or 'MathML' XML codes corresponding to the rendering
#' of 'MathJax' equations.
#'
#' This function can only be used if the command
#' [mathjax_install()] has been executed once (it installs
#' a set of "npm" packages on your machine).
#' @param x MathJax equations
#' @param to output format, one of 'svg' or 'mml'
#' @return a character vector that contains 'svg' or 'mml'
#' codes corresponding to the equations.
#' @examples
#' library(locatexec)
#' if(exec_available("node") && mathjax_available()){
#'   x <- c("(ax^2 + bx + c = 0)",
#'          "x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.")
#'   z <- transform_mathjax(x = x, to = "svg")
#'   cat(z, sep = "\n\n")
#'   z <- transform_mathjax(x = x, to = "mml")
#'   cat(z, sep = "\n\n")
#' }
#' @importFrom xml2 read_xml xml_children
#' @importFrom locatexec node_exec
transform_mathjax <- function(x, to = "svg"){

  if(length(x) < 1) return(character(0))
  to <- match.arg(to, choices = c("svg", "mml"), several.ok = FALSE)

  equatags_dir <- mathjax_npm_root()
  if(!mathjax_available()){
    stop("'mathjax-node' is not in your user data directory,",
         " run `mathjax_install()` to install it")
  }

  if("svg" %in% to) {
    args <- c("svg.js", shQuote(x))
  } else {
    args <- c("mml.js", shQuote(x))
  }

  curr_wd <- getwd()
  # ensure with an *immediate* call of on.exit() that the
  # settings are reset when the function is exited
  on.exit(setwd(curr_wd))

  setwd(equatags_dir)
  tryCatch({
    info <- system2(node_exec(), args = args, stderr = TRUE, stdout = TRUE)
    },
    warning = function(e) {
      success <- FALSE
    },
    error = function(e) {
      success <- FALSE
    },
    finally = {
      setwd(curr_wd) # redondant with on.exit(setwd(curr_wd)) but kept for healthy paranoia reason
    })

  if(length(info) < 1) {
    success <- FALSE
    return(success)
  } else if(grepl("ENOENT", info[1])) {
    success <- FALSE
    return(success)
  }
  info <- paste0(info, collapse = "\n")
  if("svg" %in% to){
    pos_to <- gregexpr("</svg>", info)[[1]]
    pos_to <- pos_to + attr(pos_to,"match.length")
    pos_from <- gregexpr("<svg", info)[[1]]
  } else {
    pos_to <- gregexpr("</math>", info)[[1]]
    pos_to <- pos_to + attr(pos_to,"match.length")
    pos_from <- gregexpr("<math", info)[[1]]
  }

  out <- list()
  for(i in seq_along(pos_from) ){
    out[[i]] <- substr(info, start = pos_from[i], stop = pos_to[i])
  }
  out <- as.character(unlist(out))

  if("mml" %in% to){
    out <- vapply(out, mml_to_ooml, FUN.VALUE = character(1L), USE.NAMES = FALSE)
  }

  out
}

