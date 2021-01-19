#' @export
#' @rdname install_mathjax_npm
mathjax_npm_root <- function(){
  dir <- user_data_dir("equatags", "ardata")
  dir
}

#' @rdname install_mathjax_npm
#' @export
mathjax_npm_available <- function(){
  dir_exists(mathjax_npm_root())
}

#' @export
#' @rdname install_mathjax_npm
uninstall_mathjax_npm <- function(){
  app_dir <- mathjax_npm_root()
  unlink(app_dir, recursive = TRUE, force = TRUE)
  invisible(NULL)
}



#' @import rappdirs processx
#' @export
#' @title Install/Uninstall "mathjax-node"
#' @description The function `install_mathjax_npm()` downloads and installs "mathjax-node" ,
#' (a "JavaScript" display engine for mathematics) in the user data directory.
#'
#' The function `uninstall_mathjax_npm()` removes "mathjax-node" from the user
#' data directory.
#'
#' `mathjax_npm_root()` returns the root directory of "mathjax-node" in the user
#' data directory and `mathjax_npm_available()` checks if "mathjax-node" is
#' in the user data directory.
#' @param force Whether to force to install (override) "mathjax-node".
#' @return `mathjax_npm_root` returns a single character value,
#' `mathjax_npm_available` returns a single logical value,
#' `uninstall_mathjax_npm()` and `install_mathjax_npm()` return
#' a `NULL` value.
#' @examples
#' if(node_available() && !mathjax_npm_available()){
#'   install_mathjax_npm()
#'   uninstall_mathjax_npm()
#' }
install_mathjax_npm <- function(force = FALSE){

  if(!node_available()){
    stop("'node.js' is not available or cannot be found. The 'node.js' software must be available to use the functions of the 'equatags' package.")
  }

  app_dir <- user_data_dir("equatags", "ardata")

  de <- dir_exists(app_dir)
  if(de && !force){
    stop("The directory \"", app_dir, "\" exists. Please either delete it, ",
         "or use install_mathjax_npm(force = TRUE).")
  } else if(de && force){
    unlink(app_dir, recursive = TRUE, force = TRUE)
  }

  dir.create(app_dir, showWarnings = FALSE, recursive = TRUE)

  package.json <- system.file(package = "equatags", "mathjax-node", "package.json")
  file.copy(package.json, app_dir, overwrite = TRUE)

  syscall <- run(npm(), args = "install",
                 wd = app_dir, encoding = "UTF-8",
                 spinner = FALSE, echo_cmd = FALSE)

  svg.js <- system.file(package = "equatags", "mathjax-node", "svg.js")
  mml.js <- system.file(package = "equatags", "mathjax-node", "mml.js")
  file.copy(c(svg.js, mml.js), to = app_dir, overwrite = TRUE)
  invisible(NULL)
}


