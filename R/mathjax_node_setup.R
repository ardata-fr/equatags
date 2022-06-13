#' @importFrom tools R_user_dir
mathjax_npm_root <- function(){
  dir <- R_user_dir(package = "equatags", which = "data")
  dir
}

#' @title Is 'mathjax-node' available
#' @description Checks if 'mathjax-node' is available.
#'
#' 'mathjax-node' should be removed with
#' command [mathjax_uninstall()] because it is not used
#' anymore thanks to package 'katex'.
#' @return a single logical value.
#' @export
#' @examples
#' mathjax_available()
#' @family tools for 'mathjax-node'
mathjax_available <- function(){
  dir.exists(mathjax_npm_root())
}

#' @export
#' @title Uninstall 'mathjax-node'
#' @description Removes 'mathjax-node'.
#'
#' 'mathjax-node' should be removed with
#' command [mathjax_uninstall()] because it is not used
#' anymore thanks to package 'katex'.
#' @return a single logical value, FALSE if the operation
#' failed, TRUE otherwise.
#' @family tools for 'mathjax-node'
#' @examples
#' if(mathjax_available()) {
#'   mathjax_uninstall()
#' }
#' @family tools for 'mathjax-node'
mathjax_uninstall <- function(){
  app_dir <- mathjax_npm_root()
  unlink(app_dir, recursive = TRUE, force = TRUE) %in% 0
}


.onAttach <- function(libname, pkgname) {
  if(mathjax_available()) {
    warning("'mathjax' distribution installed by a previous version of 'equatags'",
            " is on your machine. It should be removed.",
            " You can remove it by running the following command:\nmathjax_uninstall()",
            call. = FALSE)
  }
  invisible()
}

