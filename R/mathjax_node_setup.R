#' @importFrom rappdirs user_data_dir
mathjax_npm_root <- function(){
  dir <- user_data_dir("equatags", "ardata")
  dir
}

#' @title Is 'mathjax-node' available
#' @description Checks if 'mathjax-node' is available.
#'
#' 'mathjax-node' can be installed with command
#' [mathjax_install()] and can be removed with
#' command [mathjax_uninstall()].
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
#' @return a single logical value, FALSE if the operation
#' failed, TRUE otherwise.
#' @family tools for 'mathjax-node'
#' @examples
#' library(locatexec)
#'
#' if(exec_available("npm") &&
#'    mathjax_available()) {
#'   mathjax_uninstall()
#'   mathjax_install()
#' }
#' @family tools for 'mathjax-node'
mathjax_uninstall <- function(){
  app_dir <- mathjax_npm_root()
  unlink(app_dir, recursive = TRUE, force = TRUE)
  invisible(NULL)
}



#' @importFrom locatexec npm_exec exec_available
#' @export
#' @title Install 'mathjax-node'
#' @description Downloads and installs 'mathjax-node'
#' (APIs to call MathJax from node.js programs) in the user-specific
#' data directory managed with [user_data_dir()].
#'
#' Please note that the total size of the downloaded files is about 70 MB.
#'
#' This data directory can be removed from the computer
#' with command [mathjax_uninstall()].
#' @param force Whether to force to install (override) 'mathjax-node'.
#' @param verbose should a log be printed in the console, default to TRUE.
#' @return a single logical value, FALSE if the operation
#' failed, TRUE otherwise.
#' @family tools for 'mathjax-node'
#' @examples
#' library(locatexec)
#' if(exec_available("node") && !mathjax_available()){
#'   mathjax_install()
#'   mathjax_uninstall()
#' }
mathjax_install <- function(force = FALSE, verbose = TRUE){

  exec_available("npm", error = TRUE)

  app_dir <- mathjax_npm_root()
  de <- dir.exists(app_dir)
  if(de && !force){
    stop("The directory \"", app_dir, "\" exists. Please either delete it, ",
         "or use mathjax_install(force = TRUE).")
  } else if(de && force){
    unlink(app_dir, recursive = TRUE, force = TRUE)
  }

  dir.create(app_dir, showWarnings = FALSE, recursive = TRUE)

  package.json <- system.file(package = "equatags", "mathjax-node", "package.json")
  file.copy(package.json, app_dir, overwrite = TRUE)

  info <- NULL
  success <- TRUE

  curr_wd <- getwd()
  # ensure with an *immediate* call of on.exit() that the
  # settings are reset when the function is exited
  on.exit(setwd(curr_wd))

  setwd(app_dir)
  tryCatch({
    info <-
      system2(
        npm_exec(),
        args = "install",
        stderr = TRUE, stdout = TRUE)
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
    if(verbose) message("unknown error on operation")
    unlink(app_dir, recursive = TRUE, force = TRUE)
    return(success)
  } else if(grepl("ENOENT", info[1])) {
    success <- FALSE
    if(verbose) message(paste0(info, collapse = "\n"))
    return(success)
  }
  if(verbose && success) message(paste0(info, collapse = "\n"))

  svg.js <- system.file(package = "equatags", "mathjax-node", "svg.js")
  mml.js <- system.file(package = "equatags", "mathjax-node", "mml.js")
  file.copy(c(svg.js, mml.js), to = app_dir, overwrite = TRUE)
  invisible(TRUE)
}


