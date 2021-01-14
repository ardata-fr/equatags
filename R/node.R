.node <- new.env()
.node$dir <- NULL
.node$version <- NULL

is_windows <- function() {
  identical(.Platform$OS.type, "windows")
}

is_osx <- function() {
  Sys.info()["sysname"] == "Darwin"
}

dir_exists <- function(x) {
  length(x) > 0 && utils::file_test('-d', x)
}

find_program <- function(program) {
  if (is_osx()) {
    res <- suppressWarnings({
      # Quote the path (so it can contain spaces, etc.) and escape any quotes
      # and escapes in the path itself
      sanitized_path <- gsub("\\", "\\\\", Sys.getenv("PATH"), fixed = TRUE)
      sanitized_path <- gsub("\"", "\\\"", sanitized_path, fixed = TRUE)
      system(paste0("PATH=\"", sanitized_path, "\" /usr/bin/which ", program),
             intern = TRUE)
    })
    if (length(res) == 0)
      ""
    else
      res
  } else {
    Sys.which(program)
  }
}

#' @importFrom utils file_test
get_node_version <- function(node_dir) {
  path <- file.path(node_dir, "node")
  if (is_windows()) path <- paste0(path, ".exe")
  if (!file_test("-x", path)) return(numeric_version("0"))
  info <- system(paste(shQuote(path), "--version"), intern = TRUE)
  version <- strsplit(info, "\n")[[1]][1]
  version <- gsub("[[:alpha:]]", "", version)
  numeric_version(version)
}

#' Find the \command{node} executable
#'
#' Searches for the \command{node} executable in a few places and use the
#' highest version found, unless a specific version is requested.
#' @param cache Whether to search for \command{node} again if a node
#'   directory containing the \command{node} executable of the expected
#'   version (if provided) has been found previously. Search again if
#'   \code{cache = FALSE}.
#' @param dir A character vector of potential directory paths under which
#'   \command{node} may be found.
#' @param version The version of node to look for (e.g., \code{"14.15.4"}). If
#'   not provided, this function searches for the highest version under the
#'   potential directories.
#' @return A list containing the directory and version of node (if found).
#' @export
#' @examples
#' find_node()
#' find_node(version = '10.13.0')
find_node <- function(cache = TRUE, dir = NULL, version = NULL) {

  if (!cache) set_node_info(NULL)  # clear previously found node path
  if (!is.null(.node$dir) && (is.null(version) || version == .node$version))
    return(as.list(.node))

  # look up node in potential sources unless user has supplied `dir`
  sources <- if (length(dir) == 0) c(
    dirname(find_program("node")),
    "/usr/local/bin",
    "/usr/bin"
  ) else dir
  sources <- path.expand(sources)

  # determine the versions of the sources
  versions <- lapply(sources, function(src) {
    if (dir_exists(src)) get_node_version(src) else numeric_version("0")
  })

  # find the maximum version
  found_src <- NULL
  found_ver <- numeric_version("0")
  for (i in seq_along(sources)) {
    ver <- versions[[i]]
    if ((!is.null(version) && ver == version) || (is.null(version) && ver > found_ver)) {
      found_ver <- ver
      found_src <- sources[[i]]
    }
  }

  set_node_info(found_src, found_ver)
  as.list(.node)
}


set_node_info <- function(dir, version = if (!is.null(dir)) get_node_version(dir)) {
  .node$dir <- dir
  .node$version <- version
}

#' @title Check node availability and version
#' @description `node_available()` determine whether node is currently available on
#' the system (optionally checking for a specific version or greater).
#'
#' `node_version()` determine the specific version of node available.
#' @param version Required version of node
#' @param error Whether to signal an error if node with the required version
#'   is not found
#' @return \code{node_available} returns a logical indicating whether the
#'   required version of node is available. \code{node_version} returns a
#'   \code{\link[base]{numeric_version}} with the version of node found.
#' @examples
#' \dontrun{
#' library(equatags)
#'
#' if (node_available())
#'   cat("node", as.character(node_version()), "is available!\n")
#'
#' if (node_available("10.13.0"))
#'   cat("required version of node is available!\n")
#' }
#' @export
node_available <- function(version = NULL,
                             error = FALSE) {

  # ensure we've scanned for node
  find_node()

  # check availability
  found <- !is.null(.node$dir) && (is.null(version) || .node$version >= version)

  msg <- c(
    "node", if (!is.null(version)) c("version", version, "or higher"),
    "is required and was not found (see the help page ?equatags::node_available)."
  )
  if (error && !found) stop(paste(msg, collapse = " "), call. = FALSE)

  found
}

#' @export
#' @rdname node_available
node_version <- function() {
  find_node()
  .node$version
}

node <- function() {
  find_node()
  prg <- "node"
  if(is_windows()) prg <- "node.exe"
  file.path(.node$dir, prg)
}
npm <- function() {
  find_node()
  prg <- "npm"
  if(is_windows()) prg <- "npm.cmd"
  file.path(.node$dir, prg)
}


