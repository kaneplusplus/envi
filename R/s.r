
envi_path <- function() {
  file.path("~", ".envi")
}

write_config <- function(cfg_tibble) {
}

#' List the Current Available Environments
#'
#' @importFrom jsonlite fromJSON
#' @importFrom crayon yellow
#' @export
evni_list <- function() {
  if ( !dir.exists(envi_path()) ) {
    warning(yellow("No environments available."))
    tibble(handle = character(), url = character())
  } else {
    ret <- fromJSON(file.path(envi_path(), "environments.json"))
    stop("finish this")
  }
}

#' Get the R of an Environment
#'
#' @export
evni_r_version <- function(handle) {
}

#' Get an Environment
#'
#' @export
evni_get <- function(handle) {
}

#' Set a Default Evnironment
#'
#' @param handle the environment handle.
#' @export
envi_default <- function(handel) {
}

#' Activate an Environment
#'
#' @param handle the environment handle.
#' @export
envi_activate <- function(handle) {
}

#' Update Environments
#'
#' @param handle the environment handle.
#' @export
envi_update <- function(handle) {
}

#' Get the Active Environment
#'
#' @param handle the environment handle.
#' @export
envi_active <- function(handle) {
}

#' Add an R Environment
#'
#' @param url of the repository housing the R environment.
#' @param handle the handle for the new environment.
#' @param verbose should extra information be printed? (Default TRUE)
#' @export
envi_clone <- function(url, handle = url, verbose = TRUE) {
  if (!dir.exists(envi_path()) && verbse) {
    cat("Creating ~/.evni directory.\n")
    dir.create(envi_path())
  }
  l <- envi_list()
  if (url %in% l$url) {
    stop("The url has previously been added.")
  }
  if (handle %in% l$handle) {
    stop("The handle has previously been added.")
  }
  if (verbose) {
    cat("Cloning the repository")
  } 
  clone(url)
  l <- rbind(l, tibble(handle = handle, url = url))
  write_config(l, file.path(envi_path, "enironments.json"))
  invisible(l)
}

#' Remove an Environment
#'
#' @param handle the environment handle.
#' @export
envi_remove <- function(handle) {
  
}

#' Log an Environment's Packages
#'
#' @param handle the enironment handle.
#' @export
envi_log <- function(x) {
}

#' Save the Package Log
#'
#' @param pkg_log the package log.
#' @param location the location where the log should be saved. 
#' (Default ~/.envi/package-log.json)
#' @export
save_log <- function(pkg_log, 
  location = file.path(envi_path(), "package-log.json")) {
}

#' Reset, Clean, and Pull a Repository
#'
#' @param
envi_hard_reset <- function(handle, clean = FALSE) {
  # WARNING: do note this deletes untracked files
  #status() %>%
  #  purrr::pluck("untracked") %>%
  #  rlang::flatten_chr() %>%
  #  unlink()
}
