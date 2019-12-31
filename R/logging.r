
#' Log an Environment's Packages
#'
#' @param x a envi log object.
#' @param handle the environment handle.
#' @export
envi_log <- function(x, handle = envi_current_handle()) {
}

#' Save the Package Log
#'
#' @param pkg_log the package log.
#' @param location the location where the log should be saved. 
#' (Default ~/.envi/package-log.rds)
#' @export
envi_save_log <- function(pkg_log, 
  location = file.path(get_envi_path(), "package-log.rds")) {
}

#' Checkpoint the Current Package Configuration
#'
#' @param handle the handle to the envronment you'd like to checkpoint.
#' @export
envi_checkpoint <- function(handle = envi_current_handle()) {
}

