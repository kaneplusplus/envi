
envi_globals <- new.env(parent=emptyenv())

# Write the configuration.
write_config <- function(l, config_path) {
  saveRDS(l, config_path)
}

# Does a directory look like an R environment?
looks_like_r_environment <- function(env_path) {
  dirs <- dir(env_path)
  all(c("renv", "renv.lock") %in% dirs)
}

# Set the current handle
set_current_handle <- function(handle) {
  assign("handle", handle, pos = envi_globals, inherits = FALSE)
  invisible(TRUE)
}

#' @importFrom crayon red
check_renv_installed <- function() {
  if ( !("renv" %in% installed.packages()[,"Package"]) ) {
    stop(red("The renv package needs to be installed to run this function."))
  }
  invisible(TRUE)
}

#' @importFrom crayon yellow
deactivate_if_activated <- function(confirm = interactive(), force = FALSE) {
  handle <- envi_current_handle()
  if (!is.null(handle)) {
    warning(yellow("Deactivating current environment `", handle,
                   "`.", sep = ""),
            call. = FALSE)
  
    envi_deactivate(handle, confirm, force)
  }
}
