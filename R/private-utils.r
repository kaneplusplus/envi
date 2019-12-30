
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
  tryCatch(
    renv::activate,
    error = function(e) {
      stop(red("The renv package needs to be installed to run this function."))
      e
    })
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

#' @importFrom git2r repository
is_repo <- function(path) {
  tryCatch({
      repository(path)
      TRUE
    },
    error = function(e) {
      FALSE
    })
}

# Shamelessly taken from devtools.
yesno <- function(...) {
    yeses <- c("Yes", "Definitely", "For sure", "Yup", "Yeah",
        "Of course", "Absolutely")
    nos <- c("No way", "Not yet", "I forget", "No", "Nope", "Uhhhh... Maybe?")
    cat(paste0(..., collapse = ""))
    qs <- c(sample(yeses, 1), sample(nos, 2))
    rand <- sample(length(qs))
    menu(qs[rand]) != which(rand == 1)
}
