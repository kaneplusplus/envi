#' Get the Handle of the Current Environment
#'
#' @export
envi_current_handle <- function() {
  if (exists("handle", where = envi_globals, inherits = FALSE)) {
    envi_globals$handle
  } else {
    NULL
  }
}

#' Get the Absolute envi Package Path
#'
#' @export
get_envi_path <- function() {
  if (!exists("path", where = envi_globals, inherits = FALSE)) {
    # Use the default location.
    set_envi_path()
  }
  envi_globals$path
}

#' List the Current Available Environments
#'
#' @param env_info should individual environment information be included? 
#' (Default FALSE)
#' @importFrom crayon yellow
#' @importFrom tibble tibble
#' @export
envi_list <- function(env_info = FALSE) {
  if ( !dir.exists(get_envi_path()) ) {
    warning(yellow("No environments available."), call. = FALSE)
    ret <- tibble(handle = character(), path = character())
  } else {
    if (!file.exists(file.path(get_envi_path(), "environments.rds"))) {
      ret <- tibble(handle = character(), path = character())
    } else {
      ret <- readRDS(file.path(get_envi_path(), "environments.rds"))
    }
  }
  if (env_info) {
    ret$info <- lapply(ret$handle, envi_env_info)
  }
  ret
}

#' Get an Environments Packages and Versions
#' 
#' @param handle the handle of the environment to get the package list for.
#' @importFrom crayon red
#' @importFrom desc description
#' @importFrom tibble tibble
#' @export
envi_env_info <- function(handle) {
  el <- envi_list()
  if ( !any(handle %in% el$handle)) {
    stop(red("Environment handle", handle, "not found."))
  }
  dl <- dir(file.path(el$path[el$handle == handle]), recursive = TRUE,
            pattern = "^DESCRIPTION$")
  dsl <- strsplit(dl, .Platform$file.sep)
  
  r_vers <- vapply(dsl, function(x) x[3], NA_character_)
  ret <- tibble(r_version = r_vers)
  ret$platform <- vapply(dsl, function(x) x[4], NA_character_)
  ret$package <- vapply(dsl, function(x) x[5], NA_character_)
  ret$package_version <- NA_character_
  for (i in seq_len(nrow(ret))) {
    d <- description$new(file = file.path(el$path[el$handle == handle], dl[i]))
    ret$package_version[i] <- d[['get']]('Version')
  }
  ret
}

#' Get the path to an Environment
#'
#' @param handle the handle of the environment. If missing, all paths are 
#' returned. Default is the current, activated environment handle, or NULL
#' if one is not activated.
#' @export
envi_env_path <- function(handle = envi_current_handle()) {
  if (is.null(handle) || is.na(handle)) {
    NULL
  } else {
    l <- envi_list()
    if (!missing(handle)) {
      file.path(get_envi_path(), "environments", 
                basename(l$path[l$handle == handle]))
    } else {
      file.path(get_envi_path(), 
                "environments", 
                basename(l$path[l$handle == handle]))
    }
  }
}

