
#' Set the envi Package Path
#'
#' @param path the new envi package path.
#' @export
set_envi_path <- function(path) {
  if (missing(path)) {
    # Set the path to the default location.
    path <- file.path(path.expand("~"), ".envi")
  }
  tryCatch({
    if (!dir.exists(path)) {
      dir.create(path)
    }
    assign("path", path, pos = envi_globals, inherits = FALSE)
    invisible(TRUE)
  }, warning = function(w) {
    if (exists("path", where = envi_globals, inherits = FALSE)) {
      remove("path", envir = envi_globals)
    }
    w
  })
}

#' Activate an Environment
#'
#' @param handle the environment handle.
#' @export
envi_activate <- function(handle) {
  if ( !any(handle %in% envi_list()$handle)) {
    stop(red("Environment handle", handle, "not found."))
  }
  check_renv_installed()
  deactivate_if_activated()
  renv::activate(envi_env_path(handle))
  set_current_handle(handle)
  invisible(TRUE)
}

#' Deactivate an Environment
#'
#' @param snapshot should a snapshot be created on exit? (Default TRUE)
#' @param confirm should the user be prompted before taking action? 
#' (Default interactive()).
#' @param force should the lockfile be generated even when preflight validation
#' check have failed? (Default FALSE)
#' @export
envi_deactivate <- function(snapshot = TRUE, confirm = interactive(), 
                            force = TRUE) {
  if (is.null(envi_current_handle())) {
    warning(yellow("No activated environment.`"), call. = FALSE)
    invisible(FALSE)
  } else {
    renv::snapshot(envi_env_path(), confirm = confirm, force = force)
    renv::deactivate(envi_env_path())
    set_current_handle(NULL)
    invisible(TRUE)
  }
}

#' Initialize an R Environment
#'
#' @param handle the name of the new environment.
#' @param full_name the name of the environment directory. (Defalt is the
#' value of the handle argument)
#' @param bare should the project be initialized without attempting to 
#' discover and install R package dependencies? (Default TRUE)
#' @param git_init project include an initialized git repository? 
#' (Default TRUE)
#' @importFrom git2r init
#' @importFrom crayon red yellow
#' @importFrom tibble tibble
#' @export
envi_init <- function(handle, full_name = handle, bare = FALSE, 
                      git_init = TRUE) {
  l <- envi_list()
  if (nrow(l) > 0 && 
      (handle %in% l$handle || 
       full_name %in% vapply(l$handle, basename, NA_character_))) {
    
    stop(red("The handle or full name is already in use."))
  }
  check_renv_installed()
  if (!dir.exists(file.path(get_envi_path(), "environments"))) {
    dir.create(file.path(get_envi_path(), "environments"))
  }
  new_env_path <- file.path(get_envi_path(), "environments", full_name)
  deactivate_if_activated()
  tryCatch({
      cwd <- getwd()
      renv::init(new_env_path, bare = bare, restart = FALSE)
      if (git_init) {
        renv::hydrate("git2r")
        git2r::init(new_env_path)
      }
      setwd(cwd)
    },
    error = function(e) {
      if (cwd != getwd()) {
        setwd(cwd)
      }
      stop(e)
    })
  renv::hydrate(c("utf8", "vctrs"))
  l <- envi_list()
  l <- rbind(l, 
             tibble(handle = handle, 
                    path = file.path(get_envi_path(), "environments", 
                                         full_name)))
  write_config(l, file.path(get_envi_path(), "environments.rds"))
  set_current_handle(handle)
  invisible(TRUE)
}

#' Install a Remote Environment
#'
#' @param path the path of the repository housing the R environment to clone.
#' @param handle the handle for the new environment.
#' @param verbose should extra information be printed? (Default TRUE)
#' @param progress should the progress of the clone be shown? (Default verbose)
#' @importFrom piggyback pb_download
#' @importFrom crayon red
#' @importFrom tibble tibble
#' @export
#' @export
envi_url_install <- function(path, handle = basename(path), 
                            verbose = TRUE, progress = verbose) {
}

#' Install a Piggyback'ed Environment
#'
#' @param file name or vector of names of files to be downloaded.
#' @param repo Repository name in format "owner/repo". Will guess the
#' current repo if not specified.
#' @param handle the handle for the new environment.
#' @param tag tag for the GitHub release to which this data is attached.
#' (Default "latest")
#' @param verbose should extra information be printed? (Default TRUE)
#' @param progress should the progress of the clone be shown? (Default verbose)
#' @param keep should the original downloaded environment be kept? 
#' (Default FALSE)
#' @importFrom piggyback pb_download
#' @importFrom crayon red
#' @importFrom tibble tibble
#' @importFrom piggyback pb_download pb_list
#' @export
envi_pb_install <- function(file, repo, 
                            handle = basename(file), 
                            tag = "latest",
                            verbose = TRUE, progress = verbose,
                            keep = FALSE) {

  if (is.null(file)) {
    stop(red("You must specify a file to download."))
  }
  if (length(file) > 1) {
    stop(red("You may only install a single piggyback'ed environment at a",
             "time."))
  }

  check_if_handle_installed(handle)
  env_path <- make_env_path()
  env_path <- file.path(env_path, handle)
  while (dir.exists(env_path)) {
    env_path <- paste0(env_path, "-pb")
  }

  if (verbose) {
    cat("Downloading piggyback'ed environment.\n")
  }
  
  if (FALSE) {
    pb_download(file = file, dest = tempdir(), repo = repo, tag = tag)
  } else {
    pbi <- pb_list(repo = repo, tag = tag)
    if ( !(file %in% basename(pbi$file_name)) ) {
      stop(red("Could not find file: ", file, 
               "\nAvailable files are:\n\t",
               paste(pbi$file_name, sep = "\n\t")))
    }
    # Temporary fix while pb_download has issues.
    gh_file <- piggyback:::pb_info(repo = repo, tag = tag)$browser_download_url
    gh_file <- gh_file[file == basename(gh_file)]
    download.file(gh_file,
                  file.path(tempdir(), file), 
                  quiet = !progress)
  }
  ret <- envi_install_local_compressed(file.path(tempdir(), file), handle, 
    verbose = verbose, progress = progress)
 
  if (!keep) {
    unlink(file.path(tempdir(), file))
  } 
  invisible(ret)
}

#' Install a Local Compressed R Environment
#'
#' @param path the path of a compressed environment.
#' @param handle the handle for the new environment.
#' @param verbose should extra information be printed? (Default TRUE)
#' @param progress should the progress of the clone be shown? (Default verbose)
#' @importFrom git2r clone
#' @importFrom tibble tibble
#' @importFrom utils download.file untar unzip
#' @export
envi_install_local_compressed <- function(path, handle = basename(path), 
                        verbose = TRUE, progress = verbose) {

  env_dir <- unlist(strsplit(basename(path), "\\."))
  if (env_dir[length(env_dir)] == "zip") {
    decompress <- unzip
    env_dir <- paste(env_dir[1:(length(env_dir)-1)], collapse = ".")
  } else {
    decompress <- untar
    env_dir <- paste(env_dir[1:(length(env_dir)-2)], collapse = ".")
  }
  decompress(path, exdir = file.path(get_envi_path(), "environments"))
  invisible(add_if_r_environment(handle, 
    file.path(get_envi_path(), "environments", env_dir)))
}

#' Clone an R Environment
#'
#' @param path the path of the repository housing the R environment to clone.
#' @param handle the handle for the new environment.
#' @param verbose should extra information be printed? (Default TRUE)
#' @param progress should the progress of the clone be shown? (Default verbose)
#' @importFrom git2r clone
#' @importFrom tibble tibble
#' @export
envi_clone  <- function(path, handle = basename(path), 
                        verbose = TRUE, progress = verbose) {

  check_if_handle_installed(handle)
  env_path <- make_env_path()
  env_path <- file.path(env_path, handle)
  while (dir.exists(env_path)) {
    env_path <- paste0(env_path, "-clone")
  }

  deactivate_if_activated()

  if (verbose) {
    cat("Cloning the repository")
  }
  clone(path, env_path, progress = verbose)

  add_if_r_environment(handle, env_path)
}

#' Uninstall an Environment
#'
#' @param handle the environment handle.
#' @param confirm should the user be prompted before removing the environment?
#' (Default TRUE)
#' @param purge should all files in the environments directory be removed?
#' (Default TRUE)
#' @export
envi_uninstall <- function(handle, confirm = interactive(), purge = TRUE) {
  # Is it a legitimate handle?
  el <- envi_list()
  if ( !any(handle %in% el$handle)) {
    stop(red("Environment handle", handle, "not found."))
  }

  # If the handle is active then deactivate.
  deactivate_if_activated(confirm = confirm, force = TRUE)

  # Unlink the environments directory.
  if (purge) {
    unlink(el$path[el$handle == handle], recursive = TRUE, force = TRUE)
  }

  # Remove the environment from the configuration.
  el <- el[el$handle != handle,]
  write_config(el, file.path(get_envi_path(), "environments.rds"))
  invisible(TRUE)
}

#' Remove envi Configuration and Environments
#' 
#' @param confirm should the user be prompted to make sure they want to purge
#' all envi environments? (Default TRUE)
#' @export
#' @importFrom crayon red
purge_envi <- function(confirm = TRUE) {
  if (confirm) {
    resp <- !yesno(red("This will remove your envi environments and",
                       "cannot be undone. Are you sure you want to do",
                       "this?"))
  } else {
    resp <- TRUE
  }
  if (isTRUE(resp)) {
    # Purge the environment.

    # Deactivate the current environment.
    deactivate_if_activated(confirm = confirm, force = TRUE)

    # Uninstall the environments.
    for (handle in envi_list()$handle) {
      envi_uninstall(handle, confirm = FALSE, purge = TRUE)
    }

    # Remove the global variables
    if (exists("path", where = envi_globals, inherits = FALSE)) {
      remove("path", envir = envi_globals)
    } 
    if (exists("handle", where = envi_globals, inherits = FALSE)) {
      remove("handle", envir = envi_globals)
    }

    # Unlink the envi path directory with prejudice and abandon.
    unlink(get_envi_path(), recursive = TRUE, force = TRUE)
    invisible(TRUE)
  } else {
    invisible(FALSE)
  }
}

#' Reset a Remote Environment
#'
#' @param handle the environment handle. If not specified the current 
#' activated handle is used.
#' @param clean should untrackec files be deleted? (Default TRUE)
#' @param confirm should the user be asked before removing files? (Default TRUE)
#' @param verbose should extra information be printed? (Default TRUE)
#' @importFrom git2r status reset repository commit remote_url commits
#' @export
envi_hard_reset <- function(handle = envi_current_handle(), clean = TRUE, 
                            confirm = TRUE, verbose = TRUE) {
  el <- envi_list()
  if (!handle %in% el$handle) {
    stop(red("Environment handle", handle, "not found."))
  }
  path <- el$path[el$handle == handle]
  if (!is_repo(path)) {
    stop(stop("The", handle, "environment doesn't look like a repository."))
  }
  if (!confirm || 
      !yesno("This will reset the current state of your environment",
             "are you sure you want to proceed?")) {
    untracked <- as.character(unlist(status(path)$untracked))
    del_msg <- paste0("The following files will be deleted:\n\t", 
                      paste(untracked, collapse = "\n\t"))
    if (length(untracked)) {
      if (verbose && !confirm && clean) {
        cat(del_msg, "\n")
        unlink(file.path(path, untracked))
      }
      else if (confirm && clean) {
        if(!yesno(yellow(del_msg))) {
          unlink(untracked)
        }
      } else if (clean) {
        unlink(untracked)
      } else {
        if (verbose) {
          cat("Leaving untracked files.\n")
        }
      }
    }
    deactivate_if_activated()
    if (verbose) {
      cat("Resetting the repository.")
    }
    cmts <- commits(path)
    reset(cmts[[length(cmts)]], reset_type = "hard")
    invisible(TRUE)
  } else {
    if (verbose) {
      cat("Aborting reset.")
      invisible(FALSE)
    }
  }
}
