#' Get the home path environment variable
#'
#' @param home path to the home directory
#' @returns invisible home path
#' @keywords internal
set_home_path <- function(home = NULL) {
  if (is.null(home)) {
    home <- if (.Platform$OS.type == "windows") {
      Sys.getenv("USERPROFILE")
    } else {
      Sys.getenv("HOME")
    }
  }

  Sys.setenv(HOME = home)
  invisible(home)
}

#' Prepend a path to an environment variable
#'
#' Adds a path to a path-list environment variable unless that exact path is
#' already present.
#'
#' @param variable_name Name of the environment variable.
#' @param install_path Path to prepend.
#'
#' @returns The updated environment-variable value, invisibly.
#' @keywords internal
set_path_variable <- function(variable_name, install_path) {
  env_variable <- Sys.getenv(variable_name)

  existing_paths <- if (nzchar(env_variable)) {
    strsplit(env_variable, .Platform$path.sep, fixed = TRUE)[[1]]
  } else {
    character()
  }

  if (!install_path %in% existing_paths) {
    updated_paths <- paste(
      c(install_path, existing_paths),
      collapse = .Platform$path.sep
    )

    do.call(
      Sys.setenv,
      setNames(list(updated_paths), variable_name)
    )
  }

  invisible(Sys.getenv(variable_name))
}
