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

#' Set the path to GRASS add-ons directory
#'
#' @param addon_base path to the GRASS addons directory
#'
#' @returns The configured add-on path invisibly, or `NULL` if the directory
#'   does not exist.
#' @keywords internal
set_addons_path <- function(addon_base = NULL, gv) {
  if (is.null(addon_base)) {
    addon_base <- if (.Platform$OS.type == "windows") {
      file.path(Sys.getenv("APPDATA"), paste0("GRASS", gv$major), "addons")
    } else if (Sys.info()[["sysname"]] == "Darwin") {
      file.path(Sys.getenv("HOME"), "Library", "GRASS", gv$major_minor, "Addons")
    } else {
      file.path(Sys.getenv("HOME"), paste0(".grass", gv$major), "addons")
    }
  }

  if (!dir.exists(addon_base)) {
    return(invisible(NULL))
  }

  Sys.setenv(GRASS_ADDON_BASE = addon_base)

  bin_path <- file.path(addon_base, "bin")
  scripts_path <- file.path(addon_base, "scripts")

  if (dir.exists(scripts_path)) {
    set_path_variable("PATH", scripts_path)
  }

  if (dir.exists(bin_path)) {
    set_path_variable("PATH", bin_path)
  }

  invisible(addon_base)
}

#' Set Unix runtime environment variables for GRASS GIS
#'
#' @param gisBase Path to the GRASS installation.
#' @param addon_base Optional path to the GRASS add-ons directory.
#' @param gv Cumulative GRASS version strings returned by [grass_version()].
#'
#' @returns `NULL`, invisibly.
#' @keywords internal
setup_runtime_env_unix <- function(gisBase, addon_base = NULL, gv) {
  Sys.setenv(GISBASE = gisBase)

  # Configure add-ons first because subsequent calls prepend the core GRASS
  # directories, giving the installed modules precedence over add-ons.
  set_addons_path(addon_base, gv)

  # Calls are intentionally in reverse order because each path is prepended.
  set_path_variable("PATH", file.path(gisBase, "scripts"))
  set_path_variable("PATH", file.path(gisBase, "bin"))
  set_path_variable("LD_LIBRARY_PATH", file.path(gisBase, "lib"))
  set_path_variable("PYTHONPATH", file.path(gisBase, "etc", "python"))

  invisible(NULL)
}
