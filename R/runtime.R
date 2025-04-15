#' Set the home path variable
#'
#' @returns home path
#' @keywords internal
set_home_path_variable <- function() {
  if (.Platform$OS.type == "windows") {
    home <- Sys.getenv("USERPROFILE")
    Sys.setenv(HOME = home)
  } else {
    home <- Sys.getenv("HOME")
  }
  return(home)
}


#' Modifies a system-wide environment variable with a new path
#' 
#' This function modifies a system-wide environment variable by appending a new
#' path to it. It checks if the new path is already present in the environment
#' variable and only appends it if it is not.
#' 
#' This is used to update various environment variables related to GRASS GIS
#' installation, for example, adding the GRASS library path ($GISBASE/lib) path
#' `LD_LIBRARY_PATH` and the GRASS python modules at '$GISBASE/etc/python' to
#' `PYTHONPATH`.
#'
#' @param variable_name name of the environment variable to set/modify
#' @param install_path path to the GRASS installation directory
#' 
#' @returns NULL
#' @keywords internal
set_path_variable <- function(variable_name, install_path) {
  # check if install_path is detected in the environment variable
  env_variable <- Sys.getenv(variable_name)
  path_in_envar <- grepl(install_path, env_variable)

  # concatenate install_path with environment variable path
  if (!path_in_envar) {
    appended_path <- paste(
      install_path,
      env_variable,
      sep = ifelse(nzchar(env_variable), .Platform$path.sep, "")
    )
    do.call(Sys.setenv, as.list(setNames(appended_path, variable_name)))
  }
}


#' Set the path to GRASS add-ons directory
#'
#' @param addon_base path to the GRASS addons directory
#'
#' @returns NULL
#' @keywords internal
set_addons_path <- function(addon_base, gv) {
  # get grass add-ons directory
  if (is.null(addon_base)) {
    if (Sys.info()["sysname"] == "Darwin") {
      addon_base <-
        file.path(Sys.getenv("HOME"), "Library", "GRASS", gv$minor, "Addons")
    } else {
      addon_base <-
        file.path(Sys.getenv("HOME"), paste0(".grass", gv$major), "addons")
    }
  }
  
  addon_res <- file.exists(
    addon_base,
    file.path(addon_base, "bin"),
    file.path(addon_base, "scripts")
  )
  
  if (dir.exists(addon_base)) {
    Sys.setenv("GRASS_ADDON_BASE" = addon_base)
  }
  
  addon_subdirs <- dir.exists(c(
    file.path(addon_base, "bin"), 
    file.path(addon_base, "scripts")
  ))
  
  if (any(addon_subdirs)) {
    set_path_variable("PATH", file.path(addon_base, "bin"))
    set_path_variable("PATH", file.path(addon_base, "scripts"))
  }
}


#' Set GRASS_PYTHON environment variable
#'
#' @returns NULL
#' @keywords internal
set_path_to_python_executable <- function() {
  # set GRASS_PYTHON
  grass_python <- Sys.getenv("GRASS_PYTHON")
  OSGEO4W_ROOT <- Sys.getenv("OSGEO4W_ROOT")
  
  # check if grass_python is already set
  if (nzchar(grass_python)) {
    return()
  }
  
  # set python executable for OSGEO4W installations 
  if (nzchar(OSGEO4W_ROOT)) {
    if (file.exists(paste(OSGEO4W_ROOT, "bin/python3.exe", sep = "/"))) {
      Sys.setenv(
        GRASS_PYTHON = paste(OSGEO4W_ROOT, "bin/python3.exe", sep = "/")
      )
    } else {
      Sys.setenv(
        GRASS_PYTHON = paste(OSGEO4W_ROOT, "bin/python.exe", sep = "/")
      )
    }
    return()
  }
  
  # set GRASS_PYTHON to the python executable in the GRASS installation
  gvers <- system(
    paste0("g.version", get("addEXE", envir = .GRASS_CACHE), " -g "),
    intern = TRUE
  )
  gvers <- strsplit(gvers[1], "=")[[1]][2]
  
  python_vers <- ifelse(gvers > "7.6.1", "python3", "python")
  Sys.setenv(GRASS_PYTHON = paste0(python_vers, get("addEXE", envir = .GRASS_CACHE)))
}
