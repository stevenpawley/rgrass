# Interpreted GRASS 7, 8 raster interface functions
# Copyright (c) 2008-22 Roger S. Bivand
#
# GIS_LOCK 110814 RSB, suggested by Brian Oney

#' Initiate GRASS session
#'
#' @description
#'  Run GRASS interface in an R session not started within GRASS. In general,
#' most users will use `initGRASS` in throwaway locations, to use GRASS
#' modules  on R objects without the need to define and populate a location. The
#' function initializes environment variables used by GRASS, the .gisrc used by
#' GRASS for further environment variables, and a temporary location.
#'
#' On Windows, if OSGeo4W GRASS is being used, the R session must be started in
#' the OSGeo4W shell. If not, the non-standard placing of files and of
#' environment variables confuses the function. If `toupper(gisBase)`
#' contains "OSGEO4W64/APPS/GRASS" or "OSGEO4W/APPS/GRASS" (and
#' after converting "\\" to "/"), but the environment variable
#' `OSGEO4W_ROOT` is not defined, `initGRASS()` will exit with an
#' error before confusion leads to further errors. For further details, see
#' <https://github.com/osgeo/rgrass/issues/16> and
#' <https://github.com/osgeo/rgrass/issues/16>.
#'
#' The same restriction applies to use of GRASS with QGIS Windows standalone
#' installations, which may be used with `initGRASS` only if the R session
#' is started from the OSGeo4W shell shipped as part of the standalone installer
#' (see <https://github.com/osgeo/rgrass/issues/87>). The function will
#' exit with an error if R was not started from the QGIS OSGeo4W shell before
#' confusion leads to further errors.
#'
#' The locking functions are used internally, but are exposed for experienced
#' R/GRASS scripters needing to use the GRASS module "g.mapset" through
#' `initGRASS` in an existing GRASS location. In particular,
#' "g.mapset" may leave a `.gislock` file in the current MAPSET, so
#' it may be important to call `unlink_.gislock` to clean up before
#' quitting the R session. `remove_GISRC` may be used to try to remove the
#' file given in the "GISRC" environment variable if created by
#' `initGRASS` with argument `remove_GISRC=` TRUE.
#'
#' @details
#' The function establishes an out-of-GRASS working environment providing GRASS
#' commands with the environment variable support required, and may also provide
#' a temporary location for use until the end of the running R session if the
#' `home` argument is set to `tempdir()`, and the `gisDbase`
#' argument is not given. Running `gmeta` shows where the location is,
#' should it be desired to archive it before leaving R.
#'
#' @note
#' If any package command fails with a UTF-8 error from the XML package, try
#' using `setXMLencoding` to work around the problem that GRASS modules
#' declare --interface-description output as UTF-8 without ensuring that it is.
#'
#' @author Roger S. Bivand, e-mail: <Roger.Bivand@nhh.no>
#' @seealso [gmeta()]
#' @keywords spatial
#'
#' @param gisBase The directory path to GRASS binaries and libraries, containing
#'   bin and lib subdirectories among others; if NULL, set from environment
#'   variable GRASS_INSTALLATION if found, if not found,
#'   `system("grass --config path")` is tried.
#' @param home The directory in which to create the .gisrc file; defaults to
#'   `$HOME` on Unix systems and to `USERPROFILE` on Windows systems;
#'   can usually be set to `tempdir()`.
#' @param SG An optional `SpatRaster` or `SpatialGrid` object to
#'   define the `DEFAULT_WIND` of the temporary location.
#' @param gisDbase if missing, `tempdir()` will be used; GRASS GISDBASE
#'   directory for the working session.
#' @param addon_base if missing, assumed to be "$HOME/.grass7/addons" on
#'   Unix-like platforms, on MS Windows "\%APPDATA\%\GRASS7\addons",
#'   and checked for existence.
#' @param location if missing, `basename(tempfile())` will be used; GRASS
#'   location directory for the working session.
#' @param mapset if missing, `basename(tempfile())` will be used; GRASS
#'   mapset directory for the working session.
#' @param override default FALSE, set to TRUE if accidental trashing of GRASS
#'   .gisrc files and locations is not a problem.
#' @param use_g.dirseps.exe default TRUE; when TRUE appears to work for WinGRASS
#'   Native binaries, when FALSE for QGIS GRASS binaries; ignored on other
#'   platforms.
#' @param pid default `as.integer(round(runif(1, 1, 1000)))`, integer used
#'   to identify GIS_LOCK; the value here is arbitrary, but probably should be
#'   set correctly.
#' @param remove_GISRC default FALSE; if TRUE, attempt to unlink the temporary
#'   file named in the "GISRC" environment variable when the R session
#'   terminates or when this package is unloaded.
#' @param ignore.stderr default taking the value set by
#'   `set.ignore.stderrOption`; can be set to TRUE to silence
#'   `system()` output to standard error; does not apply on Windows
#'   platforms.
#'
#' @return The function runs `gmeta6` before returning the current values
#'   of the running GRASS session that it provides.
#' @export
#' @importFrom stats runif
#' @importFrom methods getMethod
#'
#' @examples
#' # Run only if GRASS installation is found and 'terra' package is installed
#' GRASS_INSTALLATION <- Sys.getenv("GRASS_INSTALLATION")
#' run <- nzchar(GRASS_INSTALLATION) &&
#'        file.exists(GRASS_INSTALLATION) &&
#'        file.info(GRASS_INSTALLATION)$isdir &&
#'        require(terra, quietly = TRUE)
#'
#' if (run) {
#'   # Get the terra example dataset
#'   f <- system.file("ex/elev.tif", package="terra")
#'   r <- rast(f)
#' }
#'
#' # Check for existing GRASS session running
#' if (run) {
#'   loc_existing <- try(gmeta(), silent = TRUE)
#' }
#'
#' if (run) {
#'   # Initialize a temporary GRASS project using the example data
#'   loc <- initGRASS(
#'     gisBase = GRASS_INSTALLATION,
#'     home = tempdir(),
#'     SG = r,
#'     override = TRUE
#'   )
#' }
#'
#' if (run) {
#'   # Write the example data to the GRASS database
#'   write_RAST(r, "elev", flags = "overwrite")
#'   execGRASS("r.info", map = "elev")
#' }
#'
#' if (run) {
#'   # Calculate slope and aspect raster
#'   execGRASS(
#'     "r.slope.aspect",
#'     flags    = "overwrite",
#'     elevation = "elev",
#'     slope    = "slope",
#'     aspect   = "aspect"
#'   )
#' }
#'
#' if (run) {
#'   # Read the results back into R and plot
#'   u1 <- read_RAST(c("elev", "slope", "aspect"), return_format = "terra")
#'   plot(u1[["elev"]], col = terrain.colors(50))
#' }
#'
#' # Restore the original GRASS session
#' if (run) {
#'   if (!inherits(loc_existing, "try-error")) {
#'     loc <- initGRASS(
#'       home = tempdir(),
#'       gisBase = GRASS_INSTALLATION,
#'       gisDbase = loc_existing$GISDBASE,
#'       location = loc_existing$LOCATION_NAME,
#'       mapset = loc_existing$MAPSET,
#'       override = TRUE
#'     )
#'   }
#' }
initGRASS <- function(
    gisBase = NULL,
    home = NULL,
    SG = NULL,
    gisDbase = NULL,
    addon_base = NULL,
    location = NULL,
    mapset = NULL,
    override = FALSE,
    use_g.dirseps.exe = TRUE,
    pid = round(runif(1, 1, 1000)),
    remove_GISRC = FALSE,
    ignore.stderr = get.ignore.stderrOption()) {
  
  # check for existing GRASS session from rc filename specified in GISRC
  if (nzchar(Sys.getenv("GISRC")) && !override) {
    ask_override(
      msg = paste0("A GRASS location (defined by ", Sys.getenv("GISRC"), ") is already in use"),
      missing_override = TRUE,
      envir = environment()
    )
  }

  # check for lockfile specified in the GIS_LOCK env variable
  if (nzchar(get.GIS_LOCK())) {
    if (!override) {
      ask_override(
        msg = "A GIS_LOCK environment variable is present",
        missing_override = TRUE,
        envir = environment()
      )
      unset.GIS_LOCK() # no error means user wants to override
    } else {
      unset.GIS_LOCK()
    }
  }

  pid <- suppressWarnings(as.integer(pid))

  # get grass install from PATH if not provided  
  if (is.null(gisBase)) {
    gisBase <- search_grass()
  }
  
  # check arguments
  stopifnot(
    "`pid` must be an integer" = !is.na(pid),
    "`override` must be a logical" = is.logical(override),
    "`override` must be of length 1" = length(override) == 1,
    "`use_g.dirseps.exe` must be a logical" = is.logical(use_g.dirseps.exe),
    "`use_g.dirseps.exe` must be of length 1" = length(use_g.dirseps.exe) == 1,
    "`remove_GISRC` must be a logical" = is.logical(remove_GISRC),
    "`remove_GISRC` must be of length 1" = length(remove_GISRC) == 1
  )
  
  stopifnot(
    "`gisBase` not found" = file.exists(gisBase),
    "`gisBase` is not a directory" = file.info(gisBase)$isdir[1]
  )

  bin_is_dir <- file.info(file.path(gisBase, "bin"))$isdir[1]
  stopifnot(
    "`gisBase` does not contain bin, the directory with GRASS programs" = !is.na(bin_is_dir),
    "`gisBase`/bin is not a directory" = bin_is_dir
  )

  scripts_is_dir <- file.info(file.path(gisBase, "scripts"))$isdir[1]
  stopifnot(
    "`gisBase` does not contain scripts, the directory with GRASS scripts" = !is.na(scripts_is_dir),
    "`gisBase`/scripts is not a directory" = scripts_is_dir
  )

  # set environment variables
  platform <- get("SYS", envir = .GRASS_CACHE)

  if (platform == "WinNat") {
    setup_runtime_env_windows(
      gisBase, home, addon_base, use_g.dirseps.exe,
      override, gisDbase
    )
  } else if (platform == "unix") {
    setup_runtime_env_unix(
      gisBase, home, addon_base, override,
      gisDbase
    )
  } else {
    stop(paste("Platform variant", platform, "not supported"))
  }
  
  # set gislock envionment variable and set initiated flag to TRUE in package namespace
  set.GIS_LOCK(pid)
  assign("INIT_USED", TRUE, envir = .GRASS_CACHE)
  assign("GIS_LOCK", pid, envir = .GRASS_CACHE)

  if (remove_GISRC) {
    assign("remove_GISRC", remove_GISRC, envir = .GRASS_CACHE)
  }

  # create temporary location if not supplied
  if (!is.null(gisDbase)) {
    if (!file.exists(gisDbase))
      dir.create(gisDbase)
  } else {
    gisDbase <- tempdir()
  }
  
  if (is.null(location)) {
    location <- basename(tempfile())
  }

  loc_path <- file.path(gisDbase, location)
  if (!file.exists(loc_path)) {
    dir.create(loc_path)
  }

  permanent_path <- file.path(loc_path, "PERMANENT")
  if (!file.exists(permanent_path)) {
    dir.create(permanent_path)
  }

  if (is.null(mapset)) {
    mapset <- basename(tempfile())
  }

  mapset_path <- file.path(loc_path, mapset)
  if (!file.exists(mapset_path)) {
    dir.create(mapset_path)
  }

  # write the GISRC file
  write_gisrc(
    dbase = gisDbase,
    location = location,
    mapset = mapset,
    override = override
  )
  Sys.setenv(GISBASE = gisBase)
  Sys.setenv(GISDBASE = gisDbase)
  Sys.setenv(LOCATION_NAME = location)
  Sys.setenv(MAPSET = mapset)

  # scrape grass gis version and check for supported version
  gv <- system(
    paste0("g.version", get("addEXE", envir = .GRASS_CACHE)),
    intern = TRUE
  )
  
  comp <- .compatibleGRASSVersion(gv)
  
  if (!comp) {
    stop(attr(comp, "message"))
  }

  # set GRASS_PYTHON
  set_path_to_python_executable()
  assign("GV", gv, envir = .GRASS_CACHE)
  
  # create WIND file with location projection/resolution/bounds
  write_wind(loc_path, mapset, SG, ignore.stderr)
  
  # return location metadata
  gmeta(ignore.stderr = ignore.stderr)
}

#' @rdname initGRASS
#' @export
get.GIS_LOCK <- function() {
  Sys.getenv("GIS_LOCK")
}

#' @rdname initGRASS
#' @export
set.GIS_LOCK <- function(pid) {
  if (missing(pid)) pid <- round(runif(1, 1, 1000))
  pid <- as.integer(pid)
  stopifnot(!is.na(pid))
  Sys.setenv(GIS_LOCK = pid)
}

#' @rdname initGRASS
#' @export
unset.GIS_LOCK <- function() {
  Sys.unsetenv("GIS_LOCK")
}

#' @rdname initGRASS
#' @export
unlink_.gislock <- function() {
  gl <- paste(Sys.getenv("GISDBASE"), Sys.getenv("LOCATION_NAME"),
    Sys.getenv("MAPSET"), ".gislock",
    sep = "/"
  )
  if (file.exists(gl)) unlink(gl)
}

ask_override <- function(msg, missing_override, envir) {
  if (interactive() && missing_override) {
    message(msg, ".")
    a <- ""
    while (!grepl("^[Yy]([Ee][Ss])?$|^[Nn]([Oo])?$", a)) {
      a <- readline("Do you want to override ('no' will abort)? (y/n) ")
    }
    if (grepl("^[Yy]", a)) {
      assign("override", TRUE, envir = envir)
      message("Overriding. Avoid this question by setting override = TRUE")
    } else {
      stop("Aborting. To override, set override = TRUE", call. = FALSE)
    }
  } else {
    stop(msg, "; to override, set override = TRUE", call. = FALSE)
  }
}

#' @rdname initGRASS
#' @export
remove_GISRC <- function() {
  if (get("INIT_USED", envir = .GRASS_CACHE) &&
    get("remove_GISRC", envir = .GRASS_CACHE)) {
    gisrc <- Sys.getenv("GISRC")
    if (file.exists(gisrc)) unlink(gisrc)
    Sys.unsetenv("GISRC")
  }
}