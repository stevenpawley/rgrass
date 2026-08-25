#' Create GRASS session directories
#'
#' Resolves optional database, location, and mapset paths and creates the
#' directory structure required for a GRASS session.
#'
#' @param gisDbase Optional path to the GRASS database.
#' @param location Optional GRASS location name.
#' @param mapset Optional GRASS mapset name.
#' @param tempdir Directory used for default session paths and names.
#'
#' @returns A list containing `gisDbase`, `location`, `mapset`, and `loc_path`.
#' @keywords internal
create_session_directories <- function(
    gisDbase = NULL,
    location = NULL,
    mapset = NULL,
    tempdir = base::tempdir()) {
  if (is.null(gisDbase)) {
    gisDbase <- tempdir
  }
  if (!dir.exists(gisDbase)) {
    dir.create(gisDbase, recursive = TRUE)
  }

  if (is.null(location)) {
    location <- basename(tempfile(tmpdir = tempdir))
  }
  loc_path <- file.path(gisDbase, location)
  if (!dir.exists(loc_path)) {
    dir.create(loc_path, recursive = TRUE)
  }

  permanent_path <- file.path(loc_path, "PERMANENT")
  if (!dir.exists(permanent_path)) {
    dir.create(permanent_path)
  }

  if (is.null(mapset)) {
    mapset <- basename(tempfile(tmpdir = tempdir))
  }
  mapset_path <- file.path(loc_path, mapset)
  if (!dir.exists(mapset_path)) {
    dir.create(mapset_path)
  }

  list(
    gisDbase = gisDbase,
    location = location,
    mapset = mapset,
    loc_path = loc_path
  )
}

#' Write the GRASS session configuration file
#'
#' Creates the platform-specific GISRC path and writes the values identifying
#' the active GRASS database, location, and mapset.
#'
#' @param gisDbase Path to the GRASS database.
#' @param location Name of the GRASS location.
#' @param mapset Name of the GRASS mapset.
#' @param home Directory in which to create the GISRC file.
#' @param gv GRASS version information returned by [grass_version()].
#' @param platform GRASS platform variant (`"unix"` or `"WinNat"`).
#' @param override Whether an existing GISRC file may be overwritten.
#' @param missing_override Whether `override` was omitted from [initGRASS()].
#' @param use_g.dirseps.exe Whether to convert Windows path separators with
#'   `g.dirseps.exe`.
#' @param tempdir Directory used for a fallback GISRC file when the current
#'   working directory is not writable on Windows.
#'
#' @returns The path to the GISRC file, invisibly.
#' @keywords internal
write_gisrc <- function(
    gisDbase,
    location,
    mapset,
    home,
    gv,
    platform,
    override = FALSE,
    missing_override = FALSE,
    use_g.dirseps.exe = TRUE,
    tempdir = base::tempdir()) {
  gisrc <- if (platform == "WinNat") {
    paste0(home, "\\.grassrc", gv$major)
  } else if (platform == "unix") {
    file.path(home, paste0(".grassrc", gv$major))
  } else {
    stop(paste("Platform variant", platform, "not supported"))
  }

  if (file.exists(gisrc) && !override) {
    ask_override(
      paste("A GISRC file", gisrc, "already exists"),
      missing_override = missing_override,
      envir = parent.frame()
    )
  }

  if (platform == "WinNat" && file.access(".", 2) != 0) {
    warning("working directory not writable, using tempfile for GISRC")
    gisrc <- paste(tempfile(tmpdir = tempdir), "junk", sep = "_")
  }

  writeLines(
    c(
      paste("GISDBASE:", gisDbase),
      paste("LOCATION_NAME:", location),
      paste("MAPSET:", mapset),
      "GRASS_GUI: text"
    ),
    con = gisrc
  )

  if (platform == "WinNat" && use_g.dirseps.exe) {
    gisrc <- system(
      paste("g.dirseps.exe -g", shQuote(gisrc)),
      intern = TRUE
    )
  }

  Sys.setenv(GISRC = gisrc)
  invisible(gisrc)
}

#' Initialize GRASS region files
#'
#' Creates `DEFAULT_WIND` and mapset `WIND` files, initializes the saved input
#' region, and applies projection information supplied through `SG`.
#'
#' @param loc_path Path to the GRASS location.
#' @param mapset Name of the active mapset.
#' @param SG Optional `SpatRaster` or `SpatialGrid` defining the region.
#' @param ignore.stderr Whether GRASS command standard error should be ignored.
#'
#' @returns `NULL`, invisibly.
#' @keywords internal
write_wind <- function(loc_path, mapset, SG = NULL, ignore.stderr) {
  pfile <- file.path(loc_path, "PERMANENT", "DEFAULT_WIND")
  has_SG <- FALSE

  if (!file.exists(pfile)) {
    lonlat <- FALSE
    has_SG <- !is.null(SG)

    if (has_SG) {
      if (inherits(SG, "SpatialGrid")) {
        if (!requireNamespace("sp", quietly = TRUE)) {
          stop("The sp package is required for the SG argument")
        }
        bb <- sp::bbox(SG)
        gt <- sp::gridparameters(SG)
        wkt_SG <- sp::wkt(SG)
        lonlatSG <- !sp::is.projected(SG)
      } else if (inherits(SG, "SpatRaster")) {
        if (!requireNamespace("terra", quietly = TRUE)) {
          stop("The terra package is required for the SG argument")
        }
        bb <- getMethod("ext", "SpatRaster")(SG)
        bb <- as.vector(bb)
        bb <- matrix(bb, 2, 2, byrow = TRUE)
        colnames(bb) <- c("min", "max")
        cs <- getMethod("res", "SpatRaster")(SG)
        co <- bb[, 1] + (cs / 2)
        cd <- c(
          getMethod("ncol", "SpatRaster")(SG),
          getMethod("nrow", "SpatRaster")(SG)
        )
        gt <- data.frame(
          cellcentre.offset = co, cellsize = cs,
          cells.dim = cd
        )
        wkt_SG <- getMethod("crs", "SpatRaster")(SG)
        lonlatSG <- getMethod("is.lonlat", "SpatRaster")(SG)
      } else {
        stop("SG must be a SpatRaster or SpatialGrid object")
      }
      lonlat <- !is.na(lonlatSG) && lonlatSG
    }

    cat("proj:       ", ifelse(lonlat, 3, 99), "\n", file = pfile)
    cat("zone:       0\n", file = pfile, append = TRUE)
    cat("north:      ", ifelse(has_SG, bb[2, "max"], 1), "\n",
        sep = "", file = pfile, append = TRUE
    )
    cat("south:      ", ifelse(has_SG, bb[2, "min"], 0), "\n",
        sep = "", file = pfile, append = TRUE
    )
    cat("east:       ", ifelse(has_SG, bb[1, "max"], 1), "\n",
        sep = "", file = pfile, append = TRUE
    )
    cat("west:       ", ifelse(has_SG, bb[1, "min"], 0), "\n",
        sep = "", file = pfile, append = TRUE
    )
    cat("cols:       ", ifelse(has_SG, gt$cells.dim[1], 1), "\n",
        sep = "", file = pfile, append = TRUE
    )
    cat("rows:       ", ifelse(has_SG, gt$cells.dim[2], 1), "\n",
        sep = "", file = pfile, append = TRUE
    )
    cat("e-w resol:  ", ifelse(has_SG, gt$cellsize[1], 1), "\n",
        sep = "", file = pfile, append = TRUE
    )
    cat("n-s resol:  ", ifelse(has_SG, gt$cellsize[2], 1), "\n",
        sep = "", file = pfile, append = TRUE
    )
    cat("top:        1\n", sep = "", file = pfile, append = TRUE)
    cat("bottom:     0\n", sep = "", file = pfile, append = TRUE)
    cat("cols3:      ", ifelse(has_SG, gt$cells.dim[1], 1), "\n",
        sep = "", file = pfile, append = TRUE
    )
    cat("rows3:      ", ifelse(has_SG, gt$cells.dim[2], 1), "\n",
        sep = "", file = pfile, append = TRUE
    )
    cat("depths:     1\n", sep = "", file = pfile, append = TRUE)
    cat("e-w resol3: ", ifelse(has_SG, gt$cellsize[1], 1), "\n",
        sep = "", file = pfile, append = TRUE
    )
    cat("n-s resol3: ", ifelse(has_SG, gt$cellsize[2], 1), "\n",
        sep = "", file = pfile, append = TRUE
    )
    cat("t-b resol:  1\n", sep = "", file = pfile, append = TRUE)
  }

  permanent_wind <- file.path(loc_path, "PERMANENT", "WIND")
  if (!file.exists(permanent_wind)) {
    file.copy(pfile, permanent_wind, overwrite = TRUE)
  }

  mapset_wind <- file.path(loc_path, mapset, "WIND")
  if (!file.exists(mapset_wind)) {
    file.copy(pfile, mapset_wind, overwrite = TRUE)
  }

  execGRASS(
    "g.region",
    save = "input",
    flags = "overwrite",
    ignore.stderr = ignore.stderr
  )

  if (has_SG && nzchar(wkt_SG)) {
    tf <- tempfile(tmpdir = Sys.getenv("RGRASS_TEMPDIR"))
    writeLines(wkt_SG, con = tf)

    current_mapset <- execGRASS(
      "g.mapset",
      flags = "p",
      intern = TRUE,
      ignore.stderr = ignore.stderr
    )

    if (current_mapset != "PERMANENT") {
      execGRASS(
        "g.mapset",
        mapset = "PERMANENT",
        flags = "quiet",
        ignore.stderr = ignore.stderr
      )
    }

    execGRASS(
      "g.proj",
      flags = "c",
      wkt = tf,
      ignore.stderr = ignore.stderr,
      intern = TRUE
    )

    execGRASS(
      "g.region",
      flags = "s",
      region = paste0("input@", mapset),
      ignore.stderr = ignore.stderr
    )

    execGRASS("g.region", flags = "d", ignore.stderr = ignore.stderr)

    if (current_mapset != "PERMANENT") {
      execGRASS(
        "g.mapset",
        mapset = mapset,
        flags = "quiet",
        ignore.stderr = ignore.stderr
      )
    }
  }

  invisible(NULL)
}
