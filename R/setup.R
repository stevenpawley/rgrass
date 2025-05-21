#' Title
#'
#' @param gisBase 
#' @param home 
#' @param addon_base 
#' @param override 
#' @param gisDbase 
#'
#' @returns
#' @keywords internal
setup_runtime_env_unix <-
  function(gisBase, home, addon_base, override, gisDbase) {
    # set home directory (or use USERPROFILE on windows
    set_home_path_variable(home)
    
    # set GRASS installation directory environment variable
    Sys.setenv(GISBASE = gisBase)
    Sys.setenv(OSGEO4W_ROOT = "")

    # update GRASS_ADDON_BASE with platform specific addons directory location
    gv <- grass_version(gisBase)
    set_addons_path(addon_base, gv)
    
    # update PATH with GRASS installation directories
    set_path_variable("PATH", file.path(Sys.getenv("GISBASE"), "bin"))
    set_path_variable("PATH", file.path(Sys.getenv("GISBASE"), "scripts"))

    # update LD_LIBRARY_PATH with GRASS library directory
    set_path_variable("LD_LIBRARY_PATH", file.path(Sys.getenv("GISBASE"), "lib"))

    # update PYTHONPATH with GRASS python modules directory
    set_path_variable("PYTHONPATH", file.path(Sys.getenv("GISBASE"), "etc", "python"))
  }


#' Title
#'
#' @param gisBase 
#' @param home 
#' @param addon_base 
#' @param use_g.dirseps.exe 
#' @param override 
#' @param gisDbase 
#'
#' @returns
#' @keywords internal
setup_runtime_env_windows <- 
  function(gisBase, home, addon_base, use_g.dirseps.exe, override, 
           gisDbase) {
    # set home directory (or use USERPROFILE on windows
    set_home_path_variable(home)

    # set GRASS installation directory environment variable
    Sys.setenv(GISBASE = gisBase)

    # retrieve the version of GRASS
    gv <- grass_version(gisBase)
    set_home_path_variable()
    
    # set ADDON_BASE environment variable to %APPDATA%\GRASS7\addons if not set
    if (is.null(addon_base)) {
      addon_base <- paste0(Sys.getenv("APPDATA"), "/GRASS", gv$major, "/addons")
    }
    addon_res <- file.exists(addon_base, paste0(addon_base, "/bin"))
    if (any(addon_res))
      Sys.setenv("GRASS_ADDON_BASE" = addon_base)

    # set OSGEO4W_ROOT environment variable if not set
    OSGEO4W_ROOT <- Sys.getenv("OSGEO4W_ROOT")

    if (nchar(OSGEO4W_ROOT) > 0) {
      Sys.setenv(GRASS_PROJSHARE = paste0(OSGEO4W_ROOT, "\\share\\proj"))
    } else {
      unistring <- toupper(gisBase)
      unistring <- gsub("\\\\", "/", unistring)

      if (length(grep("OSGEO4W.*/APPS/GRASS", unistring)) > 0) {
        stop(
          "NOTE: If using OSGeo4W GRASS, start R in the OSGeo4W shell,\n",
          "see help(initGRASS) for further details"
        )
      }

      if (length(grep("QGIS.*/APPS/GRASS", unistring)) > 0) {
        stop(
          paste(
            "NOTE: If using Windows standalone QGIS GRASS, start R in the QGIS",
            "standalone OSGeo4W shell, see help(initGRASS) for further details",
            sep = "\n"
          )
        )
      }
      Sys.setenv(
        GRASS_PROJSHARE = paste0(Sys.getenv("GISBASE"), "\\share\\proj")
      )
    }

    # add various directories to the PATH environment variable
    Wpath <- Sys.getenv("PATH")

    if (length(grep(basename(Sys.getenv("GISBASE")), Wpath)) < 1) {
      Sys.setenv(
        PATH = paste0(Sys.getenv("GISBASE"), "\\lib;", Sys.getenv("PATH"))
      )
      Sys.setenv(
        PATH = paste0(Sys.getenv("GISBASE"), "\\bin;", Sys.getenv("PATH"))
      )
      Sys.setenv(
        PATH = paste0(Sys.getenv("GISBASE"), "\\extrabin;", Sys.getenv("PATH"))
      )

      if (addon_res[2]) {
        Sys.setenv(PATH = paste0(
          Sys.getenv("GRASS_ADDON_BASE"),
          "\\bin;",
          Sys.getenv("PATH")
        ))
      }

      # set PYTHONPATH environment variable
      ePyPATH <- Sys.getenv("PYTHONPATH")
      cond <- length(grep(basename(Sys.getenv("GISBASE")), ePyPATH))

      if ((cond < 1) || nchar(ePyPATH) == 0) {
        GrPyPATH <- file.path(Sys.getenv("GISBASE"), "/etc/python")
        if (nchar(ePyPATH) > 0) {
          Sys.setenv(PYTHONPATH = paste(GrPyPATH, ePyPATH, sep = ";"))
        } else {
          Sys.setenv(PYTHONPATH = GrPyPATH)
        }
      }

      if (nchar(OSGEO4W_ROOT) > 0) {
        Sys.setenv(PYTHONHOME = paste(OSGEO4W_ROOT, "apps/Python37", sep = "/"))
      } else {
        G_B_files <- list.files(Sys.getenv("GISBASE"))
        Python_dir <- G_B_files[grep("Python", G_B_files)]

        if (length(Python_dir) > 0) {
          Sys.setenv(
            PYTHONHOME = paste(Sys.getenv("GISBASE"), Python_dir[1], sep = "/")
          )
        }
      }
    }

    # set GISRC environment variable to the location of the .grassrc file
    Sys.setenv(GISRC = paste0(Sys.getenv("HOME"), "\\.grassrc", gv$major))

    if (file.exists(Sys.getenv("GISRC")) && !override) {
      ask_override(
        paste("A GISRC file", Sys.getenv("GISRC"), "already exists"),
        missing_override = TRUE,
        envir = environment()
      )
    }

    # check if the working directory is writable otherwise use a tempfile
    if (file.access(".", 2) != 0) {
      warning("working directory not writable, using tempfile for GISRC")
      Sys.setenv(GISRC = paste(tempfile(), "junk", sep = "_"))
    }

    # write the GISRC file
    cat("GISDBASE:", getwd(), "\n", file = Sys.getenv("GISRC"))

    cat("LOCATION_NAME: <UNKNOWN>",
        "\n",
        file = Sys.getenv("GISRC"),
        append = TRUE)

    cat("MAPSET: <UNKNOWN>",
        "\n",
        file = Sys.getenv("GISRC"),
        append = TRUE)

    gisrc <- ifelse(
      use_g.dirseps.exe,
      system(
        paste("g.dirseps.exe -g", shQuote(Sys.getenv("GISRC"))), intern = TRUE
      ),
      Sys.getenv("GISRC")
    )

    assign("addEXE", .addexe(), envir = .GRASS_CACHE)
    Sys.setenv(GISRC = gisrc)

    if (!is.null(gisDbase)) {
      if (!file.exists(gisDbase))
        dir.create(gisDbase)
    } else {
      gisDbase <- tempdir()
    }

    gisDbase <- ifelse(
      use_g.dirseps.exe,
      system(paste("g.dirseps.exe -g", shQuote(gisDbase)), intern = TRUE),
      gisDbase
    )

    return(gisDbase)
}


#' Write the GISRC file
#'
#' @param dbase path to the GRASS database
#' @param location name of the location
#' @param mapset name of the mapset
#'
#' @returns NULL
#' @keywords internal
write_gisrc <- function(dbase, location, mapset, override = FALSE) {
  # FIXME Sys.info()["sysname"] == "Darwin"
  gv <- grass_version(Sys.getenv("GISBASE"))
  Sys.setenv(GISRC = file.path(Sys.getenv("HOME"), paste0(".grassrc", gv$major)))
  
  if (file.exists(Sys.getenv("GISRC")) && !override) {
    ask_override(
      paste("A GISRC file", Sys.getenv("GISRC"), "already exists"),
      missing_override = TRUE,
      envir = environment()
    )
  }
  
  cat("GISDBASE:", dbase, "\n", file = Sys.getenv("GISRC"))
  cat("LOCATION_NAME:", location, "\n", file = Sys.getenv("GISRC"), append = TRUE)
  cat("MAPSET:", mapset, "\n", file = Sys.getenv("GISRC"), append = TRUE)
  cat("GRASS_GUI:", "text", "\n", file = Sys.getenv("GISRC"), append = TRUE)
}

write_wind <- function(loc_path, mapset, SG, ignore.stderr) {
  pfile <- paste(loc_path, "PERMANENT", "DEFAULT_WIND", sep = "/")
  mSG <- FALSE
  
  if (!file.exists(pfile)) {
    lonlat <- FALSE
    mSG <- !is.null(SG)
    
    if (mSG) {
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
    cat("north:      ", ifelse(mSG, bb[2, "max"], 1), "\n", sep = "", file = pfile, append = TRUE)
    cat("south:      ", ifelse(mSG, bb[2, "min"], 0), "\n", sep = "", file = pfile, append = TRUE)
    cat("east:       ", ifelse(mSG, bb[1, "max"], 1), "\n", sep = "", file = pfile, append = TRUE)
    cat("west:       ", ifelse(mSG, bb[1, "min"], 0), "\n", sep = "", file = pfile, append = TRUE)
    cat("cols:       ", ifelse(mSG, gt$cells.dim[1], 1), "\n", sep = "", file = pfile, append = TRUE)
    cat("rows:       ", ifelse(mSG, gt$cells.dim[2], 1), "\n", sep = "", file = pfile, append = TRUE)
    cat("e-w resol:  ", ifelse(mSG, gt$cellsize[1], 1), "\n", sep = "", file = pfile, append = TRUE)
    cat("n-s resol:  ", ifelse(mSG, gt$cellsize[2], 1), "\n", sep = "", file = pfile, append = TRUE)
    cat("top:        1\n", sep = "", file = pfile, append = TRUE)
    cat("bottom:     0\n", sep = "", file = pfile, append = TRUE)
    cat("cols3:      ", ifelse(mSG, gt$cells.dim[1], 1), "\n", sep = "", file = pfile, append = TRUE)
    cat("rows3:      ", ifelse(mSG, gt$cells.dim[2], 1), "\n", sep = "", file = pfile, append = TRUE)
    cat("depths:     1\n", sep = "", file = pfile, append = TRUE)
    cat("e-w resol3: ", ifelse(mSG, gt$cellsize[1], 1), "\n", sep = "", file = pfile, append = TRUE)
    cat("n-s resol3: ", ifelse(mSG, gt$cellsize[2], 1), "\n", sep = "", file = pfile, append = TRUE)
    cat("t-b resol:  1\n", sep = "", file = pfile, append = TRUE)
  }
  
  tfile <- paste(loc_path, "PERMANENT", "WIND", sep = "/")
  if (!file.exists(tfile)) {
    file.copy(pfile, tfile, overwrite = TRUE)
  }
  
  tfile <- paste(loc_path, mapset, "WIND", sep = "/")
  if (!file.exists(tfile)) {
    file.copy(pfile, tfile, overwrite = TRUE)
  }
  
  # set grass computational region
  execGRASS(
    "g.region",
    save = "input",
    flags = "overwrite",
    ignore.stderr = ignore.stderr
  )
  
  if (mSG) {
    if (nzchar(wkt_SG)) {
      tf <- tempfile()
      writeLines(wkt_SG, con = tf)
      
      MS <- execGRASS(
        "g.mapset",
        flags = "p",
        intern = TRUE,
        ignore.stderr = ignore.stderr
      )
      
      if (MS != "PERMANENT") {
        execGRASS(
          "g.mapset",
          mapset = "PERMANENT",
          flags = "quiet",
          ignore.stderr = ignore.stderr
        )
      }
      tull <- execGRASS(
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
      
      if (MS != "PERMANENT") {
        execGRASS(
          "g.mapset",
          mapset = mapset,
          flags = "quiet",
          ignore.stderr = ignore.stderr
        )
      }
    }
  }
}


#' Search for GRASS GIS installation
#'
#' @returns a character string with the path to the GRASS GIS installation
#' @keywords internal
search_grass <- function() {
  message(
    "No gisBase set. Trying to detect from the GRASS_INSTALLATION ",
    "environment variable."
  )
  grass_installation <- Sys.getenv("GRASS_INSTALLATION")
  stopifnot(is.character(grass_installation))
  
  if (nchar(grass_installation) > 0) {
    message(
      "Taking gisBase value from GRASS_INSTALLATION: ",
      grass_installation
    )
    gisBase <- grass_installation
  } else {
    message(
      "No GRASS_INSTALLATION environment variable was found.\n",
      "Trying to set gisBase by running command ",
      "`grass --config path` (requires grass in the system PATH)."
    )
    
    tryCatch({
      gisBase <-
        if (.Platform$OS.type == "windows") {
          shell("grass --config path", intern = TRUE)
        } else {
          system("grass --config path", intern = TRUE)
        }
    }, error = function(e) {
      stop(
        "grass seems to be unavailable in the system PATH.\n",
        "Either provide the gisBase argument or set a ",
        "GRASS_INSTALLATION environment variable to provide the ",
        "gisBase path",
        call. = FALSE
      )
    })
    
    message(
      "Taking gisBase value from `grass --config path` output: ",
      gisBase
    )
    stopifnot(length(gisBase) == 1L)
  }
  
  return(gisBase)
}


#' Get GRASS GIS version
#'
#' @param gisBase a character string with the path to the GRASS installation
#'
#' @returns list with the major, minor, and patch numbers of GRASS
#' @keywords internal
grass_version <- function(gisBase) {
  gv <- readLines(file.path(gisBase, "etc/VERSIONNUMBER"))
  
  list(
    major = substring(gv, 1, 1),
    minor = substring(gv, 1, 3),
    patch = substring(gv, 1, 5)
  )
}
