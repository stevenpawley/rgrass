library(testthat)
library(terra)

test_that("testing basic initGRASS", {
  skip_if_not(!is.null(gisBase), "GRASS GIS not found on PATH")

  # Initialize a temporary GRASS project using the example data
  loc <- initGRASS(
    home = tempdir(),
    gisBase = gisBase,
    gisDbase = testdata$gisDbase,
    location = "nc_basic_spm_grass7",
    mapset = "PERMANENT",
    override = TRUE
  )

  expect_s3_class(loc, "gmeta")
  expect_equal(loc$LOCATION_NAME, "nc_basic_spm_grass7")
  expect_equal(loc$projection, "99")
  expect_equal(crs(loc$proj4, describe = TRUE)$name, "NAD83(HARN) / North Carolina")
})

test_that("testing initialization from SpatRaster", {
  skip_if_not(!is.null(gisBase), "GRASS GIS not found on PATH")

  meuse_grid <- rast(system.file("ex/meuse.tif", package = "terra"))
  loc <- initGRASS(home = tempdir(), gisBase = gisBase, SG = meuse_grid, override = TRUE)
  expect_s3_class(loc, "gmeta")
})

test_that("testing remove_GISRC", {
  skip_if_not(!is.null(gisBase), "GRASS GIS not found on PATH")

  loc <- initGRASS(
    home = tempdir(),
    gisBase = gisBase,
    gisDbase = testdata$gisDbase,
    location = "nc_basic_spm_grass7",
    mapset = "PERMANENT",
    remove_GISRC = TRUE,
    pid = 1000,
    override = TRUE
  )

  gisrc_file <- Sys.getenv("GISRC")
  expect_true(file.exists(gisrc_file))

  remove_GISRC()
  expect_false(file.exists(gisrc_file))
})

test_that("testing set/unset.GIS_LOCK", {
  skip_if_not(!is.null(gisBase), "GRASS GIS not found on PATH")
  skip_if_not(Sys.info()["sysname"] %in% c("Linux", "Darwin"), "test only works on *nix")

  # initiate in PERMANENT - gislock should not be present in a different/unused mapset
  loc <- initGRASS(
    home = tempdir(),
    gisBase = gisBase,
    gisDbase = testdata$gisDbase,
    location = "nc_basic_spm_grass7",
    mapset = "PERMANENT",
    remove_GISRC = TRUE,
    override = TRUE
  )

  expect_false(
    file.exists(file.path(testdata$gisDbase, "nc_basic_spm_grass7", "user1", ".gislock"))
  )

  # test initiating with a specified pid
  loc <- initGRASS(
    home = tempdir(),
    gisBase = gisBase,
    gisDbase = testdata$gisDbase,
    location = "nc_basic_spm_grass7",
    mapset = "PERMANENT",
    remove_GISRC = TRUE,
    pid = 1000,
    override = TRUE
  )
  expect_equal(get.GIS_LOCK(), "1000")

  # switch mapset to user1 which should cause a lockfile to be created
  # (lockfiles are not used in PERMANENT which is assumed to be read-only
  # for other users)
  execGRASS("g.mapset", mapset = "user1")
  expect_true(
    file.exists(file.path(testdata$gisDbase, "nc_basic_spm_grass7", "user1", ".gislock"))
  )

  # changing out of user1 should cause the lockfile to be removed
  execGRASS("g.mapset", mapset = "PERMANENT")
  expect_false(
    file.exists(file.path(testdata$gisDbase, "nc_basic_spm_grass7", "user1", ".gislock"))
  )

  # test removing the lock
  expect_equal(get.GIS_LOCK(), "1000")
  unset.GIS_LOCK()
  expect_equal(get.GIS_LOCK(), "")

  # test initation when GISRC has already been set
  expect_error(
    initGRASS(
      home = tempdir(),
      gisBase = gisBase,
      gisDbase = testdata$gisDbase,
      location = "nc_basic_spm_grass7",
      mapset = "user1",
      override = FALSE
    )
  )

  # test initation after removing GISRC
  remove_GISRC()

  expect_no_error(
    initGRASS(
      home = tempdir(),
      gisBase = gisBase,
      gisDbase = testdata$gisDbase,
      location = "nc_basic_spm_grass7",
      mapset = "user1",
      override = FALSE
    )
  )
  
  unlink_.gislock()
})
