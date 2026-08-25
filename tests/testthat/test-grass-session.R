test_that("create_session_directories creates a requested session layout", {
  gisDbase <- tempfile("grass-database-")
  withr::defer(unlink(gisDbase, recursive = TRUE))

  session <- create_session_directories(
    gisDbase = gisDbase,
    location = "location1",
    mapset = "user1",
    tempdir = tempdir()
  )

  expect_identical(session$gisDbase, gisDbase)
  expect_identical(session$location, "location1")
  expect_identical(session$mapset, "user1")
  expect_identical(session$loc_path, file.path(gisDbase, "location1"))
  expect_true(dir.exists(file.path(session$loc_path, "PERMANENT")))
  expect_true(dir.exists(file.path(session$loc_path, "user1")))
})

test_that("create_session_directories supplies temporary defaults", {
  session_tempdir <- tempfile("rgrass-session-")
  dir.create(session_tempdir)
  withr::defer(unlink(session_tempdir, recursive = TRUE))

  session <- create_session_directories(tempdir = session_tempdir)

  expect_identical(session$gisDbase, session_tempdir)
  expect_true(nzchar(session$location))
  expect_true(nzchar(session$mapset))
  expect_true(dir.exists(file.path(session$loc_path, "PERMANENT")))
  expect_true(dir.exists(file.path(session$loc_path, session$mapset)))
})

test_that("write_wind creates default region files", {
  loc_path <- tempfile("grass-location-")
  mapset <- "user1"
  dir.create(file.path(loc_path, "PERMANENT"), recursive = TRUE)
  dir.create(file.path(loc_path, mapset))
  withr::defer(unlink(loc_path, recursive = TRUE))

  commands <- character()
  local_mocked_bindings(
    execGRASS = function(cmd, ...) {
      commands <<- c(commands, cmd)
      invisible(NULL)
    },
    .package = "rgrass"
  )

  expect_invisible(
    write_wind(
      loc_path = loc_path,
      mapset = mapset,
      SG = NULL,
      ignore.stderr = TRUE
    )
  )

  default_wind <- file.path(loc_path, "PERMANENT", "DEFAULT_WIND")
  permanent_wind <- file.path(loc_path, "PERMANENT", "WIND")
  mapset_wind <- file.path(loc_path, mapset, "WIND")

  expect_true(file.exists(default_wind))
  expect_identical(readLines(permanent_wind), readLines(default_wind))
  expect_identical(readLines(mapset_wind), readLines(default_wind))
  expect_identical(commands, "g.region")

  region <- read.dcf(default_wind)
  expect_identical(unname(region[1, "proj"]), "99")
  expect_identical(unname(region[1, "north"]), "1")
  expect_identical(unname(region[1, "south"]), "0")
  expect_identical(unname(region[1, "cols"]), "1")
  expect_identical(unname(region[1, "rows"]), "1")
})

test_that("write_wind rejects unsupported SG objects", {
  loc_path <- tempfile("grass-location-")
  mapset <- "user1"
  dir.create(file.path(loc_path, "PERMANENT"), recursive = TRUE)
  dir.create(file.path(loc_path, mapset))
  withr::defer(unlink(loc_path, recursive = TRUE))

  expect_error(
    write_wind(
      loc_path = loc_path,
      mapset = mapset,
      SG = list(),
      ignore.stderr = TRUE
    ),
    "SG must be a SpatRaster or SpatialGrid object"
  )
})
