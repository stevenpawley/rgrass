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
