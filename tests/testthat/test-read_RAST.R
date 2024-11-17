library(testthat)
library(terra)
library(sp)
source("helper.R")

# setup
# testdata <- download_nc_basic()

loc <- initGRASS(
  gisDbase = "/tmp/grassdb",
  location = "nc_basic_spm_grass7",
  mapset = "PERMANENT",
  override = TRUE
)

test_that("testing read_RAST using terra", {
  # read a categorical raster map
  v1 <- read_RAST("landuse", cat = TRUE, return_format = "terra")

  expect_s4_class(v1, "SpatRaster")
  expect_false(inMemory(v1))

  # check the values and labels
  lvls <- levels(v1)
  expect_equal(lvls[[1]]$value, 0:7)
  expect_equal(
    lvls[[1]]$label,
    c("undefined", "developed", "agriculture", "herbaceous", "shrubland",
      "forest", "water", "sediment")
  )

  write_RAST(v1, "landuse1", flags = c("o", "overwrite"))
  execGRASS("g.remove", flags = "f", name = "landuse1", type = "raster")
})

test_that("testing read_RAST using sp", {
  nc_basic <- read_RAST("landuse", cat = TRUE, return_format = "SGDF")
  lvls <- levels(nc_basic$landuse)

  expect_equal(
    lvls,
    c("developed", "agriculture", "herbaceous", "shrubland",
      "forest", "water", "sediment")
  )
})
