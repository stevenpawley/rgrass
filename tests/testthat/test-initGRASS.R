library(testthat)
library(terra)

testthat::test_that("testing initGRASS", {
  # download nc basic dataset
  if (!file.exists("/tmp/nc_basic_spm_grass7.zip")) {
    download.file(
      "https://grass.osgeo.org/sampledata/north_carolina/nc_basic_spm_grass7.zip",
      "/tmp/nc_basic_spm_grass7.zip"
    )
    unzip("/tmp/nc_basic_spm_grass7.zip", exdir = "/tmp/grassdb/nc_basic_spm_grass7")
  }

  # Initialize a temporary GRASS project using the example data
  loc <- initGRASS(
    home = tempdir(),
    gisDbase = "/tmp/grassdb",
    location = "nc_basic_spm_grass7",
    mapset = "PERMANENT",
    override = TRUE
  )

  expect_s3_class(loc, "gmeta")
  expect_equal(loc$LOCATION_NAME, "nc_basic_spm_grass7")
  expect_equal(loc$projection, "99")
})
