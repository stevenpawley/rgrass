library(testthat)
source("helper.R")

# setup
# testdata <- download_nc_basic()

testthat::test_that("testing initGRASS", {
  # Initialize a temporary GRASS project using the example data
  loc <- initGRASS(
    gisDbase = "/tmp/grassdb",
    location = "nc_basic_spm_grass7",
    mapset = "PERMANENT",
    override = TRUE
  )

  expect_s3_class(loc, "gmeta")
  expect_equal(loc$LOCATION_NAME, "nc_basic_spm_grass7")
  expect_equal(loc$projection, "99")
})
