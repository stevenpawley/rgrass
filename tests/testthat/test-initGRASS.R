library(testthat)
source("helper.R")

# setup
# testdata <- download_nc_basic()
gisBase <- try(system2("grass", "--config path", stdout = TRUE))
grass_available <- !inherits(gisBase, "try-catch")

testthat::test_that("testing initGRASS", {
  skip_if_not(!isTRUE(grass_available), "GRASS GIS not found on PATH")
  
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
