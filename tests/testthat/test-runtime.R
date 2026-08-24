test_that("set_path_variable sets an empty environment variable", {
  # Use a disposable variable so the test does not modify the real PATH.
  variable <- "RGRASS_TEST_PATH"
  # An NA value tells withr to temporarily unset the variable.
  withr::local_envvar(RGRASS_TEST_PATH = NA)

  # The first path becomes the entire value of an empty variable.
  expect_identical(
    set_path_variable(variable, "/opt/grass/bin"),
    invisible("/opt/grass/bin")
  )
  expect_identical(Sys.getenv(variable), "/opt/grass/bin")
})

test_that("set_path_variable prepends to an existing path list", {
  variable <- "RGRASS_TEST_PATH"
  # Use the platform separator (`:` on Unix and `;` on Windows) so this test
  # exercises the same path-list format used by the host operating system.
  existing <- paste(c("/usr/local/bin", "/usr/bin"), collapse = .Platform$path.sep)
  expected <- paste(c("/opt/grass/bin", existing), collapse = .Platform$path.sep)
  withr::local_envvar(RGRASS_TEST_PATH = existing)

  # GRASS paths take precedence by being inserted before existing paths.
  expect_identical(
    set_path_variable(variable, "/opt/grass/bin"),
    invisible(expected)
  )
  expect_identical(Sys.getenv(variable), expected)
})

test_that("set_path_variable does not duplicate an existing path", {
  variable <- "RGRASS_TEST_PATH"
  existing <- paste(c("/opt/grass/bin", "/usr/bin"), collapse = .Platform$path.sep)
  withr::local_envvar(RGRASS_TEST_PATH = existing)

  # Repeated initialization must not make path variables grow indefinitely.
  expect_identical(
    set_path_variable(variable, "/opt/grass/bin"),
    invisible(existing)
  )
  expect_identical(Sys.getenv(variable), existing)
})

test_that("set_path_variable compares complete path entries", {
  variable <- "RGRASS_TEST_PATH"
  existing <- "/opt/grass-old/bin"
  expected <- paste(c("/opt/grass/bin", existing), collapse = .Platform$path.sep)
  withr::local_envvar(RGRASS_TEST_PATH = existing)

  # This guards against the previous substring/regular-expression approach:
  # similarly named directories are distinct entries and both must be retained.
  expect_identical(
    set_path_variable(variable, "/opt/grass/bin"),
    invisible(expected)
  )
  expect_identical(Sys.getenv(variable), expected)
})
