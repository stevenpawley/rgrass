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

test_that("set_addons_path configures a custom add-on directory", {
  addon_base <- tempfile("grass-addons-")
  dir.create(addon_base)
  dir.create(file.path(addon_base, "bin"))
  dir.create(file.path(addon_base, "scripts"))
  withr::defer(unlink(addon_base, recursive = TRUE))
  withr::local_envvar(
    GRASS_ADDON_BASE = NA,
    PATH = "/usr/bin"
  )

  expect_identical(
    set_addons_path(addon_base, gv = list(major = "8", major_minor = "8.4")),
    invisible(addon_base)
  )
  expect_identical(Sys.getenv("GRASS_ADDON_BASE"), addon_base)

  expected_path <- paste(
    c(file.path(addon_base, "scripts"), file.path(addon_base, "bin"), "/usr/bin"),
    collapse = .Platform$path.sep
  )
  expect_identical(Sys.getenv("PATH"), expected_path)
})

test_that("set_addons_path adds only an existing bin directory", {
  addon_base <- tempfile("grass-addons-")
  dir.create(file.path(addon_base, "bin"), recursive = TRUE)
  withr::defer(unlink(addon_base, recursive = TRUE))
  withr::local_envvar(
    GRASS_ADDON_BASE = NA,
    PATH = "/usr/bin"
  )

  set_addons_path(addon_base, gv = list(major = "8", major_minor = "8.4"))

  expected_path <- paste(
    c(file.path(addon_base, "bin"), "/usr/bin"),
    collapse = .Platform$path.sep
  )
  expect_identical(Sys.getenv("PATH"), expected_path)
  expect_false(file.path(addon_base, "scripts") %in% strsplit(
    Sys.getenv("PATH"), .Platform$path.sep, fixed = TRUE
  )[[1]])
})

test_that("set_addons_path adds only an existing scripts directory", {
  addon_base <- tempfile("grass-addons-")
  dir.create(file.path(addon_base, "scripts"), recursive = TRUE)
  withr::defer(unlink(addon_base, recursive = TRUE))
  withr::local_envvar(
    GRASS_ADDON_BASE = NA,
    PATH = "/usr/bin"
  )

  set_addons_path(addon_base, gv = list(major = "8", major_minor = "8.4"))

  expected_path <- paste(
    c(file.path(addon_base, "scripts"), "/usr/bin"),
    collapse = .Platform$path.sep
  )
  expect_identical(Sys.getenv("PATH"), expected_path)
  expect_false(file.path(addon_base, "bin") %in% strsplit(
    Sys.getenv("PATH"), .Platform$path.sep, fixed = TRUE
  )[[1]])
})

test_that("set_addons_path ignores a missing add-on directory", {
  addon_base <- tempfile("missing-grass-addons-")
  withr::local_envvar(
    GRASS_ADDON_BASE = NA,
    PATH = "/usr/bin"
  )

  expect_null(
    set_addons_path(addon_base, gv = list(major = "8", major_minor = "8.4"))
  )
  expect_identical(Sys.getenv("GRASS_ADDON_BASE"), "")
  expect_identical(Sys.getenv("PATH"), "/usr/bin")
})

test_that("set_addons_path does not duplicate existing PATH entries", {
  addon_base <- tempfile("grass-addons-")
  bin_path <- file.path(addon_base, "bin")
  scripts_path <- file.path(addon_base, "scripts")
  dir.create(bin_path, recursive = TRUE)
  dir.create(scripts_path)
  withr::defer(unlink(addon_base, recursive = TRUE))

  existing_path <- paste(
    c(scripts_path, bin_path, "/usr/bin"),
    collapse = .Platform$path.sep
  )
  withr::local_envvar(
    GRASS_ADDON_BASE = NA,
    PATH = existing_path
  )

  set_addons_path(addon_base, gv = list(major = "8", major_minor = "8.4"))

  expect_identical(Sys.getenv("PATH"), existing_path)
})
