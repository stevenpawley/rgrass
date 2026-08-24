#' Get the home path environment variable
#'
#' @param home path to the home directory
#' @returns invisible home path
#' @keywords internal
set_home_path <- function(home = NULL) {
  if (is.null(home)) {
    home <- if (.Platform$OS.type == "windows") {
      Sys.getenv("USERPROFILE")
    } else {
      Sys.getenv("HOME")
    }
  }

  Sys.setenv(HOME = home)
  invisible(home)
}
