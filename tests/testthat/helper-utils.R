# locates example files during both testing and installed use
example_path <- function(filename) {
  # try installed package first -- works during devtools::check()
  path <- system.file("extdata", filename, package = "honupsy")
  if (nzchar(path)) {
    return(path)
  }

  # fall back to source tree -- works during devtools::test()
  # (working directory is tests/testthat/, so the package root is two up)
  path <- file.path(
    dirname(dirname(getwd())),
    "inst",
    "extdata",
    filename
  )
  if (file.exists(path)) {
    return(path)
  }

  # skip the test rather than error -- happens during check() if
  # inst/extdata files are missing from the built package
  testthat::skip(paste("Example file not found:", filename))
}
