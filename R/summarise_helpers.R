# Internal helpers shared across summarise_* functions

#' Extract and validate $mb from an import_anq() result
#' Stops with a clear message if the expected structure is missing or if
#' assign_units() has not been called.
#' @noRd
.extract_mb <- function(data) {
  if (!is.list(data) || is.null(data$mb)) {
    stop(
      "'data' must be the output of import_anq().",
      call. = FALSE
    )
  }
  if (!"unit" %in% names(data$mb)) {
    stop(
      "No 'unit' column found in data$mb. ",
      "Have you called assign_units() first?",
      call. = FALSE
    )
  }
  data$mb
}

#' Extract and validate $ph from an import_anq() result
#' @noRd
.extract_ph <- function(data) {
  if (!is.list(data) || is.null(data$ph)) {
    stop(
      "'data' must be the output of import_anq().",
      call. = FALSE
    )
  }
  if (!"unit" %in% names(data$ph)) {
    stop(
      "No 'unit' column found in data$ph. ",
      "Have you called assign_units() first?",
      call. = FALSE
    )
  }
  data$ph
}
