#' Parse a date in ANQ format YYYYMMDD
#' @param x character, 8 characters wide
#' @return Date or NA
#' @noRd
parse_date_anq <- function(x) {
  x <- trimws(x)
  if (is.na(x) || x == "" || nchar(x) < 8) {
    return(NA_real_)
  }
  as.Date(substr(x, 1, 8), format = "%Y%m%d")
}

#' Parse a date+hour in ANQ format YYYYMMDDhh
#' @param x character, 10 characters wide
#' @return POSIXct or NA
#' @noRd
parse_datetime_anq <- function(x) {
  x <- trimws(x)
  if (is.na(x) || x == "" || nchar(x) < 8) {
    return(NA)
  }
  # sometimes only 8 characters wide, without hour
  if (nchar(x) == 8) {
    x <- paste0(x, "00")
  }
  as.POSIXct(x, format = "%Y%m%d%H", tz = "Europe/Zurich")
}

#' Parse a time in ANQ format hhmm
#' @param x character, 4 characters wide
#' @return integer, minutes since midnight, or NA
#' @noRd
parse_datetime_anq <- function(x) {
  x <- trimws(x)
  if (is.na(x) || x == "" || nchar(x) < 8) {
    return(NA)
  }
  if (nchar(x) == 8) {
    x <- paste0(x, "00")
  }
  as.POSIXct(x, format = "%Y%m%d%H", tz = "UTC")
}

#' Parse an integer, treating empty strings as NA
#' @noRd
parse_int <- function(x) {
  x <- trimws(x)
  if (is.na(x) || x == "") {
    return(NA_integer_)
  }
  # only accept strings that look like whole numbers (no decimal point)
  if (grepl(".", x, fixed = TRUE)) {
    return(NA_integer_)
  }
  suppressWarnings(as.integer(x))
}

#' Labels for coercive measure types
#' Codes as defined in ANQ data definition appendix A7 (valid from 01.01.2021)
#' @noRd
coercive_measure_label <- function(code) {
  labels <- c(
    "1" = "Psychiatric isolation",
    "2" = "Physical restraint",
    "3" = "Involuntary medication oral",
    "4" = "Involuntary medication injection",
    "5" = "Safety measure chair",
    "7" = "Safety measure bed",
    "10" = "Manual restraint",
    "11" = "Isolation for infectious/somatic reasons"
  )
  unname(labels[as.character(code)])
}

#' Label for assessment time point
#' Used by both HoNOS (PH) and BSCL (PB) parsers
#' ANQ codes: 1=admission, 2=discharge, 3=other
#' @noRd
time_point_label <- function(code) {
  switch(
    as.character(code),
    "1" = "admission",
    "2" = "discharge",
    "3" = "other",
    NA_character_
  )
}
