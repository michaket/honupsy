# import.R

#' Import ANQ data
#'
#' Reads ANQ data for the national quality measurement in inpatient adult
#' psychiatry. Supports TXT (with TAB, semicolon or pipe as delimiter) and
#' XLSX, either as a single mixed file or as separate files per record type.
#'
#' @param path character. Either a single path to a mixed file (TXT or XLSX),
#'   or a named character vector of paths to separate files. Names must
#'   correspond to record types:
#'   \code{c(MB = "mb.txt", MP = "mp.txt", PH = "ph.txt", FM = "fm.txt")}.
#'   Not all record types need to be provided.
#' @param validate logical. Should a plausibility check be performed?
#'   Default: \code{TRUE}.
#'
#' @return A named list with elements \code{mb}, \code{mp}, \code{ph} and
#'   \code{fm}, each as a data.frame. Record types not present in the input
#'   are returned as empty data.frames with the correct column types.
#'
#' @examples
#' # example files shipped with the package
#' path_txt  <- system.file("extdata", "beispiel.txt",  package = "anqimport")
#' path_xlsx <- system.file("extdata", "beispiel.xlsx", package = "anqimport")
#'
#' # single mixed TXT file
#' data <- import_anq(path_txt)
#'
#' # single XLSX file
#' data <- import_anq(path_xlsx)
#'
#' # access individual datasets
#' head(data$mb)
#' head(data$ph)
#'
#' # import only MB and FM
#' path_mb <- system.file("extdata", "beispiel_mb.txt", package = "anqimport")
#' path_fm <- system.file("extdata", "beispiel_fm.txt", package = "anqimport")
#' data <- import_anq(c(MB = path_mb, FM = path_fm))
#'
#' # skip validation
#' data <- import_anq(path_txt, validate = FALSE)
#'
#' # inspect validation messages
#' data <- import_anq(path_txt)
#' data$.validation
#'
#' \dontrun{
#' # your own files
#' data <- import_anq("path/to/data.txt")
#' data <- import_anq("path/to/data.xlsx")
#'
#' # separate files
#' data <- import_anq(c(
#'   MB = "path/to/mb.txt",
#'   MP = "path/to/mp.txt",
#'   PH = "path/to/ph.txt",
#'   FM = "path/to/fm.txt"
#' ))
#' }
#'
#' @export
import_anq <- function(path, validate = TRUE) {
  # check input
  if (!is.character(path) || length(path) == 0) {
    stop(
      "'path' must be a character vector with at least one path.",
      call. = FALSE
    )
  }

  # check files exist
  not_found <- path[!file.exists(path)]
  if (length(not_found) > 0) {
    stop(
      "File(s) not found:\n",
      paste("-", not_found, collapse = "\n"),
      call. = FALSE
    )
  }

  # read raw data
  raw <- if (length(path) == 1 && is.null(names(path))) {
    # single unnamed file: mixed file
    read_anq_raw(path)
  } else {
    # single named file or multiple files
    read_anq_multi(path)
  }

  # known record types we process
  # PB (BSCL) is intentionally not processed here
  known <- c("MB", "MP", "PH", "FM")

  # report unknown record types
  unknown <- setdiff(names(raw), known)
  if (length(unknown) > 0) {
    message(
      "The following record types are ignored: ",
      paste(unknown, collapse = ", ")
    )
  }

  # parse
  result <- list(
    mb = if ("MB" %in% names(raw)) parse_mb(raw[["MB"]]) else empty_mb(),
    mp = if ("MP" %in% names(raw)) parse_mp(raw[["MP"]]) else empty_mp(),
    ph = if ("PH" %in% names(raw)) parse_ph(raw[["PH"]]) else empty_ph(),
    fm = if ("FM" %in% names(raw)) parse_fm(raw[["FM"]]) else empty_fm()
  )

  # validate
  if (validate) {
    msgs <- validate_anq(result)
    result$.validation <- msgs
  }

  # print summary
  message(import_summary(result))

  result
}

#' Format an import summary as a string
#' @noRd
import_summary <- function(data) {
  lines <- c(
    "ANQ import completed:",
    sprintf("  MB: %d cases", nrow(data$mb)),
    sprintf("  MP: %d cases", nrow(data$mp)),
    sprintf(
      "  PH: %d assessments (%d admission, %d discharge, %d dropout)",
      nrow(data$ph),
      sum(
        !is.na(data$ph$time_point) &
          data$ph$time_point == 1L &
          !data$ph$is_dropout
      ),
      sum(
        !is.na(data$ph$time_point) &
          data$ph$time_point == 2L &
          !data$ph$is_dropout
      ),
      sum(data$ph$is_dropout)
    ),
    sprintf(
      "  FM: %d measures for %d cases",
      nrow(data$fm),
      length(unique(data$fm$fid))
    )
  )
  paste(lines, collapse = "\n")
}
