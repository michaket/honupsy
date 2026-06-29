# R/units.R

#' Import unit assignments
#'
#' Reads a lookup table that maps case identifiers (FID) to unit or ward
#' identifiers. This information is not part of the standard ANQ submission
#' format and must be supplied separately by the user.
#'
#' The input can be an Excel file, a delimited text file, or an existing
#' data frame. The user specifies which columns contain the FID and the
#' unit identifier.
#'
#' Each FID should appear only once. If a patient transferred between units
#' during their stay, we recommend using the unit the patient was on
#' immediately before discharge.
#'
#' @param x character or data.frame. Either a path to a file (XLSX, CSV,
#'   or delimited TXT) or an existing data frame.
#' @param col_fid character. Name of the column containing the case
#'   identifier (FID). Default: \code{"fid"}.
#' @param col_unit character. Name of the column containing the unit
#'   identifier. Default: \code{"unit"}.
#' @param sheet character or integer. For XLSX files: name or index of the
#'   worksheet to read. Default: \code{1} (first sheet).
#'
#' @return A data frame with two columns: \code{fid} (character) and
#'   \code{unit} (character). One row per case.
#'
#' @examples
#' \dontrun{
#' # from an Excel file
#' units <- import_unit_assignments(
#'   "unit_assignments.xlsx",
#'   col_fid  = "Fallnummer",
#'   col_unit = "Station"
#' )
#'
#' # from a CSV file
#' units <- import_unit_assignments(
#'   "unit_assignments.csv",
#'   col_fid  = "fall_id",
#'   col_unit = "ward"
#' )
#'
#' # from an existing data frame
#' df <- data.frame(
#'   fall_id = c("1001", "1002", "1003"),
#'   ward    = c("A1",   "B2",   "A1")
#' )
#' units <- import_unit_assignments(df,
#'   col_fid  = "fall_id",
#'   col_unit = "ward"
#' )
#' }
#'
#' @export
import_unit_assignments <- function(
  x,
  col_fid = "fid",
  col_unit = "unit",
  sheet = 1
) {
  # read input
  df <- if (is.data.frame(x)) {
    x
  } else if (is.character(x) && length(x) == 1) {
    read_unit_file(x, sheet = sheet)
  } else {
    stop(
      "'x' must be a file path or a data frame.",
      call. = FALSE
    )
  }

  # check columns exist
  missing_cols <- setdiff(c(col_fid, col_unit), names(df))
  if (length(missing_cols) > 0) {
    stop(
      "Column(s) not found in input: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # extract and rename
  result <- data.frame(
    fid = as.character(df[[col_fid]]),
    unit = as.character(df[[col_unit]]),
    stringsAsFactors = FALSE
  )

  # remove rows where FID is missing
  n_missing_fid <- sum(is.na(result$fid) | result$fid == "")
  if (n_missing_fid > 0) {
    warning(
      n_missing_fid,
      " row(s) with missing FID removed.",
      call. = FALSE
    )
    result <- result[!is.na(result$fid) & result$fid != "", ]
  }

  # remove rows where unit is missing
  n_missing_unit <- sum(is.na(result$unit) | result$unit == "")
  if (n_missing_unit > 0) {
    warning(
      n_missing_unit,
      " row(s) with missing unit removed.",
      call. = FALSE
    )
    result <- result[!is.na(result$unit) & result$unit != "", ]
  }

  # warn about duplicate FIDs
  duplicated_fids <- result$fid[duplicated(result$fid)]
  if (length(duplicated_fids) > 0) {
    warning(
      length(duplicated_fids),
      " duplicate FID(s) found. ",
      "Only one unit per case is allowed. ",
      "Keeping the first occurrence of each FID: ",
      paste(head(unique(duplicated_fids), 5), collapse = ", "),
      call. = FALSE
    )
    result <- result[!duplicated(result$fid), ]
  }

  result
}

#' Attach unit assignments to an imported ANQ submission
#'
#' Joins a unit lookup table (from \code{\link{import_unit_assignments}})
#' onto the relevant datasets in an imported ANQ submission. A \code{unit}
#' column is added to \code{mb}, \code{ph}, \code{pb}, and \code{fm}.
#'
#' @param data named list. Output of \code{\link{import_anq}}.
#' @param units data.frame. Output of
#'   \code{\link{import_unit_assignments}}.
#'
#' @return The input \code{data} list with a \code{unit} column added to
#'   \code{mb}, \code{mp}, \code{ph}, \code{pb}, and \code{fm}.
#'
#' @examples
#' \dontrun{
#' data  <- import_anq("data.txt")
#' units <- import_unit_assignments("units.xlsx",
#'                                  col_fid  = "Fallnummer",
#'                                  col_unit = "Station")
#' data  <- assign_units(data, units)
#'
#' # unit column is now available in all datasets
#' table(data$mb$unit)
#' }
#'
#' @export
assign_units <- function(data, units) {
  if (!is.list(data) || !all(c("mb", "ph", "pb", "fm") %in% names(data))) {
    stop(
      "'data' must be the output of import_anq().",
      call. = FALSE
    )
  }

  if (
    !is.data.frame(units) ||
      !all(c("fid", "unit") %in% names(units))
  ) {
    stop(
      "'units' must be the output of import_unit_assignments().",
      call. = FALSE
    )
  }

  # warn about FIDs in the data without a unit assignment
  if (nrow(data$mb) > 0) {
    fid_mb <- unique(data$mb$fid)
    fid_units <- unique(units$fid)
    unmatched <- setdiff(fid_mb, fid_units)
    if (length(unmatched) > 0) {
      warning(
        length(unmatched),
        " FID(s) in the imported data have no unit assignment. ",
        "These cases will have unit = NA: ",
        paste(head(unmatched, 5), collapse = ", "),
        call. = FALSE
      )
    }
    # warn about unit assignments for FIDs not in the data
    extra <- setdiff(fid_units, fid_mb)
    if (length(extra) > 0) {
      warning(
        length(extra),
        " FID(s) in unit assignments not found in the imported data.",
        call. = FALSE
      )
    }
  }

  # join unit onto each relevant dataset
  datasets <- c("mb", "mp", "ph", "pb", "fm")
  for (nm in datasets) {
    if (!is.null(data[[nm]]) && nrow(data[[nm]]) > 0) {
      data[[nm]] <- merge(
        data[[nm]],
        units,
        by = "fid",
        all.x = TRUE
      )
    }
  }

  data
}

# -- internal ------------------------------------------------------------------

#' Read a unit assignment file
#' Supports XLSX, CSV, and delimited TXT
#' @noRd
read_unit_file <- function(path, sheet = 1) {
  if (!file.exists(path)) {
    stop("File not found: ", path, call. = FALSE)
  }

  ext <- tolower(tools::file_ext(path))

  switch(
    ext,
    xlsx = ,
    xls = suppressMessages(
      readxl::read_excel(path, sheet = sheet, col_types = "text")
    ),
    csv = utils::read.csv(
      path,
      colClasses = "character",
      stringsAsFactors = FALSE,
      encoding = "UTF-8"
    ),
    {
      # TXT or unknown extension -- detect delimiter
      lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
      lines <- lines[nzchar(trimws(lines))]
      if (length(lines) == 0) {
        stop("File is empty: ", path, call. = FALSE)
      }
      sep <- switch(
        detect_delimiter(lines),
        pipe = "|",
        semicolon = ";",
        tab = "\t"
      )
      utils::read.table(
        path,
        sep = sep,
        header = TRUE,
        colClasses = "character",
        stringsAsFactors = FALSE,
        encoding = "UTF-8"
      )
    }
  )
}
