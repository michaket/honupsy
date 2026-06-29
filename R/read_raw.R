# read_raw.R

#' Detect the delimiter of a text file
#' @param lines character vector, first few lines of the file
#' @return character, one of "pipe", "semicolon", "tab"
#' @noRd
detect_delimiter <- function(lines) {
  # check only the first non-empty line
  line <- lines[nzchar(trimws(lines))][1]
  counts <- c(
    pipe = nchar(line) - nchar(gsub("|", "", line, fixed = TRUE)),
    semicolon = nchar(line) - nchar(gsub(";", "", line, fixed = TRUE)),
    tab = nchar(line) - nchar(gsub("\t", "", line, fixed = TRUE))
  )
  names(which.max(counts))
}

#' Read a text file and split by record type
#' @param path path to the file
#' @return named list of lists, names are record types (MB, MP, PH, FM, ...)
#' @noRd
read_txt_raw <- function(path) {
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]

  if (length(lines) == 0) {
    stop("File is empty: ", path)
  }

  delim <- detect_delimiter(lines)
  sep <- switch(delim, pipe = "|", semicolon = ";", tab = "\t")

  records <- strsplit(lines, split = sep, fixed = TRUE)
  types <- vapply(records, `[[`, character(1), 1)
  split(records, types)
}

#' Read an XLSX file and split by worksheet
#' @param path path to the xlsx file
#' @return named list of lists, names are record types (MB, MP, PH, FM, ...)
#' @noRd
read_xlsx_raw <- function(path) {
  sheets <- readxl::excel_sheets(path)

  known <- c("MB", "MP", "PH", "PB", "FM")
  sheets <- sheets[sheets %in% known]

  if (length(sheets) == 0) {
    stop("No known worksheets found in: ", path)
  }

  result <- lapply(sheets, function(sheet) {
    df <- suppressMessages(
      readxl::read_excel(
        path,
        sheet = sheet,
        col_types = "text",
        col_names = FALSE
      )
    )

    rows <- lapply(seq_len(nrow(df)), function(i) as.character(df[i, ]))

    # writexl writes data.frame column names as the first row --
    # drop this header row if the first cell is not a known record type
    # (i.e. it looks like "V1", "...1" or similar)
    if (length(rows) > 0 && !trimws(rows[[1]][[1]]) %in% known) {
      rows <- rows[-1]
    }

    rows
  })

  names(result) <- sheets
  result
}

#' Read a file, automatically detecting the format from the file extension
#' @param path path to the file
#' @return named list of lists
#' @noRd
read_anq_raw <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(
    ext,
    xlsx = {
      if (is_anq_template(path)) {
        read_anq_template(path)
      } else {
        read_xlsx_raw(path)
      }
    },
    xls = {
      if (is_anq_template(path)) {
        read_anq_template(path)
      } else {
        read_xlsx_raw(path)
      }
    },
    read_txt_raw(path)
  )
}

#' Read multiple separate files and merge them
#' @param paths named character vector, e.g. c(MB = "mb.txt", FM = "fm.txt")
#' @return named list of lists
#' @noRd
read_anq_multi <- function(paths) {
  result <- list()

  for (nm in names(paths)) {
    raw <- read_anq_raw(paths[[nm]])

    # use the record type from the file contents, not from the vector name --
    # but warn if the two do not match
    if (!nm %in% names(raw)) {
      warning(
        "File '",
        paths[[nm]],
        "' does not contain record type '",
        nm,
        "'",
        call. = FALSE
      )
    }

    result <- c(result, raw)
  }

  result
}


# Sheet names of the official ANQ data-entry template
ANQ_TEMPLATE_SHEETS <- c(
  "MB - Minimales Datenset" = "MB",
  "MP - Psychiatrie Zusatzdaten" = "MP",
  "PH - HoNOS" = "PH",
  "PB - BSCL" = "PB",
  "FM - Freiheitsbeschr\u00e4nkende M" = "FM"
)

#' Check whether an XLSX file is the official ANQ data-entry template
#' Detected by presence of at least one sheet with the expected full name
#' @param path path to the xlsx file
#' @return logical
#' @noRd
is_anq_template <- function(path) {
  sheets <- suppressMessages(readxl::excel_sheets(path))
  any(sheets %in% names(ANQ_TEMPLATE_SHEETS))
}

#' Read the official ANQ data-entry template
#' Handles the multi-row header structure and locates data via the
#' "Ab hier Eingabe" marker row
#' @param path path to the xlsx file
#' @return named list of lists, names are record types (MB, MP, PH, FM, ...)
#' @noRd
read_anq_template <- function(path) {
  sheets <- suppressMessages(readxl::excel_sheets(path))

  # keep only known template sheets
  known <- sheets[sheets %in% names(ANQ_TEMPLATE_SHEETS)]

  if (length(known) == 0) {
    stop("No known ANQ template sheets found in: ", path)
  }

  result <- lapply(known, function(sheet) {
    df <- suppressMessages(
      readxl::read_excel(
        path,
        sheet = sheet,
        col_types = "text",
        col_names = FALSE
      )
    )

    # convert to list of character vectors, one per row
    rows <- lapply(seq_len(nrow(df)), function(i) {
      as.character(df[i, ])
    })

    # find the "Ab hier Eingabe" marker row
    marker <- which(vapply(
      rows,
      function(r) {
        trimws(r[[1]]) == "Ab hier Eingabe"
      },
      logical(1)
    ))

    if (length(marker) == 0) {
      warning(
        "Marker 'Ab hier Eingabe' not found in sheet '",
        sheet,
        "'. ",
        "Attempting to read from row 1.",
        call. = FALSE
      )
      data_rows <- rows
    } else {
      # skip marker row + repeated header row after it
      first_data <- marker[[1]] + 2L
      if (first_data > length(rows)) {
        return(list())
      }
      data_rows <- rows[seq(first_data, length(rows))]
    }

    # keep only rows that start with the correct record type
    record_type <- ANQ_TEMPLATE_SHEETS[[sheet]]
    data_rows <- Filter(
      function(r) {
        trimws(r[[1]]) == record_type
      },
      data_rows
    )

    # replace NA strings from readxl with empty strings
    lapply(data_rows, function(r) {
      r[is.na(r)] <- ""
      r
    })
  })

  # name by internal record type
  names(result) <- ANQ_TEMPLATE_SHEETS[known]
  result
}
