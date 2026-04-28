# Generates example data for inst/extdata/
# Based on the example rows from Appendix A9 of the ANQ data definition
# (measurement year 2022, version 8.2)
#
# Run with: source("data-raw/create_example_data.R")

# create directory if it does not exist
dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# Raw data
# MB: 52 fields per row
# Fields 5 (anonymous link code) and 12 (date of birth) are left empty
# for data protection reasons (see section 7 of the data definition)
# ------------------------------------------------------------------------------

mb_rows <- list(
  # case 1: FID 5050286, PID 34986734
  # female, 61 years, canton ZH, Swiss, admission 30.07.2012,
  # discharge 08.08.2012
  # main diagnosis F200 (schizophrenia)
  c(
    "MB",
    "12345678",
    "1234C",
    "ZH",
    "", # 1-5
    "A",
    "0",
    "1",
    "0",
    "1", # 6-10
    "2",
    "",
    "61",
    "7000",
    "CHE", # 11-15
    "2012073017",
    "31",
    "1",
    "3", # 16-19
    "1",
    "2",
    "0",
    "0",
    "M500",
    "2", # 20-25
    "2012080817",
    "1",
    "11",
    "1", # 26-29
    "F200",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "", # 30-39
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "", # 40-50
    "5050286",
    "34986734" # 51-52
  ),

  # case 2: FID 5050297, PID 34986737
  # male, 67 years, canton VD, Swiss, admission 03.01.2012, discharge 26.02.2012
  # main diagnosis F312 (bipolar disorder)
  c(
    "MB",
    "12345678",
    "1234C",
    "ZH",
    "", # 1-5
    "A",
    "0",
    "1",
    "0",
    "1", # 6-10
    "1",
    "",
    "67",
    "1024",
    "CHE", # 11-15
    "2012010307",
    "11",
    "1",
    "1", # 16-19
    "1",
    "2",
    "0",
    "0",
    "M500",
    "2", # 20-25
    "2012022614",
    "1",
    "11",
    "1", # 26-29
    "F312",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "", # 30-39
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "", # 40-50
    "5050297",
    "34986737" # 51-52
  ),

  # case 3: FID 5050292, PID 349867389
  # male, 46 years, canton GE, non-Swiss, admission 23.12.2012, discharge 09.01.2013
  # main diagnosis F102 (alcohol dependence syndrome)
  c(
    "MB",
    "12345678",
    "1234C",
    "ZH",
    "", # 1-5
    "B",
    "0",
    "1",
    "0",
    "1", # 6-10
    "1",
    "",
    "46",
    "7050",
    "NON", # 11-15
    "2012122315",
    "11",
    "1",
    "2", # 16-19
    "1",
    "2",
    "0",
    "0",
    "M500",
    "8", # 20-25
    "2013010914",
    "1",
    "11",
    "1", # 26-29
    "F102",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "", # 30-39
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "", # 40-50
    "5050292",
    "349867389" # 51-52
  )
)

# ------------------------------------------------------------------------------
# MP: 35 fields per row
# ------------------------------------------------------------------------------

mp_rows <- list(
  # case 1: FID 5050286
  # married, living at home before admission, full-time employed, general psychiatry
  c(
    "MP",
    "2",
    "11", # 1-3  (record type, civil status, location)
    "0",
    "1",
    "0",
    "0",
    "0",
    "0",
    "0", # 4-10 (employment)
    "0",
    "0",
    "2", # 11-13 (employment unknown, education)
    "11",
    "1",
    "1", # 14-16 (referring institution, voluntary, IPC)
    "9",
    "12", # 17-18 (number of days, treatment)
    "1",
    "1",
    "0",
    "0",
    "1",
    "0",
    "0", # 19-25 (pharmacotherapy)
    "0",
    "0",
    "0",
    "1",
    "0", # 26-30
    "11",
    "23",
    "1", # 31-33 (discharge)
    "1", # 34    (treatment area: general psychiatry)
    "5050286" # 35    (FID)
  ),

  # case 2: FID 5050297
  # single, living in a home before admission, on pension, general psychiatry
  c(
    "MP",
    "1",
    "12", # 1-3
    "0",
    "0",
    "1",
    "0",
    "0",
    "0",
    "1", # 4-10
    "0",
    "0",
    "9", # 11-13
    "31",
    "2",
    "2", # 14-16
    "6",
    "0", # 17-18
    "0",
    "0",
    "1",
    "0",
    "1",
    "0",
    "0", # 19-25
    "0",
    "0",
    "0",
    "1",
    "0", # 26-30
    "11",
    "12",
    "2", # 31-33
    "1", # 34
    "5050297" # 35
  ),

  # case 3: FID 5050292
  # divorced, living in a home before admission, unemployed, addiction area
  c(
    "MP",
    "3",
    "11", # 1-3
    "0",
    "0",
    "1",
    "0",
    "0",
    "0",
    "0", # 4-10
    "0",
    "0",
    "3", # 11-13
    "11",
    "1",
    "1", # 14-16
    "9",
    "1", # 17-18
    "0",
    "0",
    "0",
    "0",
    "1",
    "0",
    "0", # 19-25
    "1",
    "1",
    "0",
    "0",
    "0", # 26-30
    "11",
    "12",
    "1", # 31-33
    "4", # 34    (treatment area: addiction)
    "5050292" # 35
  )
)

# ------------------------------------------------------------------------------
# PH: 21 fields per row
# taken directly from appendix A9
# ------------------------------------------------------------------------------

ph_rows <- list(
  # case 1, admission
  c(
    "PH",
    "12345678",
    "5050286",
    "1",
    "0",
    "",
    "20120730",
    "2",
    "4",
    "4",
    "1",
    "2",
    "1",
    "4",
    "1",
    "a",
    "",
    "1",
    "3",
    "4",
    "4"
  ),

  # case 1, discharge
  c(
    "PH",
    "12345678",
    "5050286",
    "2",
    "0",
    "",
    "20120806",
    "2",
    "2",
    "2",
    "1",
    "2",
    "1",
    "2",
    "3",
    "a",
    "",
    "2",
    "4",
    "2",
    "1"
  ),

  # case 2, admission
  c(
    "PH",
    "12345678",
    "5050297",
    "1",
    "0",
    "",
    "20120103",
    "2",
    "4",
    "3",
    "3",
    "4",
    "4",
    "3",
    "0",
    "0",
    "",
    "4",
    "2",
    "2",
    "1"
  ),

  # case 2, discharge: dropout (discharge within 24h of admission assessment)
  c(
    "PH",
    "12345678",
    "5050297",
    "2",
    "1",
    "",
    "20120226",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    ""
  ),

  # case 3, admission
  c(
    "PH",
    "12345678",
    "5050292",
    "1",
    "0",
    "",
    "20121223",
    "4",
    "3",
    "2",
    "2",
    "4",
    "2",
    "2",
    "3",
    "b",
    "",
    "4",
    "2",
    "2",
    "3"
  ),

  # case 3, discharge (time point 3 = other, forensic psychiatry)
  c(
    "PH",
    "12345678",
    "5050292",
    "3",
    "0",
    "",
    "20121231",
    "3",
    "3",
    "4",
    "3",
    "3",
    "4",
    "4",
    "2",
    "b",
    "",
    "4",
    "1",
    "1",
    "3"
  )
)

# ------------------------------------------------------------------------------
# PB: 60 fields per row (7 header + 53 items)
# taken from the ANQ data template example row, adjusted for our 3 cases
# note: forensic psychiatry is exempt from BSCL since 01.07.2019
# case 3 (FID 5050292) therefore has a dropout
# ------------------------------------------------------------------------------

pb_rows <- list(
  # case 1: FID 5050286, admission
  c(
    "PB", # 1  record type
    "12345678", # 2  facility
    "5050286", # 3  FID
    "1", # 4  time_point (1=admission)
    "0", # 5  dropout_code (0=no dropout)
    "", # 6  dropout_detail
    "20120730", # 7  date
    # items B1-B53 (fields 8-60), scored 0-4
    "2",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "0",
    "1",
    "0", # B1-B10
    "1",
    "2",
    "1",
    "2",
    "1",
    "2",
    "3",
    "2",
    "1",
    "0", # B11-B20
    "1",
    "2",
    "0",
    "1",
    "2",
    "1",
    "0",
    "1",
    "0",
    "1", # B21-B30
    "2",
    "1",
    "0",
    "2",
    "3",
    "2",
    "1",
    "2",
    "1",
    "0", # B31-B40
    "1",
    "2",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "1",
    "2", # B41-B50
    "1",
    "0",
    "2" # B51-B53
  ),

  # case 1: FID 5050286, discharge
  c(
    "PB", # 1
    "12345678", # 2
    "5050286", # 3
    "2", # 4  time_point (2=discharge)
    "0", # 5
    "", # 6
    "20120806", # 7
    # items B1-B53, generally lower scores at discharge
    "1",
    "0",
    "0",
    "0",
    "1",
    "0",
    "0",
    "0",
    "0",
    "0", # B1-B10
    "0",
    "1",
    "0",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "0", # B11-B20
    "0",
    "1",
    "0",
    "0",
    "1",
    "0",
    "0",
    "0",
    "0",
    "0", # B21-B30
    "1",
    "0",
    "0",
    "1",
    "2",
    "1",
    "0",
    "1",
    "0",
    "0", # B31-B40
    "0",
    "1",
    "0",
    "0",
    "0",
    "1",
    "0",
    "0",
    "0",
    "1", # B41-B50
    "0",
    "0",
    "1" # B51-B53
  ),

  # case 2: FID 5050297, admission
  c(
    "PB", # 1
    "12345678", # 2
    "5050297", # 3
    "1", # 4
    "0", # 5
    "", # 6
    "20120103", # 7
    "3",
    "2",
    "1",
    "1",
    "3",
    "3",
    "4",
    "4",
    "2",
    "2", # B1-B10
    "1",
    "1",
    "0",
    "2",
    "2",
    "1",
    "1",
    "3",
    "3",
    "2", # B11-B20
    "2",
    "1",
    "1",
    "4",
    "4",
    "4",
    "4",
    "1",
    "2",
    "3", # B21-B30
    "2",
    "2",
    "4",
    "1",
    "1",
    "1",
    "0",
    "0",
    "0",
    "1", # B31-B40
    "1",
    "1",
    "0",
    "0",
    "0",
    "0",
    "1",
    "2",
    "1",
    "0", # B41-B50
    "4",
    "1",
    "0" # B51-B53
  ),

  # case 2: FID 5050297, discharge: dropout (discharge within 24h)
  c(
    "PB", # 1
    "12345678", # 2
    "5050297", # 3
    "2", # 4  time_point (2=discharge)
    "6", # 5  dropout_code (6=discharge within 24h of admission)
    "", # 6  dropout_detail
    "20120226", # 7  date
    # items B1-B53 all empty for dropout
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    ""
  ),

  # case 3: FID 5050292, admission: dropout (forensic, exempt from BSCL)
  c(
    "PB", # 1
    "12345678", # 2
    "5050292", # 3
    "1", # 4  time_point (1=admission)
    "8", # 5  dropout_code (8=other -- forensic exemption)
    "forensic psychiatry, exempt from BSCL since 01.07.2019", # 6
    "20121223", # 7  date
    # items B1-B53 all empty for dropout
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    ""
  ),

  # case 3: FID 5050292, discharge: dropout (forensic, exempt from BSCL)
  c(
    "PB", # 1
    "12345678", # 2
    "5050292", # 3
    "2", # 4  time_point (2=discharge)
    "8", # 5  dropout_code (8=other -- forensic exemption)
    "forensic psychiatry, exempt from BSCL since 01.07.2019", # 6
    "20121231", # 7  date
    # items B1-B53 all empty for dropout
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    ""
  )
)

# ------------------------------------------------------------------------------
# FM: 8 fields per row
# taken directly from appendix A9
# ------------------------------------------------------------------------------

fm_rows <- list(
  # case 1: psychiatric isolation (type=1), short
  c("FM", "12345678", "5050286", "1", "20120730", "1330", "20120730", "1700"),
  # case 1: physical restraint (type=2), overnight
  c("FM", "12345678", "5050286", "2", "20120806", "1100", "20120807", "0900"),
  # case 3: safety measure bed (type=7), multi-day
  c("FM", "12345678", "5050292", "7", "20120606", "0900", "20120618", "1000"),
  # case 3: safety measure chair (type=5)
  c("FM", "12345678", "5050292", "5", "20120806", "1100", "20120807", "0900")
)

# ------------------------------------------------------------------------------
# Helper: collapse a list of character vectors into pipe-delimited strings
# ------------------------------------------------------------------------------

as_pipe_lines <- function(rows) {
  vapply(rows, paste, character(1), collapse = "|")
}

# ------------------------------------------------------------------------------
# Helper: convert a list of character vectors to a data frame,
# padding shorter rows with empty strings
# ------------------------------------------------------------------------------

rows_to_df <- function(rows) {
  max_fields <- max(vapply(rows, length, integer(1)))
  rows_padded <- lapply(rows, function(r) {
    length(r) <- max_fields
    r[is.na(r)] <- ""
    r
  })
  as.data.frame(
    do.call(rbind, rows_padded),
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------------------------
# 1. Mixed TXT file (all record types together, pipe-delimited)
# ------------------------------------------------------------------------------

all_lines <- c(
  as_pipe_lines(mb_rows),
  as_pipe_lines(mp_rows),
  as_pipe_lines(ph_rows),
  as_pipe_lines(pb_rows),
  as_pipe_lines(fm_rows)
)

writeLines(all_lines, "inst/extdata/example.txt", useBytes = FALSE)
message("Created: inst/extdata/example.txt")

# ------------------------------------------------------------------------------
# 2. Separate TXT files per record type
# ------------------------------------------------------------------------------

writeLines(as_pipe_lines(mb_rows), "inst/extdata/example_mb.txt")
writeLines(as_pipe_lines(mp_rows), "inst/extdata/example_mp.txt")
writeLines(as_pipe_lines(ph_rows), "inst/extdata/example_ph.txt")
writeLines(as_pipe_lines(pb_rows), "inst/extdata/example_pb.txt")
writeLines(as_pipe_lines(fm_rows), "inst/extdata/example_fm.txt")
message("Created: inst/extdata/example_mb/mp/ph/fm.txt")

# ------------------------------------------------------------------------------
# 3. Semicolon-delimited TXT file (for testing delimiter detection)
# ------------------------------------------------------------------------------

as_semi_lines <- function(rows) {
  vapply(rows, paste, character(1), collapse = ";")
}

all_semi <- c(
  as_semi_lines(mb_rows),
  as_semi_lines(mp_rows),
  as_semi_lines(ph_rows),
  as_semi_lines(pb_rows),
  as_semi_lines(fm_rows)
)

writeLines(all_semi, "inst/extdata/example_semicolon.txt")
message("Created: inst/extdata/example_semicolon.txt")

# ------------------------------------------------------------------------------
# 4. XLSX file with one worksheet per record type (simple format)
# ------------------------------------------------------------------------------

xlsx_data <- list(
  MB = rows_to_df(mb_rows),
  MP = rows_to_df(mp_rows),
  PH = rows_to_df(ph_rows),
  PB = rows_to_df(pb_rows),
  FM = rows_to_df(fm_rows)
)

if (!requireNamespace("writexl", quietly = TRUE)) {
  message("writexl is not installed, XLSX files will not be created.")
  message("Install with: install.packages('writexl')")
} else {
  writexl::write_xlsx(xlsx_data, "inst/extdata/example.xlsx")
  message("Created: inst/extdata/example.xlsx")

  # --------------------------------------------------------------------------
  # 5. Copy of the official ANQ data entry template filled with example data
  # (for testing read_anq_template)
  # Sheet names and the "Ab hier Eingabe" marker are kept in German --
  # these are literal strings from the official ANQ template and must
  # not be translated
  # --------------------------------------------------------------------------

  make_template_sheet <- function(rows) {
    df_data <- rows_to_df(rows)
    # reconstruct the template header structure:
    # several empty rows, then the "Ab hier Eingabe" marker,
    # then one empty row, then the actual data
    n_cols <- ncol(df_data)
    empty <- as.data.frame(
      matrix("", nrow = 1, ncol = n_cols),
      stringsAsFactors = FALSE
    )
    marker <- empty
    marker[1, 1] <- "Ab hier Eingabe"
    rbind(empty, empty, empty, empty, empty, empty, marker, empty, df_data)
  }

  template_data <- list(
    "MB - Minimales Datenset" = make_template_sheet(mb_rows),
    "MP - Psychiatrie Zusatzdaten" = make_template_sheet(mp_rows),
    "PH - HoNOS" = make_template_sheet(ph_rows),
    "PB - BSCL" = make_template_sheet(pb_rows),
    "FM - Freiheitsbeschränkende M" = make_template_sheet(fm_rows)
  )

  writexl::write_xlsx(template_data, "inst/extdata/example_anq_template.xlsx")
  message("Created: inst/extdata/example_anq_template.xlsx")
}

message("Done. All example data written to inst/extdata/")
