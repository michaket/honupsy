# -- parse_mb_row: valid input -------------------------------------------------

test_that("parse_mb_row parses a valid row correctly", {
  # case 1 from example data
  r <- c(
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
  )

  result <- parse_mb_row(r)

  # identification
  expect_equal(result$fid, "5050286")
  expect_equal(result$pid, "34986734")
  expect_equal(result$facility, "12345678")
  expect_equal(result$location, "1234C")
  expect_equal(result$kanton, "ZH")

  # patient
  expect_equal(result$sex, 2L) # female
  expect_equal(result$age_admission, 61L)
  expect_equal(result$nationality, "CHE")

  # admission
  expect_s3_class(result$admission, "POSIXct")
  expect_equal(format(result$admission, "%Y-%m-%d %H"), "2012-07-30 17")
  expect_equal(result$location_before_admission, 31L)
  expect_equal(result$admission_type, 1L)
  expect_equal(result$referring_institution, 3L)

  # treatment
  expect_equal(result$main_cost_centre, "M500")

  # discharge
  expect_s3_class(result$discharge, "POSIXct")
  expect_equal(format(result$discharge, "%Y-%m-%d %H"), "2012-08-08 17")

  # diagnoses
  expect_equal(result$main_diagnosis, "F200")

  # flags
  expect_equal(result$flag_psychiatry, 1L)
})

# -- parse_mb_row: too few fields ----------------------------------------------

test_that("parse_mb_row skips rows with too few fields and warns", {
  r <- c("MB", "12345678", "1234C") # only 3 fields
  expect_warning(
    result <- parse_mb_row(r),
    regexp = "52 expected"
  )
  expect_null(result)
})

# -- parse_mb: multiple rows ---------------------------------------------------

test_that("parse_mb returns one row per valid case", {
  r1 <- c(
    "MB",
    "12345678",
    "1234C",
    "ZH",
    "",
    "A",
    "0",
    "1",
    "0",
    "1",
    "2",
    "",
    "61",
    "7000",
    "CHE",
    "2012073017",
    "31",
    "1",
    "3",
    "1",
    "2",
    "0",
    "0",
    "M500",
    "2",
    "2012080817",
    "1",
    "11",
    "1",
    "F200",
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
    "5050286",
    "34986734"
  )
  r2 <- c(
    "MB",
    "12345678",
    "1234C",
    "ZH",
    "",
    "A",
    "0",
    "1",
    "0",
    "1",
    "1",
    "",
    "67",
    "1024",
    "CHE",
    "2012010307",
    "11",
    "1",
    "1",
    "1",
    "2",
    "0",
    "0",
    "M500",
    "2",
    "2012022614",
    "1",
    "11",
    "1",
    "F312",
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
    "5050297",
    "34986737"
  )

  result <- parse_mb(list(r1, r2))
  expect_equal(nrow(result), 2L)
  expect_equal(result$fid, c("5050286", "5050297"))
})

test_that("parse_mb skips invalid rows and keeps valid ones", {
  r_valid <- c(
    "MB",
    "12345678",
    "1234C",
    "ZH",
    "",
    "A",
    "0",
    "1",
    "0",
    "1",
    "2",
    "",
    "61",
    "7000",
    "CHE",
    "2012073017",
    "31",
    "1",
    "3",
    "1",
    "2",
    "0",
    "0",
    "M500",
    "2",
    "2012080817",
    "1",
    "11",
    "1",
    "F200",
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
    "5050286",
    "34986734"
  )
  r_invalid <- c("MB", "12345678") # too short

  expect_warning(
    result <- parse_mb(list(r_valid, r_invalid))
  )
  expect_equal(nrow(result), 1L)
  expect_equal(result$fid, "5050286")
})


# Skip empty rows ---------------------------------------------------------

test_that("parse_mb skips trailing empty rows silently", {
  r_valid <- c(
    "MB",
    "12345678",
    "1234C",
    "ZH",
    "",
    "A",
    "0",
    "1",
    "0",
    "1",
    "2",
    "",
    "61",
    "7000",
    "CHE",
    "2012073017",
    "31",
    "1",
    "3",
    "1",
    "2",
    "0",
    "0",
    "M500",
    "2",
    "2012080817",
    "1",
    "11",
    "1",
    "F200",
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
    "5050286",
    "34986734"
  )
  r_empty <- c("", "", "")

  expect_no_warning(
    result <- parse_mb(list(r_valid, r_empty))
  )
  expect_equal(nrow(result), 1L)
})

test_that("parse_mb skips empty rows between valid rows silently", {
  r_valid1 <- c(
    "MB",
    "12345678",
    "1234C",
    "ZH",
    "",
    "A",
    "0",
    "1",
    "0",
    "1",
    "2",
    "",
    "61",
    "7000",
    "CHE",
    "2012073017",
    "31",
    "1",
    "3",
    "1",
    "2",
    "0",
    "0",
    "M500",
    "2",
    "2012080817",
    "1",
    "11",
    "1",
    "F200",
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
    "5050286",
    "34986734"
  )
  r_empty <- c("", "", "")
  r_valid2 <- c(
    "MB",
    "12345678",
    "1234C",
    "ZH",
    "",
    "A",
    "0",
    "1",
    "0",
    "1",
    "1",
    "",
    "67",
    "1024",
    "CHE",
    "2012010307",
    "11",
    "1",
    "1",
    "1",
    "2",
    "0",
    "0",
    "M500",
    "2",
    "2012022614",
    "1",
    "11",
    "1",
    "F312",
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
    "5050297",
    "34986737"
  )

  expect_no_warning(
    result <- parse_mb(list(r_valid1, r_empty, r_valid2))
  )
  expect_equal(nrow(result), 2L)
})


# -- empty_mb ------------------------------------------------------------------

test_that("empty_mb returns a data frame with zero rows and correct types", {
  result <- empty_mb()

  expect_equal(nrow(result), 0L)

  # key column types
  expect_type(result$fid, "character")
  expect_type(result$pid, "character")
  expect_type(result$sex, "integer")
  expect_type(result$age_admission, "integer")
  expect_type(result$nationality, "character")
  expect_type(result$main_diagnosis, "character")
  expect_s3_class(result$admission, "POSIXct")
  expect_s3_class(result$discharge, "POSIXct")
})


test_that("MB row with blank final PID is preserved", {
  tmp <- tempfile(fileext = ".txt")

  fields <- rep("", 52)
  fields[1] <- "MB"
  fields[2] <- "12345678"
  fields[51] <- "FID001"
  fields[52] <- ""

  writeLines(
    paste(fields, collapse = "|"),
    tmp
  )

  raw <- read_txt_raw(tmp)
  result <- parse_mb(raw$MB)

  expect_equal(nrow(result), 1L)
  expect_equal(result$fid, "FID001")
  expect_equal(result$pid, "")

  unlink(tmp)
})
