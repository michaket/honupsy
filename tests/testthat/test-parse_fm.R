# -- parse_fm_row: valid input -------------------------------------------------

test_that("parse_fm_row parses a valid non-coercive-medication row correctly", {
  # case 1: psychiatric isolation (type=1), short duration
  r <- c(
    "FM", # 1  record type
    "12345678", # 2  facility
    "5050286", # 3  FID
    "1", # 4  measure_type (1=psychiatric isolation)
    "20120730", # 5  start_date
    "1330", # 6  start_time
    "20120730", # 7  end_date
    "1700" # 8  end_time
  )

  result <- parse_fm_row(r)

  # identification
  expect_equal(result$fid, "5050286")
  expect_equal(result$facility, "12345678")

  # measure
  expect_equal(result$measure_type, 1L)
  expect_equal(result$measure_label, "Psychiatric isolation")
  expect_false(result$is_coercive_medication)

  # start
  expect_equal(result$start_date, as.Date("2012-07-30"))
  expect_equal(result$start_time, 13L * 60L + 30L) # 810 minutes

  # end
  expect_equal(result$end_date, as.Date("2012-07-30"))
  expect_equal(result$end_time, 17L * 60L) # 1020 minutes

  # duration: same day, 1700 - 1330 = 30 minutes
  expect_equal(result$duration_min, 210L)
})

test_that("parse_fm_row parses an overnight measure correctly", {
  # case 1: physical restraint (type=2), overnight
  r <- c(
    "FM", # 1
    "12345678", # 2
    "5050286", # 3
    "2", # 4  measure_type (2=physical restraint)
    "20120806", # 5  start_date
    "1100", # 6  start_time
    "20120807", # 7  end_date
    "0900" # 8  end_time
  )

  result <- parse_fm_row(r)

  expect_equal(result$measure_type, 2L)
  expect_equal(result$measure_label, "Physical restraint")
  expect_false(result$is_coercive_medication)

  # duration: overnight
  # 20120806 1100 to 20120807 0900 = 22 hours = 1320 minutes
  expect_equal(result$duration_min, 1320L)
})

test_that("parse_fm_row parses a multi-day measure correctly", {
  # case 3: safety measure bed (type=7), multi-day
  r <- c(
    "FM", # 1
    "12345678", # 2
    "5050292", # 3
    "7", # 4  measure_type (7=safety measure bed)
    "20120606", # 5  start_date
    "0900", # 6  start_time
    "20120618", # 7  end_date
    "1000" # 8  end_time
  )

  result <- parse_fm_row(r)

  expect_equal(result$measure_type, 7L)
  expect_equal(result$measure_label, "Safety measure bed")
  expect_false(result$is_coercive_medication)

  # duration: 12 days + 1 hour = 12*1440 + 60 = 17340 minutes
  expect_equal(result$duration_min, 17340L)
})

# -- parse_fm_row: coercive medication -----------------------------------------

test_that("parse_fm_row handles involuntary medication oral correctly", {
  r <- c(
    "FM", # 1
    "12345678", # 2
    "5050286", # 3
    "3", # 4  measure_type (3=involuntary medication oral)
    "20120730", # 5  start_date
    "1330", # 6  start_time
    "", # 7  end_date (empty for coercive medication)
    "" # 8  end_time (empty for coercive medication)
  )

  result <- parse_fm_row(r)

  expect_equal(result$measure_type, 3L)
  expect_equal(result$measure_label, "Involuntary medication oral")
  expect_true(result$is_coercive_medication)

  # start is recorded
  expect_equal(result$start_date, as.Date("2012-07-30"))
  expect_equal(result$start_time, 810L)

  # end and duration are NA for coercive medication
  expect_true(is.na(result$end_date))
  expect_true(is.na(result$end_time))
  expect_true(is.na(result$duration_min))
})

test_that("parse_fm_row handles involuntary medication injection correctly", {
  r <- c(
    "FM",
    "12345678",
    "5050286",
    "4", # measure_type (4=involuntary medication injection)
    "20120730",
    "1330",
    "",
    ""
  )

  result <- parse_fm_row(r)

  expect_equal(result$measure_type, 4L)
  expect_true(result$is_coercive_medication)
  expect_true(is.na(result$duration_min))
})

# -- parse_fm_row: all measure types -------------------------------------------

test_that("parse_fm_row recognises all known measure types", {
  make_row <- function(type) {
    c(
      "FM",
      "12345678",
      "5050286",
      as.character(type),
      "20120730",
      "0900",
      "20120730",
      "1000"
    )
  }

  expect_equal(parse_fm_row(make_row(1))$measure_label, "Psychiatric isolation")
  expect_equal(parse_fm_row(make_row(2))$measure_label, "Physical restraint")
  expect_equal(parse_fm_row(make_row(5))$measure_label, "Safety measure chair")
  expect_equal(parse_fm_row(make_row(7))$measure_label, "Safety measure bed")
  expect_equal(parse_fm_row(make_row(10))$measure_label, "Manual restraint")
  expect_equal(
    parse_fm_row(make_row(11))$measure_label,
    "Isolation for infectious/somatic reasons"
  )
})

# -- parse_fm_row: implausible duration ----------------------------------------

test_that("parse_fm_row returns NA duration and warns for negative duration", {
  # end before start
  r <- c(
    "FM",
    "12345678",
    "5050286",
    "1",
    "20120730",
    "1700", # start 17:00
    "20120730",
    "0900" # end 09:00 -- before start
  )

  expect_warning(
    result <- parse_fm_row(r),
    regexp = "Negative duration"
  )
  expect_true(is.na(result$duration_min))
})

# -- parse_fm_row: too few fields ----------------------------------------------

test_that("parse_fm_row skips rows with too few fields and warns", {
  r <- c("FM", "12345678", "5050286") # only 3 fields
  expect_warning(
    result <- parse_fm_row(r),
    regexp = "at least 6 expected"
  )
  expect_null(result)
})

# -- parse_fm: multiple rows ---------------------------------------------------

test_that("parse_fm returns one row per measure", {
  r1 <- c(
    "FM",
    "12345678",
    "5050286",
    "1",
    "20120730",
    "1330",
    "20120730",
    "1700"
  )
  r2 <- c(
    "FM",
    "12345678",
    "5050286",
    "2",
    "20120806",
    "1100",
    "20120807",
    "0900"
  )
  r3 <- c(
    "FM",
    "12345678",
    "5050292",
    "7",
    "20120606",
    "0900",
    "20120618",
    "1000"
  )

  result <- parse_fm(list(r1, r2, r3))

  expect_equal(nrow(result), 3L)
  expect_equal(result$fid, c("5050286", "5050286", "5050292"))
  expect_equal(result$measure_type, c(1L, 2L, 7L))
})

test_that("parse_fm returns empty data frame when no measures present", {
  result <- parse_fm(list())
  expect_equal(nrow(result), 0L)
  expect_equal(ncol(result), ncol(empty_fm()))
})

# -- parse_fm: empty rows ------------------------------------------------------

test_that("parse_fm skips trailing empty rows silently", {
  r_valid <- c(
    "FM",
    "12345678",
    "5050286",
    "1",
    "20120730",
    "1330",
    "20120730",
    "1700"
  )
  r_empty <- c("", "", "")

  expect_no_warning(
    result <- parse_fm(list(r_valid, r_empty))
  )
  expect_equal(nrow(result), 1L)
})

test_that("parse_fm skips empty rows between valid rows silently", {
  r_valid1 <- c(
    "FM",
    "12345678",
    "5050286",
    "1",
    "20120730",
    "1330",
    "20120730",
    "1700"
  )
  r_empty <- c("", "", "")
  r_valid2 <- c(
    "FM",
    "12345678",
    "5050286",
    "2",
    "20120806",
    "1100",
    "20120807",
    "0900"
  )

  expect_no_warning(
    result <- parse_fm(list(r_valid1, r_empty, r_valid2))
  )
  expect_equal(nrow(result), 2L)
})

# -- empty_fm ------------------------------------------------------------------

test_that("empty_fm returns a data frame with zero rows and correct types", {
  result <- empty_fm()

  expect_equal(nrow(result), 0L)

  expect_type(result$fid, "character")
  expect_type(result$facility, "character")
  expect_type(result$measure_type, "integer")
  expect_type(result$measure_label, "character")
  expect_type(result$is_coercive_medication, "logical")
  expect_s3_class(result$start_date, "Date")
  expect_type(result$start_time, "integer")
  expect_s3_class(result$end_date, "Date")
  expect_type(result$end_time, "integer")
  expect_type(result$duration_min, "integer")
})
