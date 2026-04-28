# -- parse_ph_row: valid input -------------------------------------------------

test_that("parse_ph_row parses a valid admission row correctly", {
  # case 1, admission, from example data (appendix A9)
  r <- c(
    "PH", # 1  record type
    "12345678", # 2  facility
    "5050286", # 3  FID
    "1", # 4  time_point (1=admission)
    "0", # 5  dropout_code (0=no dropout)
    "", # 6  dropout_detail
    "20120730", # 7  date
    "2", # 8  h1 aggression
    "4", # 9  h2 self-harm
    "4", # 10 h3 substance use
    "1", # 11 h4 cognitive
    "2", # 12 h5 physical
    "1", # 13 h6 hallucination
    "4", # 14 h7 mood
    "1", # 15 h8 other mental
    "a", # 16 h8 type
    "", # 17 h8 detail
    "1", # 18 h9 relationships
    "3", # 19 h10 daily living
    "4", # 20 h11 living conditions
    "4" # 21 h12 occupation
  )

  result <- parse_ph_row(r)

  # identification
  expect_equal(result$fid, "5050286")
  expect_equal(result$facility, "12345678")

  # time point
  expect_equal(result$time_point, 1L)
  expect_equal(result$time_point_label, "admission")

  # dropout
  expect_false(result$is_dropout)
  expect_equal(result$dropout_code, 0L)

  # date
  expect_equal(result$date, as.Date("2012-07-30"))

  # HoNOS items (cleaned values)
  expect_equal(result$h1_aggression, 2L)
  expect_equal(result$h2_self_harm, 4L)
  expect_equal(result$h3_substance_use, 4L)
  expect_equal(result$h4_cognitive, 1L)
  expect_equal(result$h5_physical, 2L)
  expect_equal(result$h6_hallucination, 1L)
  expect_equal(result$h7_mood, 4L)
  expect_equal(result$h8_other_mental, 1L)
  expect_equal(result$h8_type, "a")
  expect_equal(result$h8_detail, "")
  expect_equal(result$h9_relationships, 1L)
  expect_equal(result$h10_daily_living, 3L)
  expect_equal(result$h11_living_conditions, 4L)
  expect_equal(result$h12_occupation, 4L)

  # raw values equal cleaned values when no 9s present
  expect_equal(result$h1_raw, result$h1_aggression)
  expect_equal(result$h8_raw, result$h8_other_mental)

  # total score
  expect_equal(
    result$honos_total,
    2L + 4L + 4L + 1L + 2L + 1L + 4L + 1L + 1L + 3L + 4L + 4L
  )
  expect_equal(result$honos_n_valid, 12L)
})

test_that("parse_ph_row parses a valid discharge row correctly", {
  # case 1, discharge
  r <- c(
    "PH", # 1
    "12345678", # 2
    "5050286", # 3
    "2", # 4  time_point (2=discharge)
    "0", # 5
    "", # 6
    "20120806", # 7
    "2", # 8  h1
    "2", # 9  h2
    "2", # 10 h3
    "1", # 11 h4
    "2", # 12 h5
    "1", # 13 h6
    "2", # 14 h7
    "3", # 15 h8
    "a", # 16 h8 type
    "", # 17 h8 detail
    "2", # 18 h9
    "4", # 19 h10
    "2", # 20 h11
    "1" # 21 h12
  )

  result <- parse_ph_row(r)

  expect_equal(result$time_point, 2L)
  expect_equal(result$time_point_label, "discharge")
  expect_equal(result$date, as.Date("2012-08-06"))
  expect_equal(
    result$honos_total,
    2L + 2L + 2L + 1L + 2L + 1L + 2L + 3L + 2L + 4L + 2L + 1L
  )
  expect_equal(result$honos_n_valid, 12L)
})

# -- parse_ph_row: dropout -----------------------------------------------------

test_that("parse_ph_row handles dropout correctly", {
  # case 2, discharge: dropout (within 24h of admission assessment)
  r <- c(
    "PH", # 1
    "12345678", # 2
    "5050297", # 3
    "2", # 4  time_point (2=discharge)
    "1", # 5  dropout_code (1=discharge within 24h)
    "", # 6  dropout_detail
    "20120226", # 7  date
    "", # 8-21 all empty (dropout)
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

  result <- parse_ph_row(r)

  expect_true(result$is_dropout)
  expect_equal(result$dropout_code, 1L)
  expect_equal(result$date, as.Date("2012-02-26"))

  # all HoNOS items should be NA for a dropout
  expect_true(is.na(result$h1_aggression))
  expect_true(is.na(result$h2_self_harm))
  expect_true(is.na(result$h8_other_mental))
  expect_true(is.na(result$h12_occupation))

  # raw values also NA for dropout
  expect_true(is.na(result$h1_raw))

  # total should be NA when all items missing
  expect_true(is.na(result$honos_total))
  expect_equal(result$honos_n_valid, 0L)
})

# -- parse_ph_row: 9 recoding --------------------------------------------------

test_that("parse_ph_row recodes 9 to NA in cleaned values but keeps in raw", {
  r <- c(
    "PH",
    "12345678",
    "5050286",
    "1",
    "0",
    "",
    "20120730",
    "9", # h1 = 9 (not known/not applicable)
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
  )

  result <- parse_ph_row(r)

  # cleaned value should be NA
  expect_true(is.na(result$h1_aggression))

  # raw value should be 9
  expect_equal(result$h1_raw, 9L)
})

test_that("honos_total excludes items with value 9 but counts remaining items", {
  r <- c(
    "PH",
    "12345678",
    "5050286",
    "1",
    "0",
    "",
    "20120730",
    "9", # h1 = 9, excluded from total
    "2", # h2
    "2", # h3
    "1", # h4
    "2", # h5
    "1", # h6
    "2", # h7
    "3", # h8
    "a",
    "",
    "2", # h9
    "4", # h10
    "2", # h11
    "1" # h12
  )

  result <- parse_ph_row(r)

  # total should sum 11 items (h1 excluded)
  expect_equal(result$honos_n_valid, 11L)
  expect_equal(
    result$honos_total,
    2L + 2L + 1L + 2L + 1L + 2L + 3L + 2L + 4L + 2L + 1L
  )
})

# -- parse_ph_row: too few fields ----------------------------------------------

test_that("parse_ph_row skips rows with too few fields and warns", {
  r <- c("PH", "12345678", "5050286") # only 3 fields
  expect_warning(
    result <- parse_ph_row(r),
    regexp = "at least 7 expected"
  )
  expect_null(result)
})

# -- parse_ph: multiple rows ---------------------------------------------------

test_that("parse_ph returns two rows per case (admission and discharge)", {
  r_admission <- c(
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
  )
  r_discharge <- c(
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
  )

  result <- parse_ph(list(r_admission, r_discharge))
  expect_equal(nrow(result), 2L)
  expect_equal(result$time_point, c(1L, 2L))
  expect_equal(unique(result$fid), "5050286")
})


test_that("parse_ph handles one admission and two discharge rows for same case", {
  # FID 5050286: one admission, two discharge records
  r_admission <- c(
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
  )
  r_discharge_1 <- c(
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
  )
  r_discharge_2 <- c(
    "PH",
    "12345678",
    "5050286",
    "2",
    "0",
    "",
    "20120808",
    "1",
    "1",
    "1",
    "1",
    "1",
    "1",
    "1",
    "1",
    "a",
    "",
    "1",
    "1",
    "1",
    "1"
  )

  result <- parse_ph(list(r_admission, r_discharge_1, r_discharge_2))

  # all three rows are returned -- parse_ph does not deduplicate,
  # the validation step will flag the duplicate discharge
  expect_equal(nrow(result), 3L)
  expect_equal(result$time_point, c(1L, 2L, 2L))
  expect_equal(unique(result$fid), "5050286")
})

# -- parse_ph: empty rows ------------------------------------------------------

test_that("parse_ph skips trailing empty rows silently", {
  r_valid <- c(
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
  )
  r_empty <- c("", "", "")

  expect_no_warning(
    result <- parse_ph(list(r_valid, r_empty))
  )
  expect_equal(nrow(result), 1L)
})

test_that("parse_ph skips empty rows between valid rows silently", {
  r_valid1 <- c(
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
  )
  r_empty <- c("", "", "")
  r_valid2 <- c(
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
  )

  expect_no_warning(
    result <- parse_ph(list(r_valid1, r_empty, r_valid2))
  )
  expect_equal(nrow(result), 2L)
})

# -- empty_ph ------------------------------------------------------------------

test_that("empty_ph returns a data frame with zero rows and correct types", {
  result <- empty_ph()

  expect_equal(nrow(result), 0L)

  expect_type(result$fid, "character")
  expect_type(result$facility, "character")
  expect_type(result$time_point, "integer")
  expect_type(result$time_point_label, "character")
  expect_type(result$is_dropout, "logical")
  expect_type(result$dropout_code, "integer")
  expect_type(result$dropout_detail, "character")
  expect_s3_class(result$date, "Date")
  expect_type(result$h1_aggression, "integer")
  expect_type(result$h1_raw, "integer")
  expect_type(result$honos_total, "integer")
  expect_type(result$honos_n_valid, "integer")
})
