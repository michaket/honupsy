# -- parse_date_anq ------------------------------------------------------------

test_that("parse_date_anq parses a valid date correctly", {
  expect_equal(parse_date_anq("20120730"), as.Date("2012-07-30"))
  expect_equal(parse_date_anq("20121231"), as.Date("2012-12-31"))
  expect_equal(parse_date_anq("20120101"), as.Date("2012-01-01"))
})

test_that("parse_date_anq only uses first 8 characters", {
  # datetime strings (10 chars) should be truncated to date
  expect_equal(parse_date_anq("2012073017"), as.Date("2012-07-30"))
})

test_that("parse_date_anq returns NA for missing or invalid input", {
  expect_true(is.na(parse_date_anq("")))
  expect_true(is.na(parse_date_anq("       "))) # only whitespace
  expect_true(is.na(parse_date_anq("1234"))) # too short
})

test_that("parse_date_anq strips whitespace", {
  expect_equal(parse_date_anq("  20120730  "), as.Date("2012-07-30"))
})

# -- parse_datetime_anq --------------------------------------------------------

test_that("parse_datetime_anq parses a valid datetime correctly", {
  result <- parse_datetime_anq("2012073017")
  expect_s3_class(result, "POSIXct")
  expect_equal(format(result, "%Y-%m-%d %H"), "2012-07-30 17")
})

test_that("parse_datetime_anq handles 8-character input without hour", {
  result <- parse_datetime_anq("20120730")
  expect_s3_class(result, "POSIXct")
  expect_equal(format(result, "%Y-%m-%d %H"), "2012-07-30 00")
})

test_that("parse_datetime_anq returns NA for missing or invalid input", {
  expect_true(is.na(parse_datetime_anq("")))
  expect_true(is.na(parse_datetime_anq("      ")))
  expect_true(is.na(parse_datetime_anq("1234")))
})

test_that("parse_datetime_anq strips whitespace", {
  result <- parse_datetime_anq("  2012073017  ")
  expect_s3_class(result, "POSIXct")
  expect_equal(format(result, "%Y-%m-%d %H"), "2012-07-30 17")
})

# -- parse_time_anq ------------------------------------------------------------

test_that("parse_time_anq returns correct minutes since midnight", {
  expect_equal(parse_time_anq("0000"), 0L)
  expect_equal(parse_time_anq("0001"), 1L)
  expect_equal(parse_time_anq("0100"), 60L)
  expect_equal(parse_time_anq("1330"), 810L) # 13*60 + 30
  expect_equal(parse_time_anq("2359"), 1439L) # 23*60 + 59
  expect_equal(parse_time_anq("2400"), 1440L) # ANQ allows 2400
  expect_equal(parse_time_anq("830"), 510L) # missing leading zero -> padded to "0830"
  expect_equal(parse_time_anq("100"), 60L) # missing leading zero -> padded to "0100"
})

test_that("parse_time_anq returns NA for missing or invalid input", {
  expect_true(is.na(parse_time_anq("")))
  expect_true(is.na(parse_time_anq("     ")))
  expect_true(is.na(parse_time_anq("08300"))) # too long
})

test_that("parse_time_anq strips whitespace", {
  expect_equal(parse_time_anq("  0830  "), 510L)
})

# -- parse_int -----------------------------------------------------------------

test_that("parse_int parses valid integers", {
  expect_equal(parse_int("1"), 1L)
  expect_equal(parse_int("0"), 0L)
  expect_equal(parse_int("42"), 42L)
  expect_equal(parse_int("9"), 9L)
})

test_that("parse_int returns NA for missing input", {
  expect_true(is.na(parse_int("")))
  expect_true(is.na(parse_int("   ")))
})

test_that("parse_int returns NA for non-numeric input", {
  expect_true(is.na(parse_int("abc")))
  expect_true(is.na(parse_int("1.5")))
  expect_true(is.na(parse_int("1,500")))
  expect_true(is.na(parse_int("1'500")))
})

test_that("parse_int strips whitespace", {
  expect_equal(parse_int("  3  "), 3L)
})

test_that("parse_int returns integer type", {
  expect_type(parse_int("1"), "integer")
})

# -- coercive_measure_label ----------------------------------------------------

test_that("coercive_measure_label returns correct labels for all known codes", {
  expect_equal(coercive_measure_label(1), "Psychiatric isolation")
  expect_equal(coercive_measure_label(2), "Physical restraint")
  expect_equal(coercive_measure_label(3), "Involuntary medication oral")
  expect_equal(coercive_measure_label(4), "Involuntary medication injection")
  expect_equal(coercive_measure_label(5), "Safety measure chair")
  expect_equal(coercive_measure_label(7), "Safety measure bed")
  expect_equal(coercive_measure_label(10), "Manual restraint")
  expect_equal(
    coercive_measure_label(11),
    "Isolation for infectious/somatic reasons"
  )
})

test_that("coercive_measure_label returns NA for unknown codes", {
  expect_true(is.na(coercive_measure_label(6)))
  expect_true(is.na(coercive_measure_label(99)))
})


# time_point_label --------------------------------------------------------

test_that("time_point_label returns correct labels", {
  expect_equal(time_point_label(1), "admission")
  expect_equal(time_point_label(2), "discharge")
  expect_equal(time_point_label(3), "other")
  expect_true(is.na(time_point_label(9)))
})
