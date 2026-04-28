# -- parse_pb_row: valid input -------------------------------------------------

test_that("parse_pb_row parses a valid admission row correctly", {
  r <- c(
    "PB", # 1  record type
    "12345678", # 2  facility
    "5050286", # 3  FID
    "1", # 4  time_point (1=admission)
    "0", # 5  dropout_code (0=no dropout)
    "", # 6  dropout_detail
    "20120730", # 7  date
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
  )

  result <- parse_pb_row(r)

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

  # spot check items
  expect_equal(result$b1, 2L)
  expect_equal(result$b2, 1L)
  expect_equal(result$b10, 0L)
  expect_equal(result$b17, 3L)
  expect_equal(result$b53, 2L)

  # all items should be non-NA for a valid row
  item_cols <- paste0("b", 1:53)
  expect_true(all(!is.na(unlist(result[item_cols]))))

  # total and n_valid
  expect_equal(result$bscl_n_valid, 53L)
  expect_equal(result$bscl_total, sum(as.integer(r[8:60])))
})

test_that("parse_pb_row parses a valid discharge row correctly", {
  r <- c(
    "PB", # 1
    "12345678", # 2
    "5050286", # 3
    "2", # 4  time_point (2=discharge)
    "0", # 5
    "", # 6
    "20120806", # 7
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
  )

  result <- parse_pb_row(r)

  expect_equal(result$time_point, 2L)
  expect_equal(result$time_point_label, "discharge")
  expect_equal(result$date, as.Date("2012-08-06"))
  expect_equal(result$bscl_n_valid, 53L)
})

# -- parse_pb_row: dropout -----------------------------------------------------

test_that("parse_pb_row handles dropout code 6 correctly", {
  # discharge within 24h of admission assessment
  r <- c(
    "PB", # 1
    "12345678", # 2
    "5050297", # 3
    "2", # 4  time_point (2=discharge)
    "6", # 5  dropout_code (6=discharge within 24h)
    "", # 6  dropout_detail
    "20120226", # 7  date
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

  result <- parse_pb_row(r)

  expect_true(result$is_dropout)
  expect_equal(result$dropout_code, 6L)
  expect_equal(result$date, as.Date("2012-02-26"))

  # all items should be NA for a dropout
  item_cols <- paste0("b", 1:53)
  expect_true(all(is.na(unlist(result[item_cols]))))

  # total should be NA, n_valid should be 0
  expect_true(is.na(result$bscl_total))
  expect_equal(result$bscl_n_valid, 0L)
})

test_that("parse_pb_row handles dropout code 8 with detail correctly", {
  # forensic exemption
  r <- c(
    "PB", # 1
    "12345678", # 2
    "5050292", # 3
    "1", # 4
    "8", # 5  dropout_code (8=other)
    "forensic psychiatry, exempt from BSCL since 01.07.2019", # 6
    "20121223", # 7
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

  result <- parse_pb_row(r)

  expect_true(result$is_dropout)
  expect_equal(result$dropout_code, 8L)
  expect_equal(
    result$dropout_detail,
    "forensic psychiatry, exempt from BSCL since 01.07.2019"
  )
})

test_that("parse_pb_row stores dropout_detail as NA when no dropout", {
  r <- c(
    "PB",
    "12345678",
    "5050286",
    "1",
    "0",
    "",
    "20120730",
    "2",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "0",
    "1",
    "0",
    "1",
    "2",
    "1",
    "2",
    "1",
    "2",
    "3",
    "2",
    "1",
    "0",
    "1",
    "2",
    "0",
    "1",
    "2",
    "1",
    "0",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "2",
    "3",
    "2",
    "1",
    "2",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "2"
  )

  result <- parse_pb_row(r)
  expect_true(is.na(result$dropout_detail))
})

# -- parse_pb_row: item scoring ------------------------------------------------

test_that("parse_pb_row returns NA for empty item fields", {
  # row with some empty item fields
  r <- c(
    "PB",
    "12345678",
    "5050286",
    "1",
    "0",
    "",
    "20120730",
    "2",
    "",
    "0",
    "1",
    "2",
    "1",
    "0",
    "0",
    "1",
    "0", # b2 empty
    "1",
    "2",
    "1",
    "2",
    "1",
    "2",
    "3",
    "2",
    "1",
    "0",
    "1",
    "2",
    "0",
    "1",
    "2",
    "1",
    "0",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "2",
    "3",
    "2",
    "1",
    "2",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "2"
  )

  result <- parse_pb_row(r)

  expect_true(is.na(result$b2))
  expect_equal(result$bscl_n_valid, 52L) # one item missing
})

test_that("bscl_total is NA when all items missing", {
  r <- c(
    "PB",
    "12345678",
    "5050286",
    "1",
    "0",
    "",
    "20120730",
    rep("", 53) # all items empty
  )

  result <- parse_pb_row(r)

  expect_true(is.na(result$bscl_total))
  expect_equal(result$bscl_n_valid, 0L)
})

# -- parse_pb_row: too few fields ----------------------------------------------

test_that("parse_pb_row skips rows with too few fields and warns", {
  r <- c("PB", "12345678", "5050286")
  expect_warning(
    result <- parse_pb_row(r),
    regexp = "at least 7 expected"
  )
  expect_null(result)
})

# -- parse_pb: multiple rows ---------------------------------------------------

test_that("parse_pb returns two rows per case (admission and discharge)", {
  r_admission <- c(
    "PB",
    "12345678",
    "5050286",
    "1",
    "0",
    "",
    "20120730",
    "2",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "0",
    "1",
    "0",
    "1",
    "2",
    "1",
    "2",
    "1",
    "2",
    "3",
    "2",
    "1",
    "0",
    "1",
    "2",
    "0",
    "1",
    "2",
    "1",
    "0",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "2",
    "3",
    "2",
    "1",
    "2",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "2"
  )
  r_discharge <- c(
    "PB",
    "12345678",
    "5050286",
    "2",
    "0",
    "",
    "20120806",
    "1",
    "0",
    "0",
    "0",
    "1",
    "0",
    "0",
    "0",
    "0",
    "0",
    "0",
    "1",
    "0",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "0",
    "0",
    "1",
    "0",
    "0",
    "1",
    "0",
    "0",
    "0",
    "0",
    "0",
    "1",
    "0",
    "0",
    "1",
    "2",
    "1",
    "0",
    "1",
    "0",
    "0",
    "0",
    "1",
    "0",
    "0",
    "0",
    "1",
    "0",
    "0",
    "0",
    "1",
    "0",
    "0",
    "1"
  )

  result <- parse_pb(list(r_admission, r_discharge))

  expect_equal(nrow(result), 2L)
  expect_equal(result$time_point, c(1L, 2L))
  expect_equal(unique(result$fid), "5050286")
})

test_that("parse_pb returns correct counts for mixed valid and dropout rows", {
  r_valid <- c(
    "PB",
    "12345678",
    "5050286",
    "1",
    "0",
    "",
    "20120730",
    "2",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "0",
    "1",
    "0",
    "1",
    "2",
    "1",
    "2",
    "1",
    "2",
    "3",
    "2",
    "1",
    "0",
    "1",
    "2",
    "0",
    "1",
    "2",
    "1",
    "0",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "2",
    "3",
    "2",
    "1",
    "2",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "2"
  )
  r_dropout <- c(
    "PB",
    "12345678",
    "5050297",
    "2",
    "6",
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

  result <- parse_pb(list(r_valid, r_dropout))

  expect_equal(nrow(result), 2L)
  expect_equal(result$is_dropout, c(FALSE, TRUE))
})

# -- parse_pb: empty rows ------------------------------------------------------

test_that("parse_pb skips trailing empty rows silently", {
  r_valid <- c(
    "PB",
    "12345678",
    "5050286",
    "1",
    "0",
    "",
    "20120730",
    "2",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "0",
    "1",
    "0",
    "1",
    "2",
    "1",
    "2",
    "1",
    "2",
    "3",
    "2",
    "1",
    "0",
    "1",
    "2",
    "0",
    "1",
    "2",
    "1",
    "0",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "2",
    "3",
    "2",
    "1",
    "2",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "2"
  )
  r_empty <- c("", "", "")

  expect_no_warning(
    result <- parse_pb(list(r_valid, r_empty))
  )
  expect_equal(nrow(result), 1L)
})

test_that("parse_pb skips empty rows between valid rows silently", {
  r_valid1 <- c(
    "PB",
    "12345678",
    "5050286",
    "1",
    "0",
    "",
    "20120730",
    "2",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "0",
    "1",
    "0",
    "1",
    "2",
    "1",
    "2",
    "1",
    "2",
    "3",
    "2",
    "1",
    "0",
    "1",
    "2",
    "0",
    "1",
    "2",
    "1",
    "0",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "2",
    "3",
    "2",
    "1",
    "2",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "2"
  )
  r_empty <- c("", "", "")
  r_valid2 <- c(
    "PB",
    "12345678",
    "5050286",
    "2",
    "0",
    "",
    "20120806",
    "1",
    "0",
    "0",
    "0",
    "1",
    "0",
    "0",
    "0",
    "0",
    "0",
    "0",
    "1",
    "0",
    "1",
    "0",
    "1",
    "2",
    "1",
    "0",
    "0",
    "0",
    "1",
    "0",
    "0",
    "1",
    "0",
    "0",
    "0",
    "0",
    "0",
    "1",
    "0",
    "0",
    "1",
    "2",
    "1",
    "0",
    "1",
    "0",
    "0",
    "0",
    "1",
    "0",
    "0",
    "0",
    "1",
    "0",
    "0",
    "0",
    "1",
    "0",
    "0",
    "1"
  )

  expect_no_warning(
    result <- parse_pb(list(r_valid1, r_empty, r_valid2))
  )
  expect_equal(nrow(result), 2L)
})

# -- empty_pb ------------------------------------------------------------------

test_that("empty_pb returns a data frame with zero rows and correct types", {
  result <- empty_pb()

  expect_equal(nrow(result), 0L)

  expect_type(result$fid, "character")
  expect_type(result$facility, "character")
  expect_type(result$time_point, "integer")
  expect_type(result$time_point_label, "character")
  expect_type(result$is_dropout, "logical")
  expect_type(result$dropout_code, "integer")
  expect_type(result$dropout_detail, "character")
  expect_s3_class(result$date, "Date")
  expect_type(result$bscl_total, "integer")
  expect_type(result$bscl_n_valid, "integer")

  # all 53 item columns present with correct type
  item_cols <- paste0("b", 1:53)
  expect_true(all(item_cols %in% names(result)))
  expect_true(all(vapply(result[item_cols], is.integer, logical(1))))
})
