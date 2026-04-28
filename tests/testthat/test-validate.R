# -- validate_structure --------------------------------------------------------

test_that("validate_structure passes when all required datasets present", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  msgs <- validate_structure(data)
  expect_equal(length(msgs), 0L)
})

test_that("validate_structure warns when MB is missing", {
  data <- list(mp = empty_mp(), ph = empty_ph(), fm = empty_fm())
  msgs <- validate_structure(data)
  expect_true(any(grepl("MB", msgs)))
})

test_that("validate_structure warns when MP is missing", {
  data <- list(mb = empty_mb(), ph = empty_ph(), fm = empty_fm())
  msgs <- validate_structure(data)
  expect_true(any(grepl("MP", msgs)))
})

test_that("validate_structure warns when PH is missing", {
  data <- list(mb = empty_mb(), mp = empty_mp(), fm = empty_fm())
  msgs <- validate_structure(data)
  expect_true(any(grepl("PH", msgs)))
})

test_that("validate_structure does not warn when PB is missing", {
  data <- list(
    mb = empty_mb(),
    mp = empty_mp(),
    ph = empty_ph(),
    fm = empty_fm()
  )
  msgs <- validate_structure(data)
  expect_false(any(grepl("PB", msgs)))
})

test_that("validate_structure does not warn when FM is missing", {
  data <- list(mb = empty_mb(), mp = empty_mp(), ph = empty_ph())
  msgs <- validate_structure(data)
  expect_false(any(grepl("FM", msgs)))
})

test_that("validate_structure warns when MB and MP have different case counts", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  # remove one MB row
  data$mb <- data$mb[-1, ]
  msgs <- validate_structure(data)
  expect_true(any(grepl("different numbers of cases", msgs)))
})

test_that("validate_structure warns when PH has more rows than expected", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  # duplicate all PH rows to exceed 2 * nrow(mb)
  data$ph <- rbind(data$ph, data$ph, data$ph)
  msgs <- validate_structure(data)
  expect_true(any(grepl("more rows than expected", msgs)))
})

# -- validate_fid --------------------------------------------------------------

test_that("validate_fid passes when all FIDs are consistent", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  msgs <- validate_fid(data)
  expect_equal(length(msgs), 0L)
})

test_that("validate_fid warns when MB has duplicate FIDs", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  # duplicate first MB row
  data$mb <- rbind(data$mb, data$mb[1, ])
  msgs <- validate_fid(data)
  expect_true(any(grepl("duplicate FIDs in MB", msgs)))
})

test_that("validate_fid warns when MB has rows without FID", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  data$mb$fid[1] <- ""
  msgs <- validate_fid(data)
  expect_true(any(grepl("without FID", msgs)))
})

test_that("validate_fid warns when MP has FIDs not in MB", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  data$mp$fid[1] <- "UNKNOWN_FID"
  msgs <- validate_fid(data)
  expect_true(any(grepl("FIDs in MP without a corresponding MB row", msgs)))
})

test_that("validate_fid warns when PH has FIDs not in MB", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  data$ph$fid[1] <- "UNKNOWN_FID"
  msgs <- validate_fid(data)
  expect_true(any(grepl("FIDs in PH without a corresponding MB row", msgs)))
})

test_that("validate_fid warns when FM has FIDs not in MB", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  data$fm$fid[1] <- "UNKNOWN_FID"
  msgs <- validate_fid(data)
  expect_true(any(grepl("FIDs in FM without a corresponding MB row", msgs)))
})

test_that("validate_fid warns when PB has FIDs not in MB", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  data$pb$fid[1] <- "UNKNOWN_FID"
  msgs <- validate_fid(data)
  expect_true(any(grepl("FIDs in PB without a corresponding MB row", msgs)))
})

test_that("validate_fid does not warn when PB is absent", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  data$pb <- NULL
  msgs <- validate_fid(data)
  expect_false(any(grepl("PB", msgs)))
})

test_that("validate_fid does not warn when FM is absent", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  data$fm <- NULL
  msgs <- validate_fid(data)
  expect_false(any(grepl("FM", msgs)))
})

test_that("validate_fid does not error when MP is absent", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  data$mp <- NULL
  expect_no_error(validate_fid(data))
})

test_that("validate_fid does not error when PH is absent", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  data$ph <- NULL
  expect_no_error(validate_fid(data))
})

# -- validate_mb ---------------------------------------------------------------

test_that("validate_mb passes on valid data", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  msgs <- validate_mb(data$mb)
  expect_equal(length(msgs), 0L)
})

test_that("validate_mb warns when flag_psychiatry != 1", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  data$mb$flag_psychiatry[1] <- 0L
  msgs <- validate_mb(data$mb)
  expect_true(any(grepl("flag_psychiatry", msgs)))
})

test_that("validate_mb warns when main_cost_centre != M500", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  data$mb$main_cost_centre[1] <- "X999"
  msgs <- validate_mb(data$mb)
  expect_true(any(grepl("main_cost_centre", msgs)))
})

test_that("validate_mb warns when age < 18", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  data$mb$age_admission[1] <- 15L
  msgs <- validate_mb(data$mb)
  expect_true(any(grepl("age < 18", msgs)))
})

test_that("validate_mb warns when discharge is before admission", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  data$mb$discharge[1] <- data$mb$admission[1] - 3600
  msgs <- validate_mb(data$mb)
  expect_true(any(grepl("discharge date before admission date", msgs)))
})

# -- validate_ph ---------------------------------------------------------------

test_that("validate_ph passes on valid data", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  msgs <- validate_ph(data$ph)
  expect_equal(length(msgs), 0L)
})

test_that("validate_ph warns on duplicate admission assessments", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  # duplicate an admission row
  admission_row <- data$ph[
    data$ph$time_point == 1L &
      data$ph$fid == "5050286",
  ]
  data$ph <- rbind(data$ph, admission_row)
  msgs <- validate_ph(data$ph)
  expect_true(any(grepl("more than one admission assessment", msgs)))
})

test_that("validate_ph warns on duplicate discharge assessments", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  discharge_row <- data$ph[
    data$ph$time_point == 2L &
      data$ph$fid == "5050286",
  ]
  data$ph <- rbind(data$ph, discharge_row)
  msgs <- validate_ph(data$ph)
  expect_true(any(grepl("more than one discharge assessment", msgs)))
})

test_that("validate_ph warns on invalid HoNOS raw item values", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  data$ph$h1_raw[1] <- 5L # invalid: outside 0-4 or 9
  msgs <- validate_ph(data$ph)
  expect_true(any(grepl("invalid values in h1_raw", msgs)))
})

# -- validate_pb ---------------------------------------------------------------

test_that("validate_pb passes on valid data", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  msgs <- validate_pb(data$pb)
  expect_equal(length(msgs), 0L)
})

test_that("validate_pb passes on empty data frame", {
  msgs <- validate_pb(empty_pb())
  expect_equal(length(msgs), 0L)
})

test_that("validate_pb warns on duplicate admission assessments", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  admission_row <- data$pb[
    data$pb$time_point == 1L &
      !data$pb$is_dropout &
      data$pb$fid == "5050286",
  ]
  data$pb <- rbind(data$pb, admission_row)
  msgs <- validate_pb(data$pb)
  expect_true(any(grepl("more than one admission assessment", msgs)))
})

test_that("validate_pb warns on invalid BSCL item values", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  data$pb$b1[1] <- 5L # invalid: outside 0-4
  msgs <- validate_pb(data$pb)
  expect_true(any(grepl("invalid values in b1", msgs)))
})

# -- validate_fm ---------------------------------------------------------------

test_that("validate_fm passes on valid data", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  msgs <- validate_fm(data$fm)
  expect_equal(length(msgs), 0L)
})

test_that("validate_fm passes on empty data frame", {
  msgs <- validate_fm(empty_fm())
  expect_equal(length(msgs), 0L)
})

test_that("validate_fm warns on unknown measure type", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  data$fm$measure_type[1] <- 99L
  msgs <- validate_fm(data$fm)
  expect_true(any(grepl("unknown measure type", msgs)))
})

test_that("validate_fm warns on overlapping non-coercive measures", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  # duplicate first FM row to create overlap
  data$fm <- rbind(data$fm, data$fm[1, ])
  msgs <- validate_fm(data$fm)
  expect_true(any(grepl("overlapping", msgs)))
})
