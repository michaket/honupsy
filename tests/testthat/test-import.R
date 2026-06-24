# -- import_anq: input validation ----------------------------------------------

test_that("import_anq errors on non-character path", {
  expect_error(import_anq(123), regexp = "character vector")
})

test_that("import_anq errors on empty path", {
  expect_error(import_anq(character(0)), regexp = "character vector")
})

test_that("import_anq errors when file does not exist", {
  expect_error(
    import_anq("nonexistent_file.txt"),
    regexp = "not found"
  )
})

# -- import_anq: TXT import ----------------------------------------------------

test_that("import_anq imports a mixed TXT file correctly", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)

  expect_type(data, "list")
  expect_true(all(c("mb", "mp", "ph", "pb", "fm") %in% names(data)))

  expect_equal(nrow(data$mb), 3L)
  expect_equal(nrow(data$mp), 3L)
  expect_equal(nrow(data$ph), 6L)
  expect_equal(nrow(data$pb), 6L)
  expect_equal(nrow(data$fm), 4L)
})

test_that("import_anq imports a semicolon-delimited TXT file correctly", {
  data <- import_anq(example_path("example_semicolon.txt"), validate = FALSE)

  expect_equal(nrow(data$mb), 3L)
  expect_equal(nrow(data$mp), 3L)
})

test_that("import_anq imports separate TXT files correctly", {
  data <- import_anq(
    c(
      MB = example_path("example_mb.txt"),
      MP = example_path("example_mp.txt"),
      PH = example_path("example_ph.txt"),
      PB = example_path("example_pb.txt"),
      FM = example_path("example_fm.txt")
    ),
    validate = FALSE
  )

  expect_equal(nrow(data$mb), 3L)
  expect_equal(nrow(data$mp), 3L)
  expect_equal(nrow(data$ph), 6L)
  expect_equal(nrow(data$pb), 6L)
  expect_equal(nrow(data$fm), 4L)
})

test_that("import_anq imports partial separate files correctly", {
  data <- import_anq(
    c(
      MB = example_path("example_mb.txt"),
      FM = example_path("example_fm.txt")
    ),
    validate = FALSE
  )

  expect_equal(nrow(data$mb), 3L)
  expect_equal(nrow(data$fm), 4L)

  # missing datasets return empty data frames
  expect_equal(nrow(data$mp), 0L)
  expect_equal(nrow(data$ph), 0L)
  expect_equal(nrow(data$pb), 0L)
})

# -- import_anq: XLSX import ---------------------------------------------------

test_that("import_anq imports a simple XLSX file correctly", {
  data <- import_anq(example_path("example.xlsx"), validate = FALSE)

  expect_equal(nrow(data$mb), 3L)
  expect_equal(nrow(data$mp), 3L)
  expect_equal(nrow(data$ph), 6L)
  expect_equal(nrow(data$pb), 6L)
  expect_equal(nrow(data$fm), 4L)
})

test_that("import_anq imports the ANQ template XLSX correctly", {
  data <- import_anq(
    example_path("example_anq_template.xlsx"),
    validate = FALSE
  )

  expect_equal(nrow(data$mb), 3L)
  expect_equal(nrow(data$mp), 3L)
  expect_equal(nrow(data$ph), 6L)
  expect_equal(nrow(data$fm), 4L)
})

# -- import_anq: TXT and XLSX produce same results ----------------------------

test_that("import_anq produces identical results for TXT and XLSX", {
  data_txt <- import_anq(example_path("example.txt"), validate = FALSE)
  data_xlsx <- import_anq(example_path("example.xlsx"), validate = FALSE)

  # same FIDs in MB
  expect_equal(sort(data_txt$mb$fid), sort(data_xlsx$mb$fid))

  # same FIDs in FM
  expect_equal(sort(data_txt$fm$fid), sort(data_xlsx$fm$fid))

  # same main diagnoses
  expect_equal(
    sort(data_txt$mb$main_diagnosis),
    sort(data_xlsx$mb$main_diagnosis)
  )

  # same HoNOS totals
  expect_equal(
    sort(data_txt$ph$honos_total),
    sort(data_xlsx$ph$honos_total)
  )
})

# -- import_anq: validate parameter -------------------------------------------

test_that("import_anq returns validation messages when validate = TRUE", {
  data <- import_anq(example_path("example.txt"), validate = TRUE)
  expect_true(".validation" %in% names(data))
  expect_type(data$.validation, "list")
})

test_that("import_anq does not return validation messages when validate = FALSE", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  expect_false(".validation" %in% names(data))
})

# -- import_anq: output structure ---------------------------------------------

test_that("import_anq returns data frames with correct column types for MB", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)

  expect_type(data$mb$fid, "character")
  expect_type(data$mb$sex, "integer")
  expect_type(data$mb$age_admission, "integer")
  expect_s3_class(data$mb$admission, "POSIXct")
  expect_s3_class(data$mb$discharge, "POSIXct")
  expect_type(data$mb$main_diagnosis, "character")
})

test_that("import_anq returns data frames with correct column types for PH", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)

  expect_type(data$ph$fid, "character")
  expect_type(data$ph$time_point, "integer")
  expect_type(data$ph$is_dropout, "logical")
  expect_s3_class(data$ph$date, "Date")
  expect_type(data$ph$h1_aggression, "integer")
  expect_type(data$ph$honos_total, "integer")
  expect_type(data$ph$honos_n_valid, "integer")
})

test_that("import_anq returns data frames with correct column types for PB", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)

  expect_type(data$pb$fid, "character")
  expect_type(data$pb$time_point, "integer")
  expect_type(data$pb$is_dropout, "logical")
  expect_s3_class(data$pb$date, "Date")
  expect_type(data$pb$b1, "integer")
  expect_type(data$pb$bscl_total, "integer")
  expect_type(data$pb$bscl_n_valid, "integer")
})

test_that("import_anq returns data frames with correct column types for FM", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)

  expect_type(data$fm$fid, "character")
  expect_type(data$fm$measure_type, "integer")
  expect_type(data$fm$is_coercive_medication, "logical")
  expect_s3_class(data$fm$start_date, "Date")
  expect_type(data$fm$duration_min, "integer")
})

# -- import_anq: specific values -----------------------------------------------

test_that("import_anq correctly imports case 1 from example data", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)

  case1_mb <- data$mb[data$mb$fid == "5050286", ]

  expect_equal(case1_mb$sex, 2L) # female
  expect_equal(case1_mb$age_admission, 61L)
  expect_equal(case1_mb$nationality, "CHE")
  expect_equal(case1_mb$main_diagnosis, "F200")
  expect_equal(case1_mb$main_cost_centre, "M500")

  case1_ph_admission <- data$ph[
    data$ph$fid == "5050286" & data$ph$time_point == 1L,
  ]

  expect_equal(case1_ph_admission$h1_aggression, 2L)
  expect_equal(case1_ph_admission$honos_n_valid, 12L)
  expect_false(case1_ph_admission$is_dropout)
})

test_that("import_anq correctly handles case 2 dropout in PH", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)

  case2_ph_discharge <- data$ph[
    data$ph$fid == "5050297" & data$ph$time_point == 2L,
  ]

  expect_true(case2_ph_discharge$is_dropout)
  expect_equal(case2_ph_discharge$dropout_code, 1L)
  expect_true(is.na(case2_ph_discharge$h1_aggression))
  expect_true(is.na(case2_ph_discharge$honos_total))
})

test_that("import_anq correctly imports FM measures", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)

  fm_case1 <- data$fm[data$fm$fid == "5050286", ]
  expect_equal(nrow(fm_case1), 2L)
  expect_equal(fm_case1$measure_type, c(1L, 2L))

  # psychiatric isolation: 1330 to 1700 = 210 minutes
  expect_equal(fm_case1$duration_min[1], 210L)

  # physical restraint: 1100 to next day 0900 = 1320 minutes
  expect_equal(fm_case1$duration_min[2], 1320L)
})

# -- import_anq: unknown record types -----------------------------------------
# Covers: setdiff(names(raw), known) -> message(... ignored ...)
# example_unknown.txt is example.txt plus two "XX" rows (an unknown type).

test_that("import_anq messages about ignored unknown record types", {
  expect_message(
    import_anq(example_path("example_unknown.txt"), validate = FALSE),
    regexp = "ignored"
  )
})

test_that("import_anq still parses known types when unknown ones are present", {
  data <- suppressMessages(
    import_anq(example_path("example_unknown.txt"), validate = FALSE)
  )
  expect_equal(nrow(data$mb), 3L)
  # the unknown type does not leak into the result
  expect_true(all(c("mb", "mp", "ph", "pb", "fm") %in% names(data)))
  expect_false("xx" %in% names(data))
})

# -- import_anq: tab-delimited TXT --------------------------------------------
# The docstring promises TAB / semicolon / pipe. example.txt is pipe-delimited
# and example_semicolon.txt covers semicolon, so TAB is the untested delimiter.
# example_tab.txt is example.txt with pipes replaced by tabs.

test_that("import_anq imports a tab-delimited TXT file correctly", {
  data <- import_anq(example_path("example_tab.txt"), validate = FALSE)
  expect_equal(nrow(data$mb), 3L)
  expect_equal(nrow(data$mp), 3L)
  expect_equal(nrow(data$ph), 6L)
})

# -- import_anq: import summary message ---------------------------------------
# Covers import_summary(), incl. the dropout / time_point counting arithmetic.
# example.txt already contains a dropout (case 5050297), so the counts are
# exercised, not just printed.

test_that("import_anq prints a summary covering all record types", {
  expect_message(
    import_anq(example_path("example.txt"), validate = FALSE),
    regexp = "ANQ import completed"
  )
  expect_message(
    import_anq(example_path("example.txt"), validate = FALSE),
    regexp = "MB: 3 cases"
  )
  # dropout arithmetic: PH line reports the known dropout
  expect_message(
    import_anq(example_path("example.txt"), validate = FALSE),
    regexp = "PH: 6 assessments.*dropout"
  )
})

# -- import_anq: empty frames have correct structure --------------------------
# Covers the else empty_*() arms beyond nrow == 0: the docstring promises
# "empty data.frames with the correct column types". Compare columns of an
# absent type against the same type when populated.

test_that("absent record types return empty frames with the right columns", {
  full <- import_anq(example_path("example.txt"), validate = FALSE)
  partial <- import_anq(
    c(MB = example_path("example_mb.txt")),
    validate = FALSE
  )

  for (type in c("mp", "ph", "pb", "fm")) {
    expect_equal(nrow(partial[[type]]), 0L, info = type)
    expect_identical(
      names(partial[[type]]),
      names(full[[type]]),
      info = type
    )
  }
})

# -- import_anq: single named path routes through the multi reader ------------
# c(MB = x) is length 1 but named, so the dispatch picks read_anq_multi,
# not read_anq_raw. This arm is otherwise only hit by length >= 2 inputs.

test_that("import_anq handles a single named path", {
  data <- import_anq(
    c(MB = example_path("example_mb.txt")),
    validate = FALSE
  )
  expect_equal(nrow(data$mb), 3L)
  expect_equal(nrow(data$mp), 0L)
})

