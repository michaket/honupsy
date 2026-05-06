# -- import_unit_assignments: input validation ---------------------------------

test_that("import_unit_assignments errors on invalid input type", {
  expect_error(
    import_unit_assignments(123),
    regexp = "file path or a data frame"
  )
})

test_that("import_unit_assignments errors when file does not exist", {
  expect_error(
    import_unit_assignments("nonexistent.csv"),
    regexp = "not found"
  )
})

test_that("import_unit_assignments errors when col_fid not found", {
  df <- data.frame(wrong_col = "1001", unit = "A1", stringsAsFactors = FALSE)
  expect_error(
    import_unit_assignments(df, col_fid = "fid"),
    regexp = "not found"
  )
})

test_that("import_unit_assignments errors when col_unit not found", {
  df <- data.frame(fid = "1001", wrong_col = "A1", stringsAsFactors = FALSE)
  expect_error(
    import_unit_assignments(df, col_unit = "unit"),
    regexp = "not found"
  )
})

# -- import_unit_assignments: from data frame ----------------------------------

test_that("import_unit_assignments reads a data frame correctly", {
  df <- data.frame(
    fid = c("5050286", "5050297", "5050292"),
    unit = c("A1", "B2", "A1"),
    stringsAsFactors = FALSE
  )

  result <- import_unit_assignments(df)

  expect_equal(nrow(result), 3L)
  expect_equal(names(result), c("fid", "unit"))
  expect_equal(result$fid, c("5050286", "5050297", "5050292"))
  expect_equal(result$unit, c("A1", "B2", "A1"))
})

test_that("import_unit_assignments handles non-standard column names", {
  df <- data.frame(
    Fallnummer = c("5050286", "5050297"),
    Station = c("A1", "B2"),
    stringsAsFactors = FALSE
  )

  result <- import_unit_assignments(
    df,
    col_fid = "Fallnummer",
    col_unit = "Station"
  )

  expect_equal(names(result), c("fid", "unit"))
  expect_equal(result$fid, c("5050286", "5050297"))
})

test_that("import_unit_assignments converts all values to character", {
  df <- data.frame(
    fid = c(5050286L, 5050297L),
    unit = c(1L, 2L),
    stringsAsFactors = FALSE
  )

  result <- import_unit_assignments(df)

  expect_type(result$fid, "character")
  expect_type(result$unit, "character")
})

# -- import_unit_assignments: data cleaning ------------------------------------

test_that("import_unit_assignments removes rows with missing FID and warns", {
  df <- data.frame(
    fid = c("5050286", "", "5050292"),
    unit = c("A1", "B2", "A1"),
    stringsAsFactors = FALSE
  )

  expect_warning(
    result <- import_unit_assignments(df),
    regexp = "missing FID"
  )
  expect_equal(nrow(result), 2L)
})

test_that("import_unit_assignments removes rows with missing unit and warns", {
  df <- data.frame(
    fid = c("5050286", "5050297", "5050292"),
    unit = c("A1", "", "A1"),
    stringsAsFactors = FALSE
  )

  expect_warning(
    result <- import_unit_assignments(df),
    regexp = "missing unit"
  )
  expect_equal(nrow(result), 2L)
})

test_that("import_unit_assignments keeps first occurrence of duplicate FIDs and warns", {
  df <- data.frame(
    fid = c("5050286", "5050286", "5050292"),
    unit = c("A1", "B2", "A1"),
    stringsAsFactors = FALSE
  )

  expect_warning(
    result <- import_unit_assignments(df),
    regexp = "duplicate FID"
  )
  expect_equal(nrow(result), 2L)
  # first occurrence kept
  expect_equal(result$unit[result$fid == "5050286"], "A1")
})

# -- import_unit_assignments: from files ---------------------------------------

test_that("import_unit_assignments reads a CSV file correctly", {
  path <- example_path("example_units.csv")
  result <- import_unit_assignments(path)

  expect_equal(nrow(result), 3L)
  expect_equal(names(result), c("fid", "unit"))
  expect_type(result$fid, "character")
  expect_type(result$unit, "character")
})

test_that("import_unit_assignments reads a TXT file correctly", {
  path <- example_path("example_units.txt")
  result <- import_unit_assignments(path)

  expect_equal(nrow(result), 3L)
  expect_equal(names(result), c("fid", "unit"))
})

test_that("import_unit_assignments reads an XLSX file with non-standard columns", {
  path <- example_path("example_units.xlsx")
  result <- import_unit_assignments(
    path,
    col_fid = "Fallnummer",
    col_unit = "Station"
  )

  expect_equal(nrow(result), 3L)
  expect_equal(names(result), c("fid", "unit"))
  expect_equal(sort(result$fid), c("5050286", "5050292", "5050297"))
})

test_that("import_unit_assignments CSV and XLSX produce same result", {
  result_csv <- import_unit_assignments(example_path("example_units.csv"))
  result_xlsx <- import_unit_assignments(
    example_path("example_units.xlsx"),
    col_fid = "Fallnummer",
    col_unit = "Station"
  )

  expect_equal(
    result_csv[order(result_csv$fid), ],
    result_xlsx[order(result_xlsx$fid), ],
    ignore_attr = TRUE
  )
})

# -- assign_units --------------------------------------------------------------

test_that("assign_units errors on invalid data argument", {
  units <- data.frame(fid = "1001", unit = "A1", stringsAsFactors = FALSE)
  expect_error(
    assign_units(list(x = 1), units),
    regexp = "import_anq"
  )
})

test_that("assign_units errors on invalid units argument", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  expect_error(
    assign_units(data, data.frame(x = 1)),
    regexp = "import_unit_assignments"
  )
})

test_that("assign_units adds unit column to all datasets", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  units <- import_unit_assignments(example_path("example_units.csv"))

  result <- assign_units(data, units)

  expect_true("unit" %in% names(result$mb))
  expect_true("unit" %in% names(result$mp))
  expect_true("unit" %in% names(result$ph))
  expect_true("unit" %in% names(result$pb))
  expect_true("unit" %in% names(result$fm))
})

test_that("assign_units joins unit values correctly", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  units <- import_unit_assignments(example_path("example_units.csv"))

  result <- assign_units(data, units)

  # case 5050286 should be on unit A1
  expect_equal(
    unique(result$mb$unit[result$mb$fid == "5050286"]),
    "A1"
  )

  # case 5050297 should be on unit B2
  expect_equal(
    unique(result$mb$unit[result$mb$fid == "5050297"]),
    "B2"
  )
})

test_that("assign_units sets unit to NA for unmatched FIDs and warns", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  # remove one FID from units
  units <- import_unit_assignments(example_path("example_units.csv"))
  units <- units[units$fid != "5050292", ]

  expect_warning(
    result <- assign_units(data, units),
    regexp = "no unit assignment"
  )

  expect_true(is.na(
    result$mb$unit[result$mb$fid == "5050292"]
  ))
})

test_that("assign_units warns about unit FIDs not in ANQ data", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  units <- import_unit_assignments(example_path("example_units.csv"))
  # add an extra FID not in ANQ data
  units <- rbind(
    units,
    data.frame(fid = "9999999", unit = "C3", stringsAsFactors = FALSE)
  )

  expect_warning(
    assign_units(data, units),
    regexp = "not found in ANQ data"
  )
})

test_that("assign_units preserves all rows in each dataset", {
  data <- import_anq(example_path("example.txt"), validate = FALSE)
  units <- import_unit_assignments(example_path("example_units.csv"))

  n_mb_before <- nrow(data$mb)
  n_ph_before <- nrow(data$ph)
  n_fm_before <- nrow(data$fm)

  result <- assign_units(data, units)

  expect_equal(nrow(result$mb), n_mb_before)
  expect_equal(nrow(result$ph), n_ph_before)
  expect_equal(nrow(result$fm), n_fm_before)
})
