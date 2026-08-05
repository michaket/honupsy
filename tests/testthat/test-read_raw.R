# -- detect_delimiter ----------------------------------------------------------

test_that("detect_delimiter identifies pipe delimiter", {
  lines <- c("MB|12345678|1234C|ZH", "MP|12345678|1234C|ZH")
  expect_equal(detect_delimiter(lines), "pipe")
})

test_that("detect_delimiter identifies semicolon delimiter", {
  lines <- c("MB;12345678;1234C;ZH", "MP;12345678;1234C;ZH")
  expect_equal(detect_delimiter(lines), "semicolon")
})

test_that("detect_delimiter identifies tab delimiter", {
  lines <- c("MB\t12345678\t1234C\tZH", "MP\t12345678\t1234C\tZH")
  expect_equal(detect_delimiter(lines), "tab")
})

test_that("detect_delimiter uses only the first non-empty line", {
  lines <- c("", "MB|12345678|1234C|ZH", "MP|12345678|1234C|ZH")
  expect_equal(detect_delimiter(lines), "pipe")
})

# -- read_txt_raw --------------------------------------------------------------

test_that("read_txt_raw reads a pipe-delimited file and splits by record type", {
  path <- example_path("example.txt")
  result <- read_txt_raw(path)

  expect_type(result, "list")
  expect_true("MB" %in% names(result))
  expect_true("MP" %in% names(result))
  expect_true("PH" %in% names(result))
  expect_true("PB" %in% names(result))
  expect_true("FM" %in% names(result))
})

test_that("read_txt_raw returns correct number of rows per record type", {
  path <- example_path("example.txt")
  result <- read_txt_raw(path)

  expect_equal(length(result$MB), 3L)
  expect_equal(length(result$MP), 3L)
  expect_equal(length(result$PH), 6L)
  expect_equal(length(result$PB), 6L)
  expect_equal(length(result$FM), 4L)
})

test_that("read_txt_raw reads a semicolon-delimited file correctly", {
  path <- example_path("example_semicolon.txt")
  result <- read_txt_raw(path)

  expect_true("MB" %in% names(result))
  expect_equal(length(result$MB), 3L)
})

test_that("read_txt_raw errors on empty file", {
  tmp <- tempfile(fileext = ".txt")
  writeLines(character(0), tmp)
  expect_error(read_txt_raw(tmp), regexp = "empty")
  unlink(tmp)
})

# -- read_xlsx_raw -------------------------------------------------------------

test_that("read_xlsx_raw reads a simple XLSX file and splits by worksheet", {
  path <- example_path("example.xlsx")
  result <- read_xlsx_raw(path)

  expect_type(result, "list")
  expect_true("MB" %in% names(result))
  expect_true("MP" %in% names(result))
  expect_true("PH" %in% names(result))
  expect_true("PB" %in% names(result))
  expect_true("FM" %in% names(result))
})

test_that("read_xlsx_raw returns correct number of rows per worksheet", {
  path <- example_path("example.xlsx")
  result <- read_xlsx_raw(path)

  expect_equal(length(result$MB), 3L)
  expect_equal(length(result$MP), 3L)
  expect_equal(length(result$PH), 6L)
  expect_equal(length(result$PB), 6L)
  expect_equal(length(result$FM), 4L)
})

test_that("read_xlsx_raw errors when no known worksheets found", {
  skip_if_not_installed("writexl")
  tmp <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(unknown = data.frame(x = 1)), tmp)
  expect_error(read_xlsx_raw(tmp), regexp = "No known worksheets")
  unlink(tmp)
})

# -- read_anq_raw --------------------------------------------------------------

test_that("read_anq_raw detects TXT format from file extension", {
  path <- example_path("example.txt")
  result <- read_anq_raw(path)
  expect_true("MB" %in% names(result))
})

test_that("read_anq_raw detects XLSX format from file extension", {
  path <- example_path("example.xlsx")
  result <- read_anq_raw(path)
  expect_true("MB" %in% names(result))
})

test_that("read_anq_raw detects ANQ template XLSX automatically", {
  path <- example_path("example_anq_template.xlsx")
  result <- read_anq_raw(path)

  expect_true("MB" %in% names(result))
  expect_equal(length(result$MB), 3L)
})

test_that("read_anq_raw produces identical results for TXT and simple XLSX", {
  path_txt <- example_path("example.txt")
  path_xlsx <- example_path("example.xlsx")

  result_txt <- read_anq_raw(path_txt)
  result_xlsx <- read_anq_raw(path_xlsx)

  # same number of rows per record type
  expect_equal(length(result_txt$MB), length(result_xlsx$MB))
  expect_equal(length(result_txt$PH), length(result_xlsx$PH))
  expect_equal(length(result_txt$FM), length(result_xlsx$FM))

  # FIDs match
  fid_txt <- vapply(result_txt$MB, `[[`, character(1), 51)
  fid_xlsx <- vapply(result_xlsx$MB, `[[`, character(1), 51)
  expect_equal(fid_txt, fid_xlsx)
})

# -- read_anq_multi ------------------------------------------------------------

test_that("read_anq_multi reads separate files and merges by record type", {
  paths <- c(
    MB = example_path("example_mb.txt"),
    FM = example_path("example_fm.txt")
  )
  result <- read_anq_multi(paths)

  expect_true("MB" %in% names(result))
  expect_true("FM" %in% names(result))
  expect_false("PH" %in% names(result))
  expect_equal(length(result$MB), 3L)
  expect_equal(length(result$FM), 4L)
})

test_that("read_anq_multi warns when file does not contain expected record type", {
  # pass MB file but label it as PH
  paths <- c(
    PH = example_path("example_mb.txt")
  )
  expect_warning(
    read_anq_multi(paths),
    regexp = "does not contain record type"
  )
})

# -- is_anq_template -----------------------------------------------------------

test_that("is_anq_template returns TRUE for the ANQ template file", {
  path <- example_path("example_anq_template.xlsx")
  expect_true(is_anq_template(path))
})

test_that("is_anq_template returns FALSE for a simple XLSX file", {
  path <- example_path("example.xlsx")
  expect_false(is_anq_template(path))
})

# -- read_anq_template ---------------------------------------------------------

test_that("read_anq_template reads data correctly from ANQ template", {
  path <- example_path("example_anq_template.xlsx")
  result <- read_anq_template(path)

  expect_true("MB" %in% names(result))
  expect_true("MP" %in% names(result))
  expect_true("PH" %in% names(result))
  expect_true("FM" %in% names(result))

  expect_equal(length(result$MB), 3L)
  expect_equal(length(result$PH), 6L)
  expect_equal(length(result$FM), 4L)
})

test_that("read_anq_template and read_txt_raw produce same FIDs", {
  path_template <- example_path("example_anq_template.xlsx")
  path_txt <- example_path("example.txt")

  result_template <- read_anq_template(path_template)
  result_txt <- read_txt_raw(path_txt)

  fid_template <- vapply(result_template$MB, `[[`, character(1), 51)
  fid_txt <- vapply(result_txt$MB, `[[`, character(1), 51)

  expect_equal(sort(fid_template), sort(fid_txt))
})


test_that("read_txt_raw preserves a trailing empty field", {
  tmp <- tempfile(fileext = ".txt")

  writeLines(
    "PH|A|B|C|",
    tmp
  )

  result <- read_txt_raw(tmp)

  expect_equal(
    result$PH[[1]],
    c("PH", "A", "B", "C", "")
  )

  unlink(tmp)
})
