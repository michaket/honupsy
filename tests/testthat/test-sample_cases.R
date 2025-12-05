test_that("sample_cases returns all expected columns", {
  x <- sample_cases(n = 10)

  expected_cols <- c(
    "case_id",
    "unit",
    "age",
    "admission",
    "discharge",
    paste0("honos_", 1:12)
  )

  expect_true(all(expected_cols %in% names(x)))
})


test_that("HoNOS item scores are valid (0–4 or 9)", {
  x <- sample_cases(n = 200)

  honos_values <- unlist(x[paste0("honos_", 1:12)])

  expect_true(all(honos_values %in% c(0:4, 9)))
})


test_that("admission and discharge dates are valid", {
  x <- sample_cases(n = 200)

  expect_s3_class(x$admission, "Date")
  expect_s3_class(x$discharge, "Date")

  # allow NA or odd values if distributions produce them,
  # but ensure no discharge < admission
  expect_true(all(x$discharge >= x$admission))
})
