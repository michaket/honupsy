test_that("summarise_honos_units returns one row per unit", {
  cases <- sample_cases(n = 20, units = c("A", "B"))
  res <- summarise_honos_units(cases)
  expect_equal(nrow(res), length(unique(cases$unit)))
})


test_that("summarise_honos_units returns expected severe columns", {
  cases <- sample_cases(n = 10)
  res <- summarise_honos_units(cases)
  expected_cols <- paste0("prop_honos_", 1:12, "_severe")
  expect_true(all(expected_cols %in% names(res)))
})


test_that("severe proportions are between 0 and 1", {
  cases <- sample_cases(n = 50)
  res <- summarise_honos_units(cases)
  prop_cols <- grep("^prop_honos_\\d+_severe$", names(res), value = TRUE)
  props <- unlist(res[prop_cols])
  expect_true(all(props >= 0 & props <= 1, na.rm = TRUE))
})


test_that("age_mean equals the mean age per unit", {
  df <- data.frame(
    unit = c("X", "X", "X"),
    age = c(40, 50, 60),
    admission = as.Date("2023-01-01"),
    discharge = as.Date("2023-01-02"),
    honos_1 = c(0L, 0L, 0L),
    honos_2 = c(0L, 0L, 0L),
    honos_3 = c(0L, 0L, 0L),
    honos_4 = c(0L, 0L, 0L),
    honos_5 = c(0L, 0L, 0L),
    honos_6 = c(0L, 0L, 0L),
    honos_7 = c(0L, 0L, 0L),
    honos_8 = c(0L, 0L, 0L),
    honos_9 = c(0L, 0L, 0L),
    honos_10 = c(0L, 0L, 0L),
    honos_11 = c(0L, 0L, 0L),
    honos_12 = c(0L, 0L, 0L)
  )

  res <- summarise_honos_units(df)
  expect_equal(res$age_mean, mean(df$age))
})


test_that("los_mean equals the mean length of stay per unit", {
  df <- data.frame(
    unit = c("X", "X"),
    age = c(40, 50),
    admission = as.Date(c("2023-01-01", "2023-01-01")),
    discharge = as.Date(c("2023-01-11", "2023-01-21")), # 10 and 20 days
    honos_1 = c(0L, 0L),
    honos_2 = c(0L, 0L),
    honos_3 = c(0L, 0L),
    honos_4 = c(0L, 0L),
    honos_5 = c(0L, 0L),
    honos_6 = c(0L, 0L),
    honos_7 = c(0L, 0L),
    honos_8 = c(0L, 0L),
    honos_9 = c(0L, 0L),
    honos_10 = c(0L, 0L),
    honos_11 = c(0L, 0L),
    honos_12 = c(0L, 0L)
  )

  res <- summarise_honos_units(df)
  expect_equal(res$los_mean, mean(c(10, 20)))
})


test_that("HoNOS score 9 is treated as not severe in the dichotomised variable", {
  df <- data.frame(
    unit = c("A", "A", "A"),
    age = c(40, 50, 60),
    admission = as.Date("2023-01-01"),
    discharge = as.Date("2023-01-02"),
    honos_1 = c(0L, 3L, 9L), # 0 = not severe, 3 = severe, 9 = treated as not severe
    honos_2 = c(0L, 0L, 0L),
    honos_3 = c(0L, 0L, 0L),
    honos_4 = c(0L, 0L, 0L),
    honos_5 = c(0L, 0L, 0L),
    honos_6 = c(0L, 0L, 0L),
    honos_7 = c(0L, 0L, 0L),
    honos_8 = c(0L, 0L, 0L),
    honos_9 = c(0L, 0L, 0L),
    honos_10 = c(0L, 0L, 0L),
    honos_11 = c(0L, 0L, 0L),
    honos_12 = c(0L, 0L, 0L)
  )

  res <- summarise_honos_units(df)

  # expectation: 1 severe (score 3) out of 3 cases (0, 3, 9->0) => 1/3
  expect_equal(res$prop_honos_1_severe, 1 / 3)
})
