test_that("returns one row per unit with the lean columns", {
  d <- sim_units(200)
  comp <- summarise_composition(d)

  expect_s3_class(comp, "data.frame")
  expect_setequal(comp$unit, unique(d$mb$unit))
  expect_named(
    comp,
    c(
      "unit",
      "n_cases",
      "n_age_missing",
      "n_los_missing",
      "n_sex_missing",
      "age_mean",
      "los_mean",
      "n_female",
      "prop_female"
    ),
    ignore.order = TRUE
  )
})


test_that("n_cases accounts for every assigned case", {
  d <- sim_units(200)
  expect_equal(sum(summarise_composition(d)$n_cases), nrow(d$mb))
})


test_that("cases without a unit are excluded", {
  set.seed(2)
  x <- sample_cases(n = 100)
  lookup <- data.frame(
    fid = x$mb$fid[1:60],
    unit = rep(c("A", "B"), each = 30)
  )
  x <- suppressWarnings(assign_units(x, import_unit_assignments(lookup)))

  expect_equal(sum(summarise_composition(x)$n_cases), 60L)
})


test_that("detail = 'full' adds distributional columns", {
  d <- sim_units(200)
  full <- summarise_composition(d, detail = "full")

  expect_true(all(
    c("age_median", "age_sd", "age_iqr", "los_median", "los_sd", "los_iqr") %in%
      names(full)
  ))
})


test_that("missing discharge is counted in n_los_missing", {
  d <- sim_units(150, p_missing_discharge = 1)
  comp <- summarise_composition(d)

  expect_equal(comp$n_los_missing, comp$n_cases) # every LOS missing
  expect_true(all(comp$n_age_missing == 0L))
  expect_true(all(comp$n_sex_missing == 0L))
})


test_that("derived statistics are in plausible ranges", {
  d <- sim_units(200)
  comp <- summarise_composition(d)

  expect_true(all(comp$age_mean >= 18))
  expect_true(all(comp$prop_female >= 0 & comp$prop_female <= 1))
  expect_true(all(comp$n_female <= comp$n_cases))
})
