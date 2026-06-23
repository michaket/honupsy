test_that("sample_cases returns the import_anq() list structure", {
  x <- sample_cases(n = 10)

  expect_type(x, "list")
  expect_named(x, c("mb", "mp", "ph", "pb", "fm"), ignore.order = TRUE)

  # MB and PH populated; MP/PB/FM empty (not generated in this version)
  expect_equal(nrow(x$mb), 10L)
  expect_gt(nrow(x$ph), 0L)
  expect_equal(nrow(x$mp), 0L)
  expect_equal(nrow(x$pb), 0L)
  expect_equal(nrow(x$fm), 0L)
})


test_that("MB is well formed", {
  x <- sample_cases(n = 200)
  mb <- x$mb

  # one row per case, unique non-missing FIDs
  expect_type(mb$fid, "character")
  expect_equal(anyDuplicated(mb$fid), 0L)
  expect_false(any(is.na(mb$fid) | mb$fid == ""))

  # validator expectations: psychiatry flag 1, M500, adult ages
  expect_true(all(mb$flag_psychiatry == 1L))
  expect_true(all(mb$main_cost_centre == "M500"))
  expect_true(all(mb$sex %in% c(1L, 2L)))
  expect_true(all(mb$age_admission >= 18L))

  # datetimes parsed, discharge not before admission
  expect_s3_class(mb$admission, "POSIXct")
  expect_s3_class(mb$discharge, "POSIXct")
  expect_true(all(mb$discharge >= mb$admission))
})


test_that("every case has PH rows and PH fids exist in MB", {
  x <- sample_cases(n = 50)

  # admission + discharge row per case by default
  expect_equal(nrow(x$ph), 2L * nrow(x$mb))
  expect_true(all(c(1L, 2L) %in% x$ph$time_point))

  # no orphan PH rows; every MB case appears in PH
  expect_true(all(x$ph$fid %in% x$mb$fid))
  expect_true(all(x$mb$fid %in% x$ph$fid))
})


test_that("include_discharge = FALSE drops discharge rows", {
  x <- sample_cases(n = 50, include_discharge = FALSE)

  expect_equal(nrow(x$ph), nrow(x$mb))
  expect_true(all(x$ph$time_point == 1L))
})


test_that("clean defaults produce complete, dropout-free data", {
  x <- sample_cases(n = 100)

  expect_false(any(x$ph$is_dropout))
  expect_false(any(is.na(x$mb$discharge)))

  # all 12 items rated on every assessment
  expect_true(all(x$ph$honos_n_valid == 12L))

  # raw item values are valid HoNOS codes
  raw_cols <- paste0("h", 1:12, "_raw")
  raw_vals <- unlist(x$ph[raw_cols], use.names = FALSE)
  expect_true(all(raw_vals %in% c(0:4, 9L)))
})


test_that("p_dropout_adm = 1 makes every admission a dropout", {
  x <- sample_cases(n = 60, p_dropout_adm = 1)

  adm <- x$ph[x$ph$time_point == 1L, ]
  expect_true(all(adm$is_dropout))
  # dropout assessments carry no rated items
  expect_true(all(adm$honos_n_valid == 0L))
})


test_that("p_item_na = 1 codes every rated item as 9", {
  x <- sample_cases(n = 60, p_item_na = 1)

  adm <- x$ph[x$ph$time_point == 1L & !x$ph$is_dropout, ]
  raw_cols <- paste0("h", 1:12, "_raw")
  raw_vals <- unlist(adm[raw_cols], use.names = FALSE)

  expect_true(all(raw_vals == 9L))
  expect_true(all(adm$honos_n_valid == 0L)) # 9 -> NA value
})


test_that("p_missing_discharge = 1 removes all discharge dates", {
  x <- sample_cases(n = 60, p_missing_discharge = 1)

  expect_true(all(is.na(x$mb$discharge)))
  # discharge assessments become dropouts when the date is missing
  dis <- x$ph[x$ph$time_point == 2L, ]
  expect_true(all(dis$is_dropout))
})


test_that("output is reproducible under set.seed", {
  set.seed(123)
  a <- sample_cases(n = 40)
  set.seed(123)
  b <- sample_cases(n = 40)

  expect_equal(a, b)
})


test_that("output feeds the summarise pipeline", {
  x <- sample_cases(n = 120, p_dropout_adm = 0.2)

  lookup <- data.frame(
    fid = x$mb$fid,
    unit = sample(c("A", "B", "C"), nrow(x$mb), replace = TRUE)
  )
  x <- assign_units(x, import_unit_assignments(lookup))

  comp <- summarise_composition(x)
  sev <- summarise_honos_severity(x)

  expect_true(all(c("unit", "n_cases") %in% names(comp)))
  expect_true("prop_assessed" %in% names(sev))

  # with ~20% admission dropouts, completion is below 1 but positive
  expect_true(all(sev$prop_assessed >= 0 & sev$prop_assessed <= 1))
  expect_equal(sum(comp$n_cases), nrow(x$mb))
})
