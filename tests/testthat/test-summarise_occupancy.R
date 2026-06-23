test_that("returns the by_unit/excluded list with expected columns", {
  d <- sim_units(100)
  res <- suppressMessages(summarise_occupancy(d, year = 2023))

  expect_named(res, c("by_unit", "excluded"), ignore.order = TRUE)
  expect_named(
    res$by_unit,
    c(
      "unit",
      "unit_size",
      "census_mean",
      "census_max",
      "occ_rate_mean",
      "occ_rate_max"
    ),
    ignore.order = TRUE
  )
})


test_that("census_mean <= census_max and rates are consistent", {
  d <- sim_units(100)
  bu <- suppressMessages(summarise_occupancy(d, year = 2023))$by_unit

  expect_true(all(bu$census_mean <= bu$census_max))
  expect_equal(bu$occ_rate_mean, bu$census_mean / bu$unit_size)
  expect_equal(bu$occ_rate_max, bu$census_max / bu$unit_size)
})


test_that("annual mean agrees with the daily series", {
  d <- sim_units(100, units = c("A", "B"))
  occ <- suppressMessages(occupancy_daily(d, year = 2023))
  res <- suppressMessages(summarise_occupancy(d, year = 2023))

  agg <- aggregate(census ~ unit, data = occ$daily, FUN = mean)
  m <- merge(res$by_unit[, c("unit", "census_mean")], agg, by = "unit")
  expect_equal(m$census_mean, m$census)
})


test_that("excluded tally matches occupancy_daily", {
  d <- sim_units(100)
  occ <- suppressMessages(occupancy_daily(d, year = 2023))
  res <- suppressMessages(summarise_occupancy(d, year = 2023))

  expect_equal(res$excluded, occ$excluded)
})
