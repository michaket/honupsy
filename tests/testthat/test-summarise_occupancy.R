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


test_that("hourly occupancy uses strict admission and discharge boundaries", {
  d <- list(
    mb = data.frame(
      fid = "1",
      unit = "A",
      admission = as.POSIXct(
        "2022-01-01 10:00:00",
        tz = "UTC"
      ),
      discharge = as.POSIXct(
        "2022-01-01 13:00:00",
        tz = "UTC"
      ),
      stringsAsFactors = FALSE
    )
  )

  occ <- suppressMessages(
    occupancy_daily(d, year = 2022)
  )

  jan1 <- occ$daily[
    occ$daily$unit == "A" &
      occ$daily$date == as.Date("2022-01-01"),
  ]

  # Paper convention:
  # 10:00 excluded
  # 11:00 counted
  # 12:00 counted
  # 13:00 excluded
  #
  # Therefore 2 patient-hours across 24 hours.
  expect_equal(jan1$census, 2 / 24)
})


test_that("hourly occupancy adds overlapping patients correctly", {
  d <- list(
    mb = data.frame(
      fid = c("1", "2"),
      unit = c("A", "A"),
      admission = as.POSIXct(
        c(
          "2022-01-01 10:00:00",
          "2022-01-01 11:00:00"
        ),
        tz = "UTC"
      ),
      discharge = as.POSIXct(
        c(
          "2022-01-01 14:00:00",
          "2022-01-01 13:00:00"
        ),
        tz = "UTC"
      ),
      stringsAsFactors = FALSE
    )
  )

  occ <- suppressMessages(
    occupancy_daily(d, year = 2022)
  )

  jan1 <- occ$daily[
    occ$daily$unit == "A" &
      occ$daily$date == as.Date("2022-01-01"),
  ]

  # Patient 1 contributes at 11:00, 12:00, 13:00 = 3 patient-hours.
  # Patient 2 contributes at 12:00 = 1 patient-hour.
  # Total = 4 patient-hours across 24 hours.
  expect_equal(jan1$census, 4 / 24)
})


test_that("discharge before admission is an error", {
  d <- list(
    mb = data.frame(
      fid = "1",
      unit = "A",
      admission = as.POSIXct(
        "2022-05-02 10:00:00",
        tz = "UTC"
      ),
      discharge = as.POSIXct(
        "2022-05-01 10:00:00",
        tz = "UTC"
      ),
      stringsAsFactors = FALSE
    )
  )

  expect_error(
    occupancy_daily(d, year = 2022),
    "Discharge precedes admission"
  )
})

test_that("unit size is largest exact occupancy level occurring more than 88 hours", {
  d <- list(
    mb = data.frame(
      fid = c("1", "2"),
      unit = c("A", "A"),
      admission = as.POSIXct(
        c(
          "2022-01-01 00:00:00",
          "2022-01-01 00:00:00"
        ),
        tz = "UTC"
      ),
      discharge = as.POSIXct(
        c(
          "2022-01-04 18:00:00",
          "2022-01-04 18:00:00"
        ),
        tz = "UTC"
      ),
      stringsAsFactors = FALSE
    )
  )

  occ <- suppressMessages(
    occupancy_daily(d, year = 2022)
  )

  # The two patients are simultaneously present for exactly 89
  # counted hourly observations. Because the paper rule requires
  # more than 88 hours, the estimated unit size is 2.
  expect_equal(
    occ$unit_size$unit_size,
    2
  )
})

test_that("units with no stay overlapping the reference year are excluded", {
  d <- list(
    mb = data.frame(
      fid = c("1", "2"),
      unit = c("A", "B"),
      admission = as.POSIXct(
        c(
          "2022-01-01 00:00:00",
          "2021-01-01 00:00:00"
        ),
        tz = "UTC"
      ),
      discharge = as.POSIXct(
        c(
          "2022-01-10 00:00:00",
          "2021-01-10 00:00:00"
        ),
        tz = "UTC"
      ),
      stringsAsFactors = FALSE
    )
  )

  occ <- suppressMessages(
    occupancy_daily(d, year = 2022)
  )

  expect_equal(unique(occ$daily$unit), "A")
  expect_equal(occ$excluded$n_outside_year, 1L)
})


test_that("unit size is NA when no positive occupancy level occurs more than 88 hours", {
  d <- list(
    mb = data.frame(
      fid = "1",
      unit = "A",
      admission = as.POSIXct(
        "2022-01-01 10:00:00",
        tz = "UTC"
      ),
      discharge = as.POSIXct(
        "2022-01-01 13:00:00",
        tz = "UTC"
      ),
      stringsAsFactors = FALSE
    )
  )

  occ <- suppressMessages(
    occupancy_daily(d, year = 2022)
  )

  expect_true(is.na(occ$unit_size$unit_size))
})

test_that("stays crossing into the reference year are clipped correctly", {
  d <- list(
    mb = data.frame(
      fid = "1",
      unit = "A",
      admission = as.POSIXct(
        "2021-12-31 22:00:00",
        tz = "UTC"
      ),
      discharge = as.POSIXct(
        "2022-01-01 03:00:00",
        tz = "UTC"
      ),
      stringsAsFactors = FALSE
    )
  )

  occ <- suppressMessages(
    occupancy_daily(d, year = 2022)
  )

  jan1 <- occ$daily[
    occ$daily$unit == "A" &
      occ$daily$date == as.Date("2022-01-01"),
  ]

  # Within 2022 the patient contributes at 00:00, 01:00 and 02:00.
  expect_equal(jan1$census, 3 / 24)
})
