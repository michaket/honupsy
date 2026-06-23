test_that("returns one row per unit with quality and item columns", {
  d <- sim_units(200)
  sev <- summarise_honos_severity(d)

  expect_setequal(sev$unit, unique(d$mb$unit))
  expect_true(all(
    c(
      "n_cases",
      "n_honos_adm",
      "prop_assessed",
      "mean_items_rated",
      "n_partial",
      "honos_total_mean"
    ) %in%
      names(sev)
  ))
  expect_true(all(paste0("h", 1:12, "_mean") %in% names(sev)))
  expect_true(all(paste0("h", 1:12, "_prop_severe") %in% names(sev)))
})


test_that("clean data is fully assessed with no partials", {
  d <- sim_units(200)
  sev <- summarise_honos_severity(d)

  expect_equal(sev$prop_assessed, rep(1, nrow(sev)))
  expect_equal(sev$mean_items_rated, rep(12, nrow(sev)))
  expect_equal(sev$n_partial, rep(0L, nrow(sev)))
})


test_that("all admission dropouts means nothing is assessed", {
  d <- sim_units(150, p_dropout_adm = 1)
  sev <- summarise_honos_severity(d)

  expect_equal(sev$n_honos_adm, rep(0L, nrow(sev)))
  expect_equal(sev$prop_assessed, rep(0, nrow(sev)))
})


test_that("all items coded 9 makes every assessment partial", {
  d <- sim_units(150, p_item_na = 1)
  sev <- summarise_honos_severity(d)

  expect_equal(sev$n_partial, sev$n_honos_adm) # all partial
  expect_equal(sev$mean_items_rated, rep(0, nrow(sev)))

  props <- sev[, paste0("h", 1:12, "_prop_severe")]
  expect_true(all(props == 0)) # NA counts as non-severe
})


test_that("n_cases agrees with summarise_composition", {
  d <- sim_units(200)
  comp <- summarise_composition(d)
  sev <- summarise_honos_severity(d)

  m <- merge(
    comp[, c("unit", "n_cases")],
    sev[, c("unit", "n_cases")],
    by = "unit"
  )
  expect_equal(m$n_cases.x, m$n_cases.y)
})


test_that("item means and severe proportions are in range", {
  d <- sim_units(200)
  sev <- summarise_honos_severity(d)

  means <- unlist(sev[, paste0("h", 1:12, "_mean")], use.names = FALSE)
  props <- unlist(sev[, paste0("h", 1:12, "_prop_severe")], use.names = FALSE)

  expect_true(all(means >= 0 & means <= 4, na.rm = TRUE))
  expect_true(all(props >= 0 & props <= 1, na.rm = TRUE))
})
