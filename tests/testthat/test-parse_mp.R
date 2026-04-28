# -- parse_mp_row: valid input -------------------------------------------------

test_that("parse_mp_row parses a valid row correctly", {
  # case 1 from example data: married, full-time employed, general psychiatry
  r <- c(
    "MP", # 1  record type
    "2", # 2  civil status (2=married)
    "11", # 3  location before admission
    "0", # 4  employment_part_time
    "1", # 5  employment_full_time
    "0", # 6  employment_unemployed
    "0", # 7  employment_homemaker
    "0", # 8  employment_in_education
    "0", # 9  employment_rehabilitation
    "0", # 10 employment_pension
    "0", # 11 employment_sheltered
    "0", # 12 employment_unknown
    "2", # 13 highest_education
    "11", # 14 referring_institution
    "1", # 15 voluntary_admission
    "1", # 16 involuntary_placement
    "9", # 17 number_of_days
    "12", # 18 treatment
    "1", # 19 pharma_neuroleptics
    "1", # 20 pharma_depot_neuroleptics
    "0", # 21 pharma_antidepressants
    "0", # 22 pharma_tranquilizers
    "1", # 23 pharma_hypnotics
    "0", # 24 pharma_antiepileptics
    "0", # 25 pharma_lithium
    "0", # 26 pharma_addiction_substitution
    "0", # 27 pharma_addiction_aversion
    "0", # 28 pharma_antiparkinson
    "1", # 29 pharma_other
    "0", # 30 pharma_somatic
    "11", # 31 discharge_decision
    "23", # 32 residence_after_discharge
    "1", # 33 treatment_after_discharge
    "1", # 34 treatment_area (1=general psychiatry)
    "5050286" # 35 FID
  )

  result <- parse_mp_row(r)

  # identification
  expect_equal(result$fid, "5050286")

  # sociodemographics
  expect_equal(result$civil_status, 2L) # married
  expect_equal(result$location_before_admission, 11L)

  # employment (multiple answers possible)
  expect_equal(result$employment_part_time, 0L)
  expect_equal(result$employment_full_time, 1L) # full-time
  expect_equal(result$employment_unemployed, 0L)
  expect_equal(result$employment_pension, 0L)
  expect_equal(result$employment_unknown, 0L)

  # education
  expect_equal(result$highest_education, 2L)

  # admission
  expect_equal(result$referring_institution, 11L)
  expect_equal(result$involuntary_placement, 1L)

  # pharmacotherapy
  expect_equal(result$pharma_neuroleptics, 1L)
  expect_equal(result$pharma_antidepressants, 0L)
  expect_equal(result$pharma_hypnotics, 1L)
  expect_equal(result$pharma_antiepileptics, 0L)

  # discharge
  expect_equal(result$discharge_decision, 11L)
  expect_equal(result$residence_after_discharge, 23L)
  expect_equal(result$treatment_after_discharge, 1L)

  # treatment area
  expect_equal(result$treatment_area, 1L) # general psychiatry
})

test_that("parse_mp_row parses treatment area correctly for all areas", {
  make_row <- function(treatment_area) {
    r <- c(
      "MP",
      "2",
      "11",
      "0",
      "1",
      "0",
      "0",
      "0",
      "0",
      "0",
      "0",
      "0",
      "2",
      "11",
      "1",
      "1",
      "9",
      "12",
      "1",
      "1",
      "0",
      "0",
      "1",
      "0",
      "0",
      "0",
      "0",
      "0",
      "1",
      "0",
      "11",
      "23",
      "1",
      as.character(treatment_area),
      "5050286"
    )
  }

  expect_equal(parse_mp_row(make_row(1))$treatment_area, 1L) # general
  expect_equal(parse_mp_row(make_row(2))$treatment_area, 2L) # child & adolescent
  expect_equal(parse_mp_row(make_row(3))$treatment_area, 3L) # geriatric
  expect_equal(parse_mp_row(make_row(4))$treatment_area, 4L) # addiction
  expect_equal(parse_mp_row(make_row(5))$treatment_area, 5L) # forensic
})

# -- parse_mp_row: too few fields ----------------------------------------------

test_that("parse_mp_row skips rows with too few fields and warns", {
  r <- c("MP", "2", "11") # only 3 fields
  expect_warning(
    result <- parse_mp_row(r),
    regexp = "35 expected"
  )
  expect_null(result)
})

# -- parse_mp: multiple rows ---------------------------------------------------

test_that("parse_mp returns one row per valid case", {
  r1 <- c(
    "MP",
    "2",
    "11",
    "0",
    "1",
    "0",
    "0",
    "0",
    "0",
    "0",
    "0",
    "0",
    "2",
    "11",
    "1",
    "1",
    "9",
    "12",
    "1",
    "1",
    "0",
    "0",
    "1",
    "0",
    "0",
    "0",
    "0",
    "0",
    "1",
    "0",
    "11",
    "23",
    "1",
    "1",
    "5050286"
  )
  r2 <- c(
    "MP",
    "1",
    "12",
    "0",
    "0",
    "1",
    "0",
    "0",
    "0",
    "1",
    "0",
    "0",
    "9",
    "31",
    "2",
    "2",
    "6",
    "0",
    "0",
    "0",
    "1",
    "0",
    "1",
    "0",
    "0",
    "0",
    "0",
    "0",
    "1",
    "0",
    "11",
    "12",
    "2",
    "1",
    "5050297"
  )

  result <- parse_mp(list(r1, r2))
  expect_equal(nrow(result), 2L)
  expect_equal(result$fid, c("5050286", "5050297"))
})

test_that("parse_mp skips invalid rows and keeps valid ones", {
  r_valid <- c(
    "MP",
    "2",
    "11",
    "0",
    "1",
    "0",
    "0",
    "0",
    "0",
    "0",
    "0",
    "0",
    "2",
    "11",
    "1",
    "1",
    "9",
    "12",
    "1",
    "1",
    "0",
    "0",
    "1",
    "0",
    "0",
    "0",
    "0",
    "0",
    "1",
    "0",
    "11",
    "23",
    "1",
    "1",
    "5050286"
  )
  r_invalid <- c("MP", "2") # too short

  expect_warning(
    result <- parse_mp(list(r_valid, r_invalid))
  )
  expect_equal(nrow(result), 1L)
  expect_equal(result$fid, "5050286")
})

# -- parse_mp: empty rows ------------------------------------------------------

test_that("parse_mp skips trailing empty rows silently", {
  r_valid <- c(
    "MP",
    "2",
    "11",
    "0",
    "1",
    "0",
    "0",
    "0",
    "0",
    "0",
    "0",
    "0",
    "2",
    "11",
    "1",
    "1",
    "9",
    "12",
    "1",
    "1",
    "0",
    "0",
    "1",
    "0",
    "0",
    "0",
    "0",
    "0",
    "1",
    "0",
    "11",
    "23",
    "1",
    "1",
    "5050286"
  )
  r_empty <- c("", "", "")

  expect_no_warning(
    result <- parse_mp(list(r_valid, r_empty))
  )
  expect_equal(nrow(result), 1L)
})

test_that("parse_mp skips empty rows between valid rows silently", {
  r_valid1 <- c(
    "MP",
    "2",
    "11",
    "0",
    "1",
    "0",
    "0",
    "0",
    "0",
    "0",
    "0",
    "0",
    "2",
    "11",
    "1",
    "1",
    "9",
    "12",
    "1",
    "1",
    "0",
    "0",
    "1",
    "0",
    "0",
    "0",
    "0",
    "0",
    "1",
    "0",
    "11",
    "23",
    "1",
    "1",
    "5050286"
  )
  r_empty <- c("", "", "")
  r_valid2 <- c(
    "MP",
    "1",
    "12",
    "0",
    "0",
    "1",
    "0",
    "0",
    "0",
    "1",
    "0",
    "0",
    "9",
    "31",
    "2",
    "2",
    "6",
    "0",
    "0",
    "0",
    "1",
    "0",
    "1",
    "0",
    "0",
    "0",
    "0",
    "0",
    "1",
    "0",
    "11",
    "12",
    "2",
    "1",
    "5050297"
  )

  expect_no_warning(
    result <- parse_mp(list(r_valid1, r_empty, r_valid2))
  )
  expect_equal(nrow(result), 2L)
})

# -- empty_mp ------------------------------------------------------------------

test_that("empty_mp returns a data frame with zero rows and correct types", {
  result <- empty_mp()

  expect_equal(nrow(result), 0L)

  expect_type(result$fid, "character")
  expect_type(result$civil_status, "integer")
  expect_type(result$employment_full_time, "integer")
  expect_type(result$employment_pension, "integer")
  expect_type(result$highest_education, "integer")
  expect_type(result$pharma_neuroleptics, "integer")
  expect_type(result$treatment_area, "integer")
  expect_type(result$discharge_decision, "integer")
})
