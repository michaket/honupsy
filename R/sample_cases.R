#' Generate Sample Inpatient Cases With HoNOS Scores
#'
#'Creates a simulated data set of inpatient psychiatric cases with unit
#' assignments, admission and discharge dates, age, and item scores from the
#' Health of the Nation Outcome Scales (HoNOS).
#' The simulated data are useful for examples, testing, and development of
#' functions that work with routinely collected HoNOS data.
#' The overall structure largely follows the Swiss data definition for the
#' national measurements in adult inpatient psychiatry.
#'
#' @param n Integer. Number of cases to generate. Defaults to \code{1000}.
#' @param units Character vector of unit identifiers from which units are
#'   sampled. Defaults to \code{LETTERS[1:5]}.
#' @param start_date Start date of the admission period as \code{Date}.
#'   Defaults to \code{as.Date("2023-01-01")}.
#' @param end_date End date of the admission period as \code{Date}.
#'   Defaults to \code{as.Date("2023-12-31")}.
#'
#' @return
#' A data frame with \code{n} rows and the following columns:
#' \itemize{
#'   \item \code{case_id}: Unique case identifier (integer).
#'   \item \code{unit}: Unit identifier (character).
#'   \item \code{age}: Patient age (numeric).
#'   \item \code{admission}: Admission date (\code{Date}).
#'   \item \code{discharge}: Discharge date (\code{Date}).
#'   \item \code{honos_1}, \code{honos_2}, \dots, \code{honos_12}: Scores for the
#'     twelve items of the Health of the Nation Outcome Scales (HoNOS). Each item
#'     is coded as an integer according to the official scale:
#'     \itemize{
#'       \item 0 = no problem
#'       \item 1 = problem of minor clinical significance, no action required
#'       \item 2 = mild but definite problem
#'       \item 3 = moderately severe problem
#'       \item 4 = severe to very severe problem
#'       \item 9 = not known / not applicable
#'     }
#' }
#'
#' @export
#'
#' @examples
#' # Generate 100 sample cases on three units
#' cases <- sample_cases(n = 100, units = c("A", "B", "C"))
#' head(cases)
sample_cases <- function(
  n = 1000,
  units = LETTERS[1:5],
  start_date = as.Date("2023-01-01"),
  end_date = as.Date("2023-12-31")
) {
  # Generate case-level data
  df_cases <- data.frame(
    case_id = sample(10000:99999, size = n, replace = FALSE),
    unit = sample(units, size = n, replace = TRUE),
    # simulate a plausible adult psychiatric inpatient age distribution
    age = round(stats::rnorm(n, mean = 45, sd = 20)),
    admission = sample(
      seq(start_date, end_date, by = "day"),
      size = n,
      replace = TRUE
    )
  )

  # Generate discharge dates
  # simulate a plausible adult psychiatric length of stay distribution
  los <- round(stats::rnorm(n, mean = 25, sd = 30))
  los <- pmax(los, 1L) # ensure length of stay at least 1 day

  df_cases$discharge <- df_cases$admission + los

  # Add 12 HoNOS items
  for (i in 1:12) {
    df_cases[[paste0("honos_", i)]] <- sample(
      c(0:4, 9),
      size = n,
      replace = TRUE,
      # simulate a plausible distribution of HoNOS item
      # scores for adult psychiatric patients
      prob = c(0.369, 0.20, 0.18, 0.16, 0.09, 0.001)
    )
  }

  df_cases
}
