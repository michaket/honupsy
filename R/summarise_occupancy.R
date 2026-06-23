#' Summarise Unit Occupancy Over a Calendar Year
#'
#' Annual per-unit occupancy: estimated unit size, mean and maximum daily
#' census, and the corresponding occupancy rates. This is a thin aggregation of
#' \code{\link{occupancy_daily}}; use that function directly if you need the
#' daily series (e.g. for plotting occupancy over time).
#'
#' @inheritParams occupancy_daily
#'
#' @return A \code{data.frame} with one row per unit and the following columns:
#' \describe{
#'   \item{unit}{Unit identifier (character).}
#'   \item{unit_size}{Estimated unit capacity (1\% rule).}
#'   \item{census_mean}{Mean daily patient census across all days of the year.}
#'   \item{census_max}{Maximum daily patient census observed.}
#'   \item{occ_rate_mean}{Mean daily occupancy rate (\code{census_mean / unit_size}).}
#'   \item{occ_rate_max}{Maximum daily occupancy rate (\code{census_max / unit_size}).}
#' }
#' The result also carries the attribute \code{"excluded"}: a one-row data
#' frame with \code{n_input} (cases in MB), \code{n_no_unit},
#' \code{n_no_admission}, \code{n_no_discharge} (overlapping counts of the
#' missing-field reasons), \code{n_excluded_missing} (cases dropped for any
#' missing field), and \code{n_outside_year} (cases dropped because their
#' clipped stay did not overlap the reference year).
#'
#' @seealso \code{\link{occupancy_daily}}, \code{\link{summarise_composition}},
#'   \code{\link{summarise_honos_severity}}
#'
#' @importFrom stats aggregate
#' @export
#'
#' @examples
#' \dontrun{
#' data  <- import_anq("data.txt")
#' units <- import_unit_assignments("units.xlsx")
#' data  <- assign_units(data, units)
#' result <- summarise_occupancy(data, year = 2022)
#' attr(result, "excluded")   # how many cases were dropped, and why
#' }
summarise_occupancy <- function(data, year) {
  daily <- occupancy_daily(data, year)
  unit_size <- attr(daily, "unit_size")

  annual <- aggregate(
    census ~ unit,
    data = daily,
    FUN = function(x) c(mean = mean(x), max = max(x))
  )
  annual <- data.frame(
    unit = annual$unit,
    census_mean = annual$census[, "mean"],
    census_max = annual$census[, "max"]
  )

  result <- merge(annual, unit_size, by = "unit", all.x = TRUE)
  result$occ_rate_mean <- result$census_mean / result$unit_size
  result$occ_rate_max <- result$census_max / result$unit_size

  result <- result[
    order(result$unit),
    c(
      "unit",
      "unit_size",
      "census_mean",
      "census_max",
      "occ_rate_mean",
      "occ_rate_max"
    )
  ]
  rownames(result) <- NULL

  attr(result, "excluded") <- attr(daily, "excluded")
  result
}
