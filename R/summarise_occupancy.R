#' Summarise Unit Occupancy Over a Calendar Year
#'
#' Annual per-unit occupancy: estimated unit size, mean and maximum daily
#' census, and the corresponding occupancy rates. This is a thin aggregation of
#' \code{\link{occupancy_daily}}; use that function directly if you need the
#' daily series (e.g. for plotting occupancy over time).
#'
#' @inheritParams occupancy_daily
#'
#' @return A named list with two elements:
#'
#'   - `by_unit`: a data frame with one row per unit and columns `unit`,
#'     `unit_size` (1% rule capacity), `census_mean`, `census_max`,
#'     `occ_rate_mean` (`census_mean / unit_size`) and `occ_rate_max`
#'     (`census_max / unit_size`).
#'   - `excluded`: a one-row data frame with `n_input` (cases in MB),
#'     `n_no_unit`, `n_no_admission`, `n_no_discharge` (overlapping counts of
#'     the missing-field reasons), `n_excluded_missing` (cases dropped for any
#'     missing field), and `n_outside_year` (cases dropped because their clipped
#'     stay did not overlap the reference year).
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
#' result$by_unit
#' result$excluded   # how many cases were dropped, and why
#' }
summarise_occupancy <- function(data, year) {
  occ <- occupancy_daily(data, year)

  annual <- aggregate(
    census ~ unit,
    data = occ$daily,
    FUN = function(x) c(mean = mean(x), max = max(x))
  )
  annual <- data.frame(
    unit = annual$unit,
    census_mean = annual$census[, "mean"],
    census_max = annual$census[, "max"]
  )

  by_unit <- merge(annual, occ$unit_size, by = "unit", all.x = TRUE)
  by_unit$occ_rate_mean <- by_unit$census_mean / by_unit$unit_size
  by_unit$occ_rate_max <- by_unit$census_max / by_unit$unit_size

  by_unit <- by_unit[
    order(by_unit$unit),
    c(
      "unit",
      "unit_size",
      "census_mean",
      "census_max",
      "occ_rate_mean",
      "occ_rate_max"
    )
  ]
  rownames(by_unit) <- NULL

  list(by_unit = by_unit, excluded = occ$excluded)
}
