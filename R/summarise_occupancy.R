#' Summarise Unit Occupancy Over a Calendar Year
#'
#' Expands each inpatient stay to hourly intervals and computes daily and
#' annual occupancy statistics per unit. Unit size is estimated from the data
#' using the 1\% rule: the largest occupancy level observed during at least 1\%
#' of all hours in the year (88 hours).
#'
#' Stays are clipped to the reference year before expansion. The hourly
#' census follows a right-open convention: a patient is counted in hour
#' \code{h} if their admission is strictly before \code{h + 1} and their
#' discharge is strictly after \code{h} (i.e. \code{admission < h_end} and
#' \code{discharge > h_start}).
#'
#' This function requires \code{purrr} and can be slow for large datasets
#' (thousands of cases × 8760 hours). Consider running it once and caching
#' the result.
#'
#' The input must be the output of \code{\link{import_anq}} after unit
#' assignments have been attached with \code{\link{assign_units}}. Cases
#' without a unit assignment or with missing admission/discharge are excluded
#' before expansion, and cases whose stay does not overlap the reference year
#' are dropped during clipping. Both exclusion counts are reported via a
#' message and stored on the result as the attribute \code{"excluded"} (a
#' one-row data frame; access with \code{attr(result, "excluded")}).
#'
#' @param data Named list. Output of \code{\link{import_anq}} with unit
#'   assignments attached via \code{\link{assign_units}}.
#' @param year Integer. The calendar year to summarise (e.g. \code{2022}).
#'
#' @return A \code{data.frame} with one row per unit and the following columns:
#' \describe{
#'   \item{unit}{Unit identifier (character).}
#'   \item{unit_size}{Estimated unit capacity (1\% rule: maximum occupancy
#'     observed in at least 88 hours of the year).}
#'   \item{census_mean}{Mean daily patient census across all days of the year.}
#'   \item{census_max}{Maximum daily patient census observed.}
#'   \item{occ_rate_mean}{Mean daily occupancy rate
#'     (\code{census_mean / unit_size}).}
#'   \item{occ_rate_max}{Maximum daily occupancy rate observed.}
#' }
#' The result additionally carries an attribute \code{"excluded"}: a one-row
#' data frame with \code{n_input} (cases in MB), \code{n_no_unit},
#' \code{n_no_admission}, \code{n_no_discharge} (overlapping counts of the
#' missing-field reasons), \code{n_excluded_missing} (cases dropped for any
#' missing field), and \code{n_outside_year} (cases dropped because their
#' clipped stay did not overlap the reference year).
#'
#' @seealso \code{\link{summarise_composition}},
#'   \code{\link{summarise_honos_severity}}
#'
#' @importFrom dplyr filter mutate group_by summarise left_join count ungroup
#'   coalesce right_join arrange .data
#' @importFrom purrr map2
#' @importFrom rlang .data
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
  if (!is.numeric(year) || length(year) != 1L || year != as.integer(year)) {
    stop("'year' must be a single integer, e.g. 2022.", call. = FALSE)
  }
  year <- as.integer(year)

  mb <- .extract_mb(data)

  # tally exclusion reasons before filtering (reasons overlap)
  n_input <- nrow(mb)
  n_no_unit <- sum(is.na(mb$unit))
  n_no_admission <- sum(is.na(mb$admission))
  n_no_discharge <- sum(is.na(mb$discharge))

  keep <- !is.na(mb$unit) & !is.na(mb$admission) & !is.na(mb$discharge)
  mb <- mb[keep, ]
  n_excluded_missing <- n_input - nrow(mb)

  if (nrow(mb) == 0L) {
    stop(
      "No cases with unit, admission, and discharge after filtering.",
      call. = FALSE
    )
  }

  # period boundaries
  start_yr <- as.POSIXct(
    paste0(year, "-01-01 00:00:00"),
    tz = "UTC"
  )
  end_yr <- as.POSIXct(
    paste0(year + 1L, "-01-01 00:00:00"),
    tz = "UTC"
  )

  # one row per unit
  units_df <- data.frame(
    unit = sort(unique(mb$unit)),
    stringsAsFactors = FALSE
  )

  # full hourly spine: every hour of the year x every unit
  hours_spine <- data.frame(
    date_hour = seq.POSIXt(start_yr, end_yr - 3600, by = "hour")
  )
  hours_spine <- merge(hours_spine, units_df) # cross join

  # clip stays to the reference year and derive hourly bounds
  mb_clipped <- mb
  mb_clipped$start_hour <- pmax(
    mb_clipped$admission + 3600, # first full hour strictly after admission
    start_yr
  )
  mb_clipped$end_excl <- pmin(
    mb_clipped$discharge,
    end_yr
  )
  mb_clipped <- mb_clipped[mb_clipped$start_hour < mb_clipped$end_excl, ]

  n_outside_year <- nrow(mb) - nrow(mb_clipped)

  if (nrow(mb_clipped) == 0L) {
    stop(
      "No stays overlap with ",
      year,
      " after clipping.",
      call. = FALSE
    )
  }

  # report exclusions
  excluded <- data.frame(
    n_input = n_input,
    n_no_unit = n_no_unit,
    n_no_admission = n_no_admission,
    n_no_discharge = n_no_discharge,
    n_excluded_missing = n_excluded_missing,
    n_outside_year = n_outside_year
  )
  message(sprintf(
    paste0(
      "summarise_occupancy: %d of %d cases used. ",
      "Excluded: %d for missing unit/admission/discharge, ",
      "%d not overlapping %d."
    ),
    nrow(mb_clipped),
    n_input,
    n_excluded_missing,
    n_outside_year,
    year
  ))

  # expand each stay to the hours it covers
  hour_seqs <- purrr::map2(
    mb_clipped$start_hour,
    mb_clipped$end_excl,
    function(s, e) seq.POSIXt(s, e - 3600, by = "hour")
  )

  expanded <- data.frame(
    unit = rep(mb_clipped$unit, lengths(hour_seqs)),
    date_hour = do.call(c, hour_seqs)
  )

  # hourly census per unit
  hourly_census <- aggregate(
    rep(1L, nrow(expanded)),
    by = list(unit = expanded$unit, date_hour = expanded$date_hour),
    FUN = sum
  )
  names(hourly_census)[3] <- "n_patients"

  # fill in zeros for hours with no patients
  hourly_census <- merge(
    hours_spine,
    hourly_census,
    by = c("unit", "date_hour"),
    all.x = TRUE
  )
  hourly_census$n_patients[is.na(hourly_census$n_patients)] <- 0L

  # unit size: 1% rule
  # 1% of hours in a year = 8760 * 0.01 = 87.6, rounded up to 88
  threshold <- ceiling(0.01 * length(unique(hours_spine$date_hour)))

  hour_counts <- aggregate(
    n_patients ~ unit + n_patients,
    data = hourly_census,
    FUN = length
  )
  names(hour_counts)[3] <- "n_hours"

  eligible <- hour_counts[hour_counts$n_hours >= threshold, ]

  unit_size <- aggregate(
    n_patients ~ unit,
    data = eligible,
    FUN = max
  )
  names(unit_size)[2] <- "unit_size"

  # daily census: average hourly counts within each day
  hourly_census$date <- as.Date(hourly_census$date_hour, tz = "UTC")

  daily_census <- aggregate(
    n_patients ~ unit + date,
    data = hourly_census,
    FUN = mean
  )

  # annual summary per unit
  annual <- aggregate(
    n_patients ~ unit,
    data = daily_census,
    FUN = function(x) c(mean = mean(x), max = max(x))
  )
  # aggregate returns a matrix column; flatten
  annual <- data.frame(
    unit = annual$unit,
    census_mean = annual$n_patients[, "mean"],
    census_max = annual$n_patients[, "max"]
  )

  # join unit size and compute occupancy rates
  result <- merge(annual, unit_size, by = "unit", all.x = TRUE)
  result$occ_rate_mean <- result$census_mean / result$unit_size
  result$occ_rate_max <- result$census_max / result$unit_size

  # reorder columns
  result <- result[, c(
    "unit",
    "unit_size",
    "census_mean",
    "census_max",
    "occ_rate_mean",
    "occ_rate_max"
  )]

  result <- result[order(result$unit), ]

  # attach exclusion tally (dataset-level, so not a per-unit column)
  attr(result, "excluded") <- excluded

  result
}
