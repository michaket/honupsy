#' Daily Unit Occupancy Over a Calendar Year
#'
#' Expands each inpatient stay to hourly intervals and returns the daily
#' patient census per unit: one row per unit and day of the reference year.
#' This is the shared computational core used by \code{\link{summarise_occupancy}}
#' and is the natural input for plotting occupancy over time.
#'
#' Unit size is estimated from the data using the 1\% rule: the largest
#' occupancy level observed during at least 1\% of all hours in the year
#' (88 hours). The hourly census follows a right-open convention: a patient is
#' counted in hour \code{h} if \code{admission < h + 1} and \code{discharge > h}.
#' Each daily \code{census} is the mean of the 24 hourly counts for that day, so
#' it is an average daily census rather than a snapshot.
#'
#' Days on which a unit held no patients are present with \code{census = 0}, so
#' the series is gap-free across the year. Occupancy rate may legitimately
#' exceed 1 when more patients are present than the estimated capacity.
#'
#' This function requires \code{purrr} and can be slow for large datasets
#' (thousands of cases x 8760 hours). Consider running it once and caching the
#' result.
#'
#' The input must be the output of \code{\link{import_anq}} after unit
#' assignments have been attached with \code{\link{assign_units}}. Cases
#' without a unit assignment or with missing admission/discharge are excluded
#' before expansion; cases whose stay does not overlap the reference year are
#' dropped during clipping. Both counts are reported via a message and stored
#' on the result as the attribute \code{"excluded"}.
#'
#' @param data Named list. Output of \code{\link{import_anq}} with unit
#'   assignments attached via \code{\link{assign_units}}.
#' @param year Integer. The calendar year to summarise (e.g. \code{2022}).
#'
#' @return A named list with three elements:
#' \describe{
#'   \item{daily}{A \code{data.frame} with one row per unit and day, sorted by
#'     unit then date, with columns \code{unit}, \code{date}, \code{census}
#'     (mean of the 24 hourly counts) and \code{occ_rate}
#'     (\code{census / unit_size}).}
#'   \item{unit_size}{A \code{data.frame} of \code{unit} and its estimated
#'     capacity (1\% rule).}
#'   \item{excluded}{A one-row \code{data.frame} with the exclusion tally; see
#'     \code{\link{summarise_occupancy}}.}
#' }
#'
#' @seealso \code{\link{summarise_occupancy}}
#'
#' @importFrom purrr map2
#' @importFrom stats aggregate
#' @export
#'
#' @examples
#' \dontrun{
#' data  <- import_anq("data.txt")
#' units <- import_unit_assignments("units.xlsx")
#' data  <- assign_units(data, units)
#' occ <- occupancy_daily(data, year = 2022)
#' head(occ$daily)
#' occ$unit_size
#' occ$excluded
#' }
occupancy_daily <- function(data, year) {
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
  start_yr <- as.POSIXct(paste0(year, "-01-01 00:00:00"), tz = "UTC")
  end_yr <- as.POSIXct(paste0(year + 1L, "-01-01 00:00:00"), tz = "UTC")

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
  mb_clipped$end_excl <- pmin(mb_clipped$discharge, end_yr)
  mb_clipped <- mb_clipped[mb_clipped$start_hour < mb_clipped$end_excl, ]

  n_outside_year <- nrow(mb) - nrow(mb_clipped)

  if (nrow(mb_clipped) == 0L) {
    stop("No stays overlap with ", year, " after clipping.", call. = FALSE)
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
      "occupancy_daily: %d of %d cases used. ",
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

  unit_size <- aggregate(n_patients ~ unit, data = eligible, FUN = max)
  names(unit_size)[2] <- "unit_size"

  # daily census: average hourly counts within each day
  hourly_census$date <- as.Date(hourly_census$date_hour, tz = "UTC")

  daily <- aggregate(
    n_patients ~ unit + date,
    data = hourly_census,
    FUN = mean
  )
  names(daily)[names(daily) == "n_patients"] <- "census"

  # occupancy rate per day (unit_size constant within a unit)
  daily <- merge(daily, unit_size, by = "unit", all.x = TRUE)
  daily$occ_rate <- daily$census / daily$unit_size
  daily$unit_size <- NULL

  daily <- daily[
    order(daily$unit, daily$date),
    c(
      "unit",
      "date",
      "census",
      "occ_rate"
    )
  ]
  rownames(daily) <- NULL

  list(
    daily = daily,
    unit_size = unit_size[order(unit_size$unit), ],
    excluded = excluded
  )
}
