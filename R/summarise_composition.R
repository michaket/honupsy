# internal: min/max that return NA (not Inf/-Inf) when every value is missing,
# so an all-missing column does not emit a base-R warning or a misleading bound
safe_min <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

#' Summarise Case Composition by Unit
#'
#' Computes case-level descriptive statistics per unit from the MB dataset.
#' Returns one row per unit.
#'
#' The input must be the output of \code{\link{import_anq}} after unit
#' assignments have been attached with \code{\link{assign_units}}. Cases
#' without a unit assignment (\code{unit = NA}) are silently excluded.
#'
#' Length of stay is computed as the number of days between admission and
#' discharge (i.e. \code{difftime(discharge, admission, units = "days")}).
#' All summary statistics are taken over non-missing values: cases with a
#' missing component are still counted in \code{n_cases} but are excluded from
#' the relevant statistic, and \code{prop_female} is computed only over cases
#' with known sex. The \code{n_*_missing} columns report how many cases were
#' excluded from each statistic. Given the structure of the ANQ MB dataset
#' these fields are effectively always populated, so in practice these counts
#' are normally zero and serve mainly as a check.
#'
#' @param data Named list. Output of \code{\link{import_anq}} with unit
#'   assignments attached via \code{\link{assign_units}}.
#' @param detail Character; level of detail in the output. \code{"summary"}
#'   (the default) returns the lean set of columns. \code{"full"} additionally
#'   returns the median, standard deviation, minimum, maximum, 25th and 75th
#'   percentiles, and IQR for both age and length of stay.
#'
#' @return A \code{data.frame} with one row per unit. With
#'   \code{detail = "summary"} the columns are \code{unit}, \code{n_cases},
#'   \code{n_age_missing}, \code{n_los_missing}, \code{n_sex_missing},
#'   \code{age_mean}, \code{los_mean}, \code{n_female}, \code{prop_female}.
#'   With \code{detail = "full"} the distributional statistics
#'   (\code{age_median}, \code{age_sd}, \code{age_min}, \code{age_max},
#'   \code{age_q25}, \code{age_q75}, \code{age_iqr}, and the corresponding
#'   \code{los_*} columns) are added. The \code{n_*_missing} columns count
#'   cases on the unit with a missing age, length of stay (missing admission
#'   or discharge date), or sex, respectively.
#'
#' @seealso \code{\link{summarise_honos_severity}},
#'   \code{\link{summarise_occupancy}}
#'
#' @importFrom dplyr filter mutate summarise group_by n .data
#' @export
#'
#' @examples
#' \dontrun{
#' data  <- import_anq("data.txt")
#' units <- import_unit_assignments("units.xlsx")
#' data  <- assign_units(data, units)
#' summarise_composition(data)                 # lean default
#' summarise_composition(data, detail = "full") # full distribution
#' }
summarise_composition <- function(data, detail = c("summary", "full")) {
  detail <- match.arg(detail)

  mb <- .extract_mb(data)

  result <- mb |>
    dplyr::filter(!is.na(.data$unit)) |>
    dplyr::mutate(
      los = as.numeric(
        difftime(.data$discharge, .data$admission, units = "days")
      )
    ) |>
    dplyr::group_by(.data$unit) |>
    dplyr::summarise(
      n_cases = dplyr::n(),
      # data quality: cases excluded from each statistic for missingness
      n_age_missing = sum(is.na(.data$age_admission)),
      n_los_missing = sum(is.na(.data$los)),
      n_sex_missing = sum(is.na(.data$sex)),
      age_mean = mean(.data$age_admission, na.rm = TRUE),
      age_median = stats::median(.data$age_admission, na.rm = TRUE),
      age_sd = stats::sd(.data$age_admission, na.rm = TRUE),
      age_min = safe_min(.data$age_admission),
      age_max = safe_max(.data$age_admission),
      age_q25 = stats::quantile(.data$age_admission, 0.25, na.rm = TRUE),
      age_q75 = stats::quantile(.data$age_admission, 0.75, na.rm = TRUE),
      age_iqr = stats::IQR(.data$age_admission, na.rm = TRUE),
      los_mean = mean(.data$los, na.rm = TRUE),
      los_median = stats::median(.data$los, na.rm = TRUE),
      los_sd = stats::sd(.data$los, na.rm = TRUE),
      los_min = safe_min(.data$los),
      los_max = safe_max(.data$los),
      los_q25 = stats::quantile(.data$los, 0.25, na.rm = TRUE),
      los_q75 = stats::quantile(.data$los, 0.75, na.rm = TRUE),
      los_iqr = stats::IQR(.data$los, na.rm = TRUE),
      n_female = sum(.data$sex == 2L, na.rm = TRUE),
      prop_female = sum(.data$sex == 2L, na.rm = TRUE) /
        sum(!is.na(.data$sex)),
      .groups = "drop"
    )

  result <- as.data.frame(result)

  if (detail == "summary") {
    result <- result[, c(
      "unit",
      "n_cases",
      "n_age_missing",
      "n_los_missing",
      "n_sex_missing",
      "age_mean",
      "los_mean",
      "n_female",
      "prop_female"
    )]
  }

  result
}
