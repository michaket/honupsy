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
#' with known sex. Given the structure of the ANQ MB dataset these fields are
#' effectively always populated, so in practice the exclusion has no effect.
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
#'   \code{age_mean}, \code{los_mean}, \code{n_female}, \code{prop_female}.
#'   With \code{detail = "full"} the distributional statistics
#'   (\code{age_median}, \code{age_sd}, \code{age_min}, \code{age_max},
#'   \code{age_q25}, \code{age_q75}, \code{age_iqr}, and the corresponding
#'   \code{los_*} columns) are added.
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
      age_mean = mean(.data$age_admission, na.rm = TRUE),
      age_median = stats::median(.data$age_admission, na.rm = TRUE),
      age_sd = stats::sd(.data$age_admission, na.rm = TRUE),
      age_min = min(.data$age_admission, na.rm = TRUE),
      age_max = max(.data$age_admission, na.rm = TRUE),
      age_q25 = stats::quantile(.data$age_admission, 0.25, na.rm = TRUE),
      age_q75 = stats::quantile(.data$age_admission, 0.75, na.rm = TRUE),
      age_iqr = stats::IQR(.data$age_admission, na.rm = TRUE),
      los_mean = mean(.data$los, na.rm = TRUE),
      los_median = stats::median(.data$los, na.rm = TRUE),
      los_sd = stats::sd(.data$los, na.rm = TRUE),
      los_min = min(.data$los, na.rm = TRUE),
      los_max = max(.data$los, na.rm = TRUE),
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
      "age_mean",
      "los_mean",
      "n_female",
      "prop_female"
    )]
  }

  result
}
