#' Summarise HoNOS Severity by Unit
#'
#' Summarises routinely collected inpatient psychiatric data at the unit level.
#' The function treats the value 9 in Health of the Nation Outcome Scales (HoNOS)
#' items as missing, creates indicators for severe problems (scores > 2), and
#' returns the proportion of severe cases per HoNOS item and unit, alongside
#' mean age and mean length of stay.
#'
#' The expected input structure corresponds to the output of
#' \code{\link{sample_cases}()} or a similar data set with one row per case.
#'
#' @param data A data frame or tibble with one row per case, containing at least
#'   the following variables:
#'   \itemize{
#'     \item \code{unit}: Unit identifier (character or factor).
#'     \item \code{age}: Patient age (numeric).
#'     \item \code{admission}: Admission date (class \code{Date}).
#'     \item \code{discharge}: Discharge date (class \code{Date}).
#'     \item \code{honos_1}, \code{honos_2}, \dots, \code{honos_12}: HoNOS item
#'       scores coded as integers, where 0–4 indicate severity and 9 indicates
#'       "not known" or missing.
#'   }
#'
#' @return
#' A tibble with one row per \code{unit} and the following columns:
#' \itemize{
#'   \item \code{prop_honos_1_severe}, \dots, \code{prop_honos_12_severe}:
#'     Proportion of cases on the unit with a severe score (> 2) on each HoNOS item.
#'   \item \code{age_mean}: Mean age on the unit.
#'   \item \code{los_mean}: Mean length of stay in days on the unit.
#' }
#'
#' @seealso \code{\link{sample_cases}} to generate example data.
#'
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' # Generate example data and summarise HoNOS severity by unit
#' cases <- sample_cases(n = 200)
#' summarise_honos_units(cases)
summarise_honos_units <- function(data = data) {
  data |>
    # Recode 9 → NA for HoNOS items
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("honos_"),
        ~ dplyr::case_match(.x, 9L ~ NA_integer_, .default = .x)
      )
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      # Create severe indicators; HoNOS = 9 ("not known / not applicable")
      # is treated as 0 (not severe) in the dichotomised variable
      dplyr::across(
        dplyr::starts_with("honos_"),
        ~ dplyr::if_else(.x > 2, 1L, 0L, missing = 0L),
        .names = "{.col}_severe"
      ),
      # Length of stay
      los = as.numeric(.data$discharge - .data$admission)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      honos_total_score = rowSums(
        dplyr::across(
          dplyr::starts_with("honos_") & !dplyr::ends_with("_severe")
        ),
        na.rm = TRUE
      )
    ) |>
    dplyr::group_by(.data$unit) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::starts_with("honos_") & dplyr::ends_with("_severe"),
        ~ mean(.x, na.rm = TRUE),
        .names = "prop_{.col}"
      ),
      age_mean = mean(.data$age, na.rm = TRUE),
      los_mean = mean(.data$los, na.rm = TRUE),
      .groups = "drop"
    )
}
