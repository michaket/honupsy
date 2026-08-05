#' Summarise HoNOS Severity at Admission by Unit
#'
#' Computes unit-level HoNOS severity indicators from admission assessments
#' (time point 1, non-dropout). Returns one row per unit with completion and
#' data-quality counts, mean item scores, proportion of severe cases per item
#' (score > 2), and mean total score.
#'
#' Item values of 9 ("not known / not applicable") are treated as missing and
#' excluded from means. When computing the severe indicator, missing values
#' (including 9s) are treated as non-severe (0), so that missingness does not
#' inflate severity proportions.
#'
#' The input must be the output of \code{\link{import_anq}} after unit
#' assignments have been attached with \code{\link{assign_units}}. Cases
#' without a unit assignment or without an admission HoNOS assessment are
#' included in \code{n_cases} but contribute \code{NA} to the HoNOS means.
#'
#' @param data Named list. Output of \code{\link{import_anq}} with unit
#'   assignments attached via \code{\link{assign_units}}.
#'
#' @return A data frame with one row per unit and the following columns:
#'
#'   - `unit`: unit identifier (character).
#'   - `n_cases`: number of cases on the unit (from MB).
#'   - `n_honos_adm`: number of cases with a valid admission HoNOS assessment.
#'   - `prop_assessed`: proportion of cases with a valid admission HoNOS
#'     assessment (`n_honos_adm / n_cases`). Quantifies how much of the unit the
#'     severity figures actually describe.
#'   - `mean_items_rated`: mean number of HoNOS items rated (0-12) among
#'     assessed cases. Below 12 indicates items coded 9 or left blank. `NA` for
#'     units with no admission assessment.
#'   - `n_partial`: number of assessed cases with fewer than 12 items rated,
#'     i.e. assessments whose total is summed over an incomplete item set.
#'   - `h1_mean`, ..., `h12_mean`: mean score per HoNOS item at admission
#'     (9s excluded).
#'   - `h1_prop_severe`, ..., `h12_prop_severe`: proportion of cases with a
#'     severe score (> 2) per item. Missing and 9-coded values count as
#'     non-severe.
#'   - `honos_total_mean`: mean total HoNOS score at admission.
#'
#' @seealso \code{\link{summarise_composition}},
#'   \code{\link{summarise_occupancy}}
#'
#' @importFrom dplyr left_join filter mutate summarise group_by across
#'   starts_with n
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' data  <- import_anq("data.txt")
#' units <- import_unit_assignments("units.xlsx")
#' data  <- assign_units(data, units)
#' summarise_honos_severity(data)
#' }
summarise_honos_severity <- function(data) {
  mb <- .extract_mb(data)
  ph <- .extract_ph(data)

  # HoNOS item column names as they appear in $ph
  item_cols <- c(
    "h1_aggression",
    "h2_self_harm",
    "h3_substance_use",
    "h4_cognitive",
    "h5_physical",
    "h6_hallucination",
    "h7_mood",
    "h8_other_mental",
    "h9_relationships",
    "h10_daily_living",
    "h11_living_conditions",
    "h12_occupation"
  )

  # admission assessments only, no dropouts
  ph_adm <- ph |>
    dplyr::filter(
      .data$time_point == 1L,
      !.data$is_dropout
    )

  # case counts from MB (source of truth for n_cases)
  unit_counts <- mb |>
    dplyr::filter(!is.na(.data$unit)) |>
    dplyr::group_by(.data$unit) |>
    dplyr::summarise(n_cases = dplyr::n(), .groups = "drop")

  # join unit onto PH via MB (PH does not carry unit directly after assign_units
  # only joins onto mb/ph/pb/fm -- but assign_units does add unit to ph too)
  ph_adm_unit <- ph_adm |>
    dplyr::filter(!is.na(.data$unit))

  # compute severe indicators (9 / NA -> 0, >2 -> 1)
  severe_cols <- paste0(item_cols, "_severe")
  ph_adm_unit <- ph_adm_unit |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(item_cols),
        ~ dplyr::if_else(.x > 2L, 1L, 0L, missing = 0L),
        .names = "{.col}_severe"
      )
    )

  # aggregate
  ph_summary <- ph_adm_unit |>
    dplyr::group_by(.data$unit) |>
    dplyr::summarise(
      n_honos_adm = dplyr::n(),
      # data quality: how complete are the assessments behind the means
      mean_items_rated = mean(.data$honos_n_valid, na.rm = TRUE),
      n_partial = sum(.data$honos_n_valid < 12L, na.rm = TRUE),
      # means (NA for 9-coded values, already recoded in parser)
      dplyr::across(
        dplyr::all_of(item_cols),
        ~ mean(.x, na.rm = TRUE),
        .names = "{.col}_mean"
      ),
      # proportions severe
      dplyr::across(
        dplyr::all_of(severe_cols),
        ~ mean(.x, na.rm = TRUE),
        .names = "{.col}_prop"
      ),
      honos_total_mean = mean(.data$honos_total, na.rm = TRUE),
      .groups = "drop"
    )

  # rename: h1_aggression_mean -> h1_mean, h1_aggression_severe_prop ->
  # h1_prop_severe (cleaner output column names)
  item_short <- paste0("h", 1:12)
  old_mean <- paste0(item_cols, "_mean")
  new_mean <- paste0(item_short, "_mean")
  old_severe <- paste0(item_cols, "_severe_prop")
  new_severe <- paste0(item_short, "_prop_severe")

  names(ph_summary)[match(old_mean, names(ph_summary))] <- new_mean
  names(ph_summary)[match(old_severe, names(ph_summary))] <- new_severe

  # join onto unit counts so units with no HoNOS still appear
  result <- dplyr::left_join(unit_counts, ph_summary, by = "unit")

  # units with no admission HoNOS: 0 assessed, not NA
  result$n_honos_adm[is.na(result$n_honos_adm)] <- 0L
  result$n_partial[is.na(result$n_partial)] <- 0L

  # completion rate (mean_items_rated stays NA where there is nothing to rate)
  result$prop_assessed <- result$n_honos_adm / result$n_cases

  # put the quality block up front, keep honos_total_mean last
  front <- c(
    "unit",
    "n_cases",
    "n_honos_adm",
    "prop_assessed",
    "mean_items_rated",
    "n_partial"
  )
  result <- result[, c(front, setdiff(names(result), front))]

  as.data.frame(result)
}
