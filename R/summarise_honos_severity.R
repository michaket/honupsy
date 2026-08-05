#' Summarise HoNOS Severity at Admission by Unit
#'
#' Computes unit-level HoNOS severity indicators from admission assessments
#' (time point 1, non-dropout). Returns one row per unit with completion and
#' data-quality counts, mean item scores, proportion of severe cases per item
#' (score > 2), and mean total score.
#'
#' HoNOS item scores of 3 or 4 are classified as severe. Severe-item
#' proportions use all cases in the MB dataset assigned to the respective
#' unit as the denominator, reproducing the operationalisation used in the
#' associated study. Cases without a non-dropout admission HoNOS assessment,
#' and individual items that are missing or coded 9, are classified as
#' non-severe for these proportions. Missing values are excluded when
#' calculating item means.
#'
#' Item means and severe-item proportions therefore use different
#' missing-data rules: item means are based on observed ratings, whereas
#' severe-item proportions use all cases assigned to the unit as their
#' denominator.
#'
#' The input must be the output of \code{\link{import_anq}} after unit
#' assignments have been attached with \code{\link{assign_units}}. Cases
#' without an admission HoNOS assessment remain included in
#' \code{n_cases} and in the denominator of the severe-item proportions, but
#' do not contribute to HoNOS item means or total-score summaries.
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
#'   - `h1_prop_severe`, ..., `h12_prop_severe`: proportion of all cases
#'     assigned to the unit with a severe admission score (3 or 4) on the
#'     respective item. Cases without a non-dropout admission HoNOS
#'     assessment and missing or 9-coded item values count as non-severe.
#'   - `honos_total_mean`: mean admission HoNOS total, where each patient's
#'     total is the sum of available item scores. Missing and 9-coded items
#'     are omitted rather than prorated to 12 items. Assessments with no
#'     valid HoNOS items do not contribute to the mean.
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

  duplicate_adm <- unique(
    ph_adm$fid[duplicated(ph_adm$fid)]
  )

  if (length(duplicate_adm) > 0L) {
    stop(
      "Multiple non-dropout admission HoNOS records found for FID(s): ",
      paste(head(duplicate_adm, 5L), collapse = ", "),
      ". Cannot calculate unit-level HoNOS summaries unambiguously.",
      call. = FALSE
    )
  }

  # case counts from MB (source of truth for n_cases)
  unit_counts <- mb |>
    dplyr::filter(!is.na(.data$unit)) |>
    dplyr::group_by(.data$unit) |>
    dplyr::summarise(n_cases = dplyr::n(), .groups = "drop")

  # join unit onto PH via MB (PH does not carry unit directly after assign_units
  # only joins onto mb/ph/pb/fm -- but assign_units does add unit to ph too)
  ph_adm_unit <- ph_adm |>
    dplyr::filter(!is.na(.data$unit))

  # Severe-item proportions reproduce the study operationalisation:
  # denominator = all MB cases assigned to the unit.
  severity_cases <- mb |>
    dplyr::filter(!is.na(.data$unit)) |>
    dplyr::select("fid", "unit") |>
    dplyr::left_join(
      ph_adm |>
        dplyr::select(
          "fid",
          dplyr::all_of(item_cols)
        ),
      by = "fid"
    )

  severe_cols <- paste0(item_cols, "_severe")

  severity_cases <- severity_cases |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(item_cols),
        ~ as.integer(!is.na(.x) & .x %in% 3:4),
        .names = "{.col}_severe"
      )
    )

  severity_summary <- severity_cases |>
    dplyr::group_by(.data$unit) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(severe_cols),
        ~ mean(.x),
        .names = "{.col}_prop"
      ),
      .groups = "drop"
    )

  # aggregate
  ph_summary <- ph_adm_unit |>
    dplyr::group_by(.data$unit) |>
    dplyr::summarise(
      n_honos_adm = dplyr::n(),
      mean_items_rated = mean(.data$honos_n_valid, na.rm = TRUE),
      n_partial = sum(.data$honos_n_valid < 12L, na.rm = TRUE),

      dplyr::across(
        dplyr::all_of(item_cols),
        ~ mean(.x, na.rm = TRUE),
        .names = "{.col}_mean"
      ),

      honos_total_mean = mean(.data$honos_total, na.rm = TRUE),
      .groups = "drop"
    )

  # join onto unit counts so units with no HoNOS still appear
  result <- unit_counts |>
    dplyr::left_join(ph_summary, by = "unit") |>
    dplyr::left_join(severity_summary, by = "unit")

  # rename: h1_aggression_mean -> h1_mean,
  # h1_aggression_severe_prop -> h1_prop_severe
  item_short <- paste0("h", 1:12)
  old_mean <- paste0(item_cols, "_mean")
  new_mean <- paste0(item_short, "_mean")
  old_severe <- paste0(item_cols, "_severe_prop")
  new_severe <- paste0(item_short, "_prop_severe")

  names(result)[match(old_mean, names(result))] <- new_mean
  names(result)[match(old_severe, names(result))] <- new_severe

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
