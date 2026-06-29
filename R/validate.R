#' Validate an imported ANQ submission
#' @param data named list with mb, mp, ph, pb, fm data.frames
#' @return invisibly a named list of validation messages per section,
#'         warnings are also issued
#' @importFrom utils head
#' @noRd
validate_anq <- function(data) {
  msgs <- list(
    structure = validate_structure(data),
    fid = validate_fid(data),
    mb = if (!is.null(data$mb)) validate_mb(data$mb) else character(),
    ph = if (!is.null(data$ph)) validate_ph(data$ph) else character(),
    pb = if (!is.null(data$pb)) validate_pb(data$pb) else character(),
    fm = if (!is.null(data$fm)) validate_fm(data$fm) else character()
  )

  all_msgs <- unlist(msgs, use.names = FALSE)

  if (length(all_msgs) > 0) {
    warning(
      length(all_msgs),
      " validation issue(s):\n",
      paste("-", all_msgs, collapse = "\n"),
      call. = FALSE
    )
  }

  invisible(msgs)
}

# -- Structure -----------------------------------------------------------------

#' Check whether all expected datasets are present
#' @noRd
validate_structure <- function(data) {
  msgs <- character()

  # MB, MP and PH are always expected
  # PB and FM are optional -- absence is clinically valid
  expected <- c("mb", "mp", "ph")
  missing <- expected[!expected %in% names(data)]

  if (length(missing) > 0) {
    msgs <- c(
      msgs,
      paste0(
        "Missing datasets: ",
        paste(toupper(missing), collapse = ", ")
      )
    )
  }

  # MB and MP must have the same number of cases
  if (!is.null(data$mb) && !is.null(data$mp)) {
    if (nrow(data$mb) != nrow(data$mp)) {
      msgs <- c(
        msgs,
        sprintf(
          "MB and MP have different numbers of cases: MB=%d, MP=%d",
          nrow(data$mb),
          nrow(data$mp)
        )
      )
    }
  }

  # PH should have twice as many rows as MB (admission + discharge)
  # dropouts may shift this, so only warn
  if (!is.null(data$mb) && !is.null(data$ph)) {
    n_mb <- nrow(data$mb)
    n_ph <- nrow(data$ph)
    if (n_ph > n_mb * 2L) {
      msgs <- c(
        msgs,
        sprintf(
          "PH has more rows than expected: %d rows for %d cases (max. %d expected)",
          n_ph,
          n_mb,
          n_mb * 2L
        )
      )
    }
  }

  msgs
}

# -- FID consistency -----------------------------------------------------------

#' Check FID consistency across all datasets
#' @noRd
validate_fid <- function(data) {
  msgs <- character()

  if (is.null(data$mb)) {
    return(msgs)
  }

  # use raw FIDs for duplicate check, unique for set operations below
  fid_mb_raw <- data$mb$fid
  fid_mb <- unique(fid_mb_raw)

  # FIDs without a value
  if (any(is.na(fid_mb_raw) | fid_mb_raw == "")) {
    msgs <- c(
      msgs,
      sprintf(
        "%d MB rows without FID",
        sum(is.na(fid_mb_raw) | fid_mb_raw == "")
      )
    )
  }

  # duplicate FIDs in MB
  duplicated_fids <- fid_mb_raw[duplicated(fid_mb_raw)]
  if (length(duplicated_fids) > 0) {
    msgs <- c(
      msgs,
      sprintf(
        "%d duplicate FIDs in MB: %s",
        length(duplicated_fids),
        paste(head(unique(duplicated_fids), 5), collapse = ", ")
      )
    )
  }

  # MP: FIDs not present in MB
  if (!is.null(data$mp)) {
    fid_mp <- unique(data$mp$fid)
    only_mp <- setdiff(fid_mp, fid_mb)
    only_mb <- setdiff(fid_mb, fid_mp)
    if (length(only_mp) > 0) {
      msgs <- c(
        msgs,
        sprintf(
          "%d FIDs in MP without a corresponding MB row",
          length(only_mp)
        )
      )
    }
    if (length(only_mb) > 0) {
      msgs <- c(
        msgs,
        sprintf(
          "%d FIDs in MB without a corresponding MP row",
          length(only_mb)
        )
      )
    }
  }

  # PH: FIDs not present in MB
  if (!is.null(data$ph)) {
    fid_ph <- unique(data$ph$fid)
    only_ph <- setdiff(fid_ph, fid_mb)
    only_mb <- setdiff(fid_mb, fid_ph)
    if (length(only_ph) > 0) {
      msgs <- c(
        msgs,
        sprintf(
          "%d FIDs in PH without a corresponding MB row",
          length(only_ph)
        )
      )
    }
    if (length(only_mb) > 0) {
      msgs <- c(
        msgs,
        sprintf(
          "%d FIDs in MB without any PH rows (neither admission nor discharge)",
          length(only_mb)
        )
      )
    }
  }

  # PB: FIDs not present in MB
  if (!is.null(data$pb) && nrow(data$pb) > 0) {
    fid_pb <- unique(data$pb$fid)
    only_pb <- setdiff(fid_pb, fid_mb)
    if (length(only_pb) > 0) {
      msgs <- c(
        msgs,
        sprintf(
          "%d FIDs in PB without a corresponding MB row",
          length(only_pb)
        )
      )
    }
    # no warning for MB FIDs without PB -- PB is optional
  }

  # FM: FIDs not present in MB
  if (!is.null(data$fm) && nrow(data$fm) > 0) {
    fid_fm <- unique(data$fm$fid)
    only_fm <- setdiff(fid_fm, fid_mb)
    if (length(only_fm) > 0) {
      msgs <- c(
        msgs,
        sprintf(
          "%d FIDs in FM without a corresponding MB row",
          length(only_fm)
        )
      )
    }
  }

  msgs
}

# -- MB ------------------------------------------------------------------------

#' Content plausibility checks for MB
#' @noRd
validate_mb <- function(mb) {
  msgs <- character()

  # psychiatry flag should always be 1
  if (!is.null(mb$flag_psychiatry)) {
    n_wrong <- sum(!is.na(mb$flag_psychiatry) & mb$flag_psychiatry != 1L)
    if (n_wrong > 0) {
      msgs <- c(
        msgs,
        sprintf(
          "%d MB rows with flag_psychiatry != 1",
          n_wrong
        )
      )
    }
  }

  # main cost centre should be M500
  if (!is.null(mb$main_cost_centre)) {
    n_wrong <- sum(!is.na(mb$main_cost_centre) & mb$main_cost_centre != "M500")
    if (n_wrong > 0) {
      msgs <- c(
        msgs,
        sprintf(
          "%d MB rows with main_cost_centre != M500",
          n_wrong
        )
      )
    }
  }

  # age plausible (adult psychiatry: >= 18)
  if (!is.null(mb$age_admission)) {
    n_young <- sum(!is.na(mb$age_admission) & mb$age_admission < 18L)
    if (n_young > 0) {
      msgs <- c(
        msgs,
        sprintf(
          "%d cases with age < 18 (adult psychiatry expected)",
          n_young
        )
      )
    }
  }

  # discharge after admission
  if (!is.null(mb$admission) && !is.null(mb$discharge)) {
    n_wrong <- sum(
      !is.na(mb$admission) & !is.na(mb$discharge) & mb$discharge < mb$admission
    )
    if (n_wrong > 0) {
      msgs <- c(
        msgs,
        sprintf(
          "%d cases with discharge date before admission date",
          n_wrong
        )
      )
    }
  }

  msgs
}

# -- PH ------------------------------------------------------------------------

#' Content plausibility checks for PH
#' @noRd
validate_ph <- function(ph) {
  msgs <- character()
  if (nrow(ph) == 0) {
    return(msgs)
  }
  # per FID: exactly one admission and one discharge row expected
  ph_no_dropout <- ph[!ph$is_dropout, ]

  if (nrow(ph_no_dropout) > 0) {
    counts <- table(ph_no_dropout$fid, ph_no_dropout$time_point)

    # duplicate admission assessments
    if ("1" %in% colnames(counts)) {
      n_dup <- sum(counts[, "1"] > 1L)
      if (n_dup > 0) {
        msgs <- c(
          msgs,
          sprintf(
            "%d FIDs with more than one admission assessment (PH)",
            n_dup
          )
        )
      }
    }

    # duplicate discharge assessments
    if ("2" %in% colnames(counts)) {
      n_dup <- sum(counts[, "2"] > 1L)
      if (n_dup > 0) {
        msgs <- c(
          msgs,
          sprintf(
            "%d FIDs with more than one discharge assessment (PH)",
            n_dup
          )
        )
      }
    }
  }

  # HoNOS raw item values must be in valid range (0-4 or 9)
  item_cols <- paste0("h", c(1:8, 9:12), "_raw")
  item_cols <- item_cols[item_cols %in% names(ph)]

  for (col in item_cols) {
    n_invalid <- sum(
      !is.na(ph[[col]]) &
        !(ph[[col]] %in% 0:4) &
        ph[[col]] != 9L
    )
    if (n_invalid > 0) {
      msgs <- c(
        msgs,
        sprintf(
          "%d invalid values in %s (expected 0-4 or 9)",
          n_invalid,
          col
        )
      )
    }
  }

  msgs
}

# -- PB ------------------------------------------------------------------------

#' Content plausibility checks for PB
#' @noRd
validate_pb <- function(pb) {
  msgs <- character()
  if (nrow(pb) == 0) {
    return(msgs)
  }
  # per FID: exactly one admission and one discharge row expected
  pb_no_dropout <- pb[!pb$is_dropout, ]

  if (nrow(pb_no_dropout) > 0) {
    counts <- table(pb_no_dropout$fid, pb_no_dropout$time_point)

    # duplicate admission assessments
    if ("1" %in% colnames(counts)) {
      n_dup <- sum(counts[, "1"] > 1L)
      if (n_dup > 0) {
        msgs <- c(
          msgs,
          sprintf(
            "%d FIDs with more than one admission assessment (PB)",
            n_dup
          )
        )
      }
    }

    # duplicate discharge assessments
    if ("2" %in% colnames(counts)) {
      n_dup <- sum(counts[, "2"] > 1L)
      if (n_dup > 0) {
        msgs <- c(
          msgs,
          sprintf(
            "%d FIDs with more than one discharge assessment (PB)",
            n_dup
          )
        )
      }
    }
  }

  # BSCL item values must be in valid range (0-4)
  item_cols <- paste0("b", 1:53)
  item_cols <- item_cols[item_cols %in% names(pb)]

  for (col in item_cols) {
    n_invalid <- sum(
      !is.na(pb[[col]]) & !(pb[[col]] %in% 0:4)
    )
    if (n_invalid > 0) {
      msgs <- c(
        msgs,
        sprintf(
          "%d invalid values in %s (expected 0-4)",
          n_invalid,
          col
        )
      )
    }
  }

  msgs
}

# -- FM ------------------------------------------------------------------------

#' Content plausibility checks for FM
#' @noRd
validate_fm <- function(fm) {
  msgs <- character()

  if (nrow(fm) == 0) {
    return(msgs)
  }

  # unknown measure types
  known_types <- c(1L, 2L, 3L, 4L, 5L, 7L, 10L, 11L)
  n_unknown <- sum(!is.na(fm$measure_type) & !fm$measure_type %in% known_types)
  if (n_unknown > 0) {
    msgs <- c(
      msgs,
      sprintf(
        "%d FM rows with unknown measure type",
        n_unknown
      )
    )
  }

  # implausible duration (already set to NA, but count the rows)
  if (!is.null(fm$duration_min)) {
    n_implausible <- sum(
      is.na(fm$duration_min) &
        !fm$is_coercive_medication &
        !is.na(fm$start_date) &
        !is.na(fm$end_date)
    )
    if (n_implausible > 0) {
      msgs <- c(
        msgs,
        sprintf(
          "%d FM rows with implausible time values (duration_min = NA despite available data)",
          n_implausible
        )
      )
    }
  }

  # overlapping measures per FID (excluding involuntary medication)
  not_coercive <- fm[
    !fm$is_coercive_medication &
      !is.na(fm$start_date) &
      !is.na(fm$end_date),
  ]

  if (nrow(not_coercive) > 0) {
    n_overlaps <- 0L
    fids <- unique(not_coercive$fid)

    for (fid in fids) {
      fm_fid <- not_coercive[not_coercive$fid == fid, ]
      if (nrow(fm_fid) < 2L) {
        next
      }

      # check all pairs
      for (i in seq_len(nrow(fm_fid) - 1L)) {
        for (j in seq(i + 1L, nrow(fm_fid))) {
          b1 <- as.integer(fm_fid$start_date[i]) * 1440L + fm_fid$start_time[i]
          e1 <- as.integer(fm_fid$end_date[i]) * 1440L + fm_fid$end_time[i]
          b2 <- as.integer(fm_fid$start_date[j]) * 1440L + fm_fid$start_time[j]
          e2 <- as.integer(fm_fid$end_date[j]) * 1440L + fm_fid$end_time[j]

          if (b1 < e2 && b2 < e1) {
            n_overlaps <- n_overlaps + 1L
          }
        }
      }
    }

    if (n_overlaps > 0L) {
      msgs <- c(
        msgs,
        sprintf(
          "%d overlapping coercive measures found (excluding involuntary medication)",
          n_overlaps
        )
      )
    }
  }

  msgs
}
