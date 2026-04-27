# parse_fm.R

#' Parse FM records into a data frame
#' @param records list of character vectors, FM rows from read_anq_raw()
#' @return data.frame with one row per measure (0 to n rows per case)
#' @noRd
parse_fm <- function(records) {
  rows <- lapply(records, parse_fm_row)
  rows <- Filter(Negate(is.null), rows)

  if (length(rows) == 0) {
    # no warning here -- it is perfectly normal for a case to have no FM records
    return(empty_fm())
  }

  do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
}

#' Parse a single FM row
#' @noRd
parse_fm_row <- function(r) {
  if (length(r) < 6) {
    warning(
      "FM row has only ",
      length(r),
      " fields (at least 6 expected), ",
      "skipping. First fields: ",
      paste(r[seq_len(min(3, length(r)))], collapse = "|"),
      call. = FALSE
    )
    return(NULL)
  }

  measure_type <- parse_int(r[[4]]) # ANQ: 8.01.V04

  # involuntary medication (3=oral, 4=injection) has no end date/time
  is_coercive_medication <- !is.na(measure_type) && measure_type %in% c(3L, 4L)

  # start is mandatory for all FM
  start_date <- parse_date_anq(r[[5]]) # ANQ: 8.01.V05
  start_time <- parse_time_anq(r[[6]]) # ANQ: 8.01.V06

  # end only for non-involuntary-medication
  if (!is_coercive_medication && length(r) >= 8) {
    end_date <- parse_date_anq(r[[7]]) # ANQ: 8.01.V07
    end_time <- parse_time_anq(r[[8]]) # ANQ: 8.01.V08
  } else {
    end_date <- NA_real_
    end_time <- NA_integer_
  }

  # calculate duration in minutes where possible
  duration_min <- fm_duration(
    start_date,
    start_time,
    end_date,
    end_time,
    is_coercive_medication
  )

  list(
    # identification
    fid = trimws(r[[3]]), # ANQ: FID
    facility = trimws(r[[2]]), # ANQ: 8.01.V02

    # measure
    measure_type = measure_type,
    measure_label = coercive_measure_label(measure_type),
    is_coercive_medication = is_coercive_medication,

    # start
    start_date = start_date,
    start_time = start_time, # minutes since midnight

    # end (NA for involuntary medication)
    end_date = end_date,
    end_time = end_time, # minutes since midnight

    # duration (NA for involuntary medication or missing time values)
    duration_min = duration_min
  )
}

#' Calculate the duration of a coercive measure in minutes
#' @param start_date Date
#' @param start_time integer, minutes since midnight
#' @param end_date Date
#' @param end_time integer, minutes since midnight
#' @param is_coercive_medication logical
#' @return integer, duration in minutes, or NA
#' @noRd
fm_duration <- function(
  start_date,
  start_time,
  end_date,
  end_time,
  is_coercive_medication
) {
  # involuntary medication has no end point
  if (is_coercive_medication) {
    return(NA_integer_)
  }

  # missing time values
  if (
    is.na(start_date) || is.na(start_time) || is.na(end_date) || is.na(end_time)
  ) {
    return(NA_integer_)
  }

  # convert start and end to minutes since a common reference point
  start_total <- as.integer(start_date) * 1440L + start_time
  end_total <- as.integer(end_date) * 1440L + end_time
  duration <- end_total - start_total

  # negative duration is not plausible
  if (duration < 0L) {
    warning(
      "Negative duration for FM record: start ",
      format(start_date),
      " ",
      sprintf("%02d:%02d", start_time %/% 60L, start_time %% 60L),
      " end ",
      format(end_date),
      " ",
      sprintf("%02d:%02d", end_time %/% 60L, end_time %% 60L),
      call. = FALSE
    )
    return(NA_integer_)
  }

  duration
}

#' Empty FM data frame with correct column types
#' @noRd
empty_fm <- function() {
  data.frame(
    fid = character(),
    facility = character(),
    measure_type = integer(),
    measure_label = character(),
    is_coercive_medication = logical(),
    start_date = as.Date(character()),
    start_time = integer(),
    end_date = as.Date(character()),
    end_time = integer(),
    duration_min = integer(),
    stringsAsFactors = FALSE
  )
}
