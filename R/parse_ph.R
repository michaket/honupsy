#' Parse PH records into a data frame
#' @param records list of character vectors, PH rows from read_anq_raw()
#' @return data.frame with two rows per case (admission + discharge)
#' @importFrom stats setNames
#' @noRd
parse_ph <- function(records) {
  rows <- lapply(records, parse_ph_row)
  rows <- Filter(Negate(is.null), rows)

  if (length(rows) == 0) {
    warning("No valid PH rows found", call. = FALSE)
    return(empty_ph())
  }

  do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
}

#' Parse a single PH row
#' @noRd
parse_ph_row <- function(r) {
  # silently skip completely empty rows (common in Excel exports)
  if (all(trimws(r) == "")) {
    return(NULL)
  }
  if (length(r) < 7) {
    warning(
      "PH row has only ",
      length(r),
      " fields (at least 7 expected), ",
      "skipping. First fields: ",
      paste(r[seq_len(min(3, length(r)))], collapse = "|"),
      call. = FALSE
    )
    return(NULL)
  }

  time_point <- parse_int(r[[4]]) # ANQ: 4.01.V04, 1=admission, 2=discharge, 3=other
  dropout_code <- parse_int(r[[5]]) # ANQ: 4.01.V05, 0=no dropout, 1=<24h, 2=other
  is_dropout <- !is.na(dropout_code) && dropout_code != 0L

  if (!is_dropout && length(r) >= 21) {
    items <- lapply(
      list(
        r[[8]],
        r[[9]],
        r[[10]],
        r[[11]],
        r[[12]],
        r[[13]],
        r[[14]],
        r[[15]],
        r[[18]],
        r[[19]],
        r[[20]],
        r[[21]]
      ),
      parse_honos_item
    )

    names(items) <- c(
      "h1",
      "h2",
      "h3",
      "h4",
      "h5",
      "h6",
      "h7",
      "h8",
      "h9",
      "h10",
      "h11",
      "h12"
    )

    h8_type <- trimws(r[[16]]) # ANQ: 4.02.V09
    h8_detail <- trimws(r[[17]]) # ANQ: 4.02.V10
  } else {
    items <- setNames(
      replicate(
        12,
        list(value = NA_integer_, raw = NA_integer_),
        simplify = FALSE
      ),
      c(
        "h1",
        "h2",
        "h3",
        "h4",
        "h5",
        "h6",
        "h7",
        "h8",
        "h9",
        "h10",
        "h11",
        "h12"
      )
    )
    h8_type <- NA_character_
    h8_detail <- NA_character_
  }

  total_result <- honos_total_score(
    items$h1$value,
    items$h2$value,
    items$h3$value,
    items$h4$value,
    items$h5$value,
    items$h6$value,
    items$h7$value,
    items$h8$value,
    items$h9$value,
    items$h10$value,
    items$h11$value,
    items$h12$value
  )

  list(
    # identification
    fid = trimws(r[[3]]), # ANQ: FID
    facility = trimws(r[[2]]), # ANQ: 4.01.V02

    # time point
    time_point = time_point,
    time_point_label = time_point_label(time_point),

    # dropout
    is_dropout = is_dropout,
    dropout_code = dropout_code,
    dropout_detail = if (is_dropout) trimws(r[[6]]) else NA_character_,

    # assessment date
    date = parse_date_anq(r[[7]]), # ANQ: 4.02.V00

    # HoNOS items: cleaned value (9 -> NA)
    h1_aggression = items$h1$value, # ANQ: 4.02.V01
    h2_self_harm = items$h2$value, # ANQ: 4.02.V02
    h3_substance_use = items$h3$value, # ANQ: 4.02.V03
    h4_cognitive = items$h4$value, # ANQ: 4.02.V04
    h5_physical = items$h5$value, # ANQ: 4.02.V05
    h6_hallucination = items$h6$value, # ANQ: 4.02.V06
    h7_mood = items$h7$value, # ANQ: 4.02.V07
    h8_other_mental = items$h8$value, # ANQ: 4.02.V08
    h8_type = h8_type, # ANQ: 4.02.V09
    h8_detail = h8_detail, # ANQ: 4.02.V10
    h9_relationships = items$h9$value, # ANQ: 4.02.V11
    h10_daily_living = items$h10$value, # ANQ: 4.02.V12
    h11_living_conditions = items$h11$value, # ANQ: 4.02.V13
    h12_occupation = items$h12$value, # ANQ: 4.02.V14

    # raw values (9 remains 9)
    h1_raw = items$h1$raw,
    h2_raw = items$h2$raw,
    h3_raw = items$h3$raw,
    h4_raw = items$h4$raw,
    h5_raw = items$h5$raw,
    h6_raw = items$h6$raw,
    h7_raw = items$h7$raw,
    h8_raw = items$h8$raw,
    h9_raw = items$h9$raw,
    h10_raw = items$h10$raw,
    h11_raw = items$h11$raw,
    h12_raw = items$h12$raw,

    # total score and number of valid items
    honos_total = total_result$total,
    honos_n_valid = total_result$n_valid
  )
}

#' Parse a single HoNOS item
#' 0-4 = valid values, 9 = not known / not applicable -> stored as raw,
#' recoded to NA in the cleaned value
#' Returns list(value, raw)
#' @noRd
parse_honos_item <- function(x) {
  x <- trimws(x)
  if (is.na(x) || x == "") {
    return(list(value = NA_integer_, raw = NA_integer_))
  }
  val <- suppressWarnings(as.integer(x))
  list(
    value = if (!is.na(val) && val %in% 0:4) {
      val
    } else {
      NA_integer_
    },
    raw = val
  )
}

#' Calculate HoNOS total score
#'
#' The total is the sum of the available HoNOS item scores. Items coded 9,
#' blank, or otherwise missing are omitted from the sum; scores are not
#' prorated to 12 items. `n_valid` reports the number of items contributing
#' to the score. If no valid items are available, the total is `NA`.
#'
#' This reproduces the scoring approach used in the associated study.
#'
#' @return List with `total` and `n_valid`.
#' @noRd
honos_total_score <- function(...) {
  items <- c(...)
  n_valid <- sum(!is.na(items))
  list(
    total = if (n_valid == 0L) NA_integer_ else sum(items, na.rm = TRUE),
    n_valid = n_valid
  )
}


#' Empty PH data frame with correct column types
#' @noRd
empty_ph <- function() {
  data.frame(
    fid = character(),
    facility = character(),
    time_point = integer(),
    time_point_label = character(),
    is_dropout = logical(),
    dropout_code = integer(),
    dropout_detail = character(),
    date = as.Date(character()),
    h1_aggression = integer(),
    h2_self_harm = integer(),
    h3_substance_use = integer(),
    h4_cognitive = integer(),
    h5_physical = integer(),
    h6_hallucination = integer(),
    h7_mood = integer(),
    h8_other_mental = integer(),
    h8_type = character(),
    h8_detail = character(),
    h9_relationships = integer(),
    h10_daily_living = integer(),
    h11_living_conditions = integer(),
    h12_occupation = integer(),
    h1_raw = integer(),
    h2_raw = integer(),
    h3_raw = integer(),
    h4_raw = integer(),
    h5_raw = integer(),
    h6_raw = integer(),
    h7_raw = integer(),
    h8_raw = integer(),
    h9_raw = integer(),
    h10_raw = integer(),
    h11_raw = integer(),
    h12_raw = integer(),
    honos_total = integer(),
    honos_n_valid = integer(),
    stringsAsFactors = FALSE
  )
}
