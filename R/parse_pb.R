#' Parse PB records into a data frame
#' @param records list of character vectors, PB rows from read_anq_raw()
#' @return data.frame with two rows per case (admission + discharge)
#' @importFrom stats setNames
#' @noRd
parse_pb <- function(records) {
  rows <- lapply(records, parse_pb_row)
  rows <- Filter(Negate(is.null), rows)

  if (length(rows) == 0) {
    warning("No valid PB rows found", call. = FALSE)
    return(empty_pb())
  }

  do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
}

#' Parse a single PB row
#' @noRd
parse_pb_row <- function(r) {
  # silently skip completely empty rows (common in Excel exports)
  if (all(trimws(r) == "")) {
    return(NULL)
  }

  if (length(r) < 7) {
    warning(
      "PB row has only ",
      length(r),
      " fields (at least 7 expected), ",
      "skipping. First fields: ",
      paste(r[seq_len(min(3, length(r)))], collapse = "|"),
      call. = FALSE
    )
    return(NULL)
  }

  time_point <- parse_int(r[[4]]) # ANQ: 5.01.V04, 1=admission, 2=discharge
  dropout_code <- parse_int(r[[5]]) # ANQ: 5.01.V05, 0-8
  is_dropout <- !is.na(dropout_code) && dropout_code != 0L

  # parse items only when no dropout and enough fields present
  if (!is_dropout && length(r) >= 60) {
    items <- vapply(r[8:60], parse_bscl_item, integer(1))
    names(items) <- paste0("b", 1:53)
  } else {
    items <- rep(NA_integer_, 53)
    names(items) <- paste0("b", 1:53)
  }

  # total score and number of valid items
  n_valid <- sum(!is.na(items))
  bscl_total <- if (n_valid == 0L) NA_integer_ else sum(items, na.rm = TRUE)

  c(
    list(
      # identification
      fid = trimws(r[[3]]), # ANQ: FID
      facility = trimws(r[[2]]), # ANQ: 5.01.V02

      # time point
      time_point = time_point,
      time_point_label = time_point_label(time_point),

      # dropout
      is_dropout = is_dropout,
      dropout_code = dropout_code,
      dropout_detail = if (is_dropout) trimws(r[[6]]) else NA_character_,

      # assessment date
      date = parse_date_anq(r[[7]]), # ANQ: 5.04.V00

      # total score and number of valid items
      bscl_total = bscl_total,
      bscl_n_valid = n_valid
    ),
    as.list(items)
  )
}

#' Parse a single BSCL item
#' Valid values: 0-4. Empty string or NA -> NA.
#' Unlike HoNOS, there is no "not applicable" code (no 9).
#' @noRd
parse_bscl_item <- function(x) {
  x <- trimws(x)
  if (is.na(x) || x == "") {
    return(NA_integer_)
  }
  suppressWarnings(as.integer(x))
}

#' Dropout code labels for BSCL
#' ANQ: 5.01.V05
#' @noRd
bscl_dropout_label <- function(code) {
  switch(
    as.character(code),
    "0" = "no dropout",
    "1" = "patient refusal",
    "2" = "language barrier",
    "3" = "too ill",
    "4" = "death",
    "5" = "too young",
    "6" = "discharge within 24h of admission assessment",
    "7" = "unexpected discharge or non-return from leave",
    "8" = "other",
    NA_character_
  )
}

#' Empty PB data frame with correct column types
#' @noRd
empty_pb <- function() {
  item_cols <- setNames(
    rep(list(integer()), 53),
    paste0("b", 1:53)
  )

  do.call(
    data.frame,
    c(
      list(
        fid = character(),
        facility = character(),
        time_point = integer(),
        time_point_label = character(),
        is_dropout = logical(),
        dropout_code = integer(),
        dropout_detail = character(),
        date = as.Date(character()),
        bscl_total = integer(),
        bscl_n_valid = integer()
      ),
      item_cols,
      list(stringsAsFactors = FALSE)
    )
  )
}
