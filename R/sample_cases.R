#' Generate Synthetic ANQ Cases in Import-Pipeline Format
#'
#' Simulates inpatient psychiatric cases and returns them in exactly the format
#' produced by \code{\link{import_anq}}: a named list with \code{mb}, \code{mp},
#' \code{ph}, \code{pb} and \code{fm} data frames. The data are useful for
#' examples, teaching, and testing the \code{summarise_*()} functions without
#' real patient data.
#'
#' The generator works by writing valid ANQ raw records (MB and PH) to a
#' temporary pipe-delimited file and reading them back with
#' \code{\link{import_anq}}. The returned structure therefore comes from the
#' real parser and cannot drift from it. In this version only MB and PH are
#' generated; \code{mp}, \code{pb} and \code{fm} are returned as empty data
#' frames, as they would be for an import that lacked those record types.
#'
#' Unit (ward) assignments are deliberately \emph{not} included, mirroring real
#' ANQ data: attach them afterwards with \code{\link{import_unit_assignments}}
#' and \code{\link{assign_units}} before calling the \code{summarise_*()}
#' functions.
#'
#' The probability arguments default to clean data (no dropouts, no missing
#' items, no missing discharge dates). Raise them to exercise the data-quality
#' columns reported by the summary functions: \code{p_dropout_*} drives
#' \code{prop_assessed}, \code{p_item_na} drives \code{mean_items_rated} and
#' \code{n_partial}, and \code{p_missing_discharge} drives the occupancy
#' exclusion count and \code{n_los_missing}.
#'
#' Randomness honours \code{set.seed()}; the function does not reseed.
#'
#' @param n Integer. Number of cases to generate. Default \code{1000}.
#' @param start_date,end_date Admission window as \code{Date}. Admission dates
#'   are sampled uniformly in \code{[start_date, end_date]}. Defaults span 2023.
#' @param p_dropout_adm,p_dropout_dis Numeric in \code{[0, 1]}. Probability that
#'   the admission / discharge HoNOS assessment is a dropout (no items rated).
#'   Defaults \code{0}.
#' @param p_item_na Numeric in \code{[0, 1]}. Per-item probability that a rated
#'   HoNOS item is coded 9 ("not known / not applicable"). Default \code{0}.
#' @param p_missing_discharge Numeric in \code{[0, 1]}. Probability that a case
#'   has no discharge date (its discharge HoNOS row is then a dropout).
#'   Default \code{0}.
#' @param include_discharge Logical. Generate discharge (time point 2) HoNOS
#'   rows in addition to admission rows. Default \code{TRUE}.
#'
#' @return A named list with elements \code{mb}, \code{mp}, \code{ph},
#'   \code{pb} and \code{fm}, identical in structure to \code{\link{import_anq}}.
#'
#' @seealso \code{\link{import_anq}}, \code{\link{assign_units}}
#'
#' @export
#'
#' @examples
#' # clean data, default settings
#' data <- sample_cases(n = 100)
#' names(data)
#' nrow(data$mb)
#'
#' # attach units, then summarise
#' set.seed(1)
#' lookup <- data.frame(
#'   fid  = data$mb$fid,
#'   unit = sample(c("A", "B", "C"), nrow(data$mb), replace = TRUE)
#' )
#' data <- assign_units(data, import_unit_assignments(lookup))
#' summarise_composition(data)
#'
#' # messy data to exercise the quality columns
#' messy <- sample_cases(
#'   n = 200,
#'   p_dropout_adm = 0.1,
#'   p_item_na = 0.05,
#'   p_missing_discharge = 0.08
#' )
sample_cases <- function(
  n = 1000,
  start_date = as.Date("2023-01-01"),
  end_date = as.Date("2023-12-31"),
  p_dropout_adm = 0,
  p_dropout_dis = 0,
  p_item_na = 0,
  p_missing_discharge = 0,
  include_discharge = TRUE
) {
  stopifnot(
    n >= 1,
    start_date <= end_date,
    p_dropout_adm >= 0,
    p_dropout_adm <= 1,
    p_dropout_dis >= 0,
    p_dropout_dis <= 1,
    p_item_na >= 0,
    p_item_na <= 1,
    p_missing_discharge >= 0,
    p_missing_discharge <= 1
  )
  n <- as.integer(n)

  # -- case-level facts --------------------------------------------------------
  fid <- sprintf("%08d", 10000000L + sample.int(89999999L, n))
  pid <- paste0("P", fid)
  sex <- sample(c(1L, 2L), n, replace = TRUE)
  age <- pmax(round(stats::rnorm(n, mean = 45, sd = 15)), 18L)
  diagnosis <- sample(
    c("F32.1", "F20.0", "F10.2", "F31.1", "F43.1", "F60.3"),
    n,
    replace = TRUE
  )

  adm_date <- sample(seq(start_date, end_date, by = "day"), n, replace = TRUE)
  # length of stay in days: gamma is always positive and right-skewed, so no
  # negatives to clamp (mean ~24, plus 1 to guarantee at least a one-day stay)
  los <- 1L + round(stats::rgamma(n, shape = 2, scale = 12))
  dis_date <- adm_date + los
  adm_hour <- sample(0:23, n, replace = TRUE)
  dis_hour <- sample(0:23, n, replace = TRUE)

  missing_dis <- stats::runif(n) < p_missing_discharge

  adm_dt <- paste0(format(adm_date, "%Y%m%d"), sprintf("%02d", adm_hour))
  dis_dt <- paste0(format(dis_date, "%Y%m%d"), sprintf("%02d", dis_hour))
  dis_dt[missing_dis] <- "" # parses to NA discharge

  # -- MB rows (52 fields, field 1 = record type) -----------------------------
  mb <- matrix("", nrow = n, ncol = 52)
  mb[, 1] <- "MB"
  mb[, 2] <- "PSY01" # facility
  mb[, 8] <- "1" # flag_psychiatry (validator expects 1)
  mb[, 11] <- as.character(sex)
  mb[, 13] <- as.character(age)
  mb[, 16] <- adm_dt
  mb[, 24] <- "M500" # main_cost_centre (validator expects M500)
  mb[, 26] <- dis_dt
  mb[, 30] <- diagnosis
  mb[, 51] <- fid
  mb[, 52] <- pid # last field non-empty: keeps all 52 on re-read
  mb_lines <- do.call(paste, c(asplit(mb, 2), sep = "|"))

  # -- PH item helper ----------------------------------------------------------
  # 12-column character matrix of item scores; "" for dropout rows
  ph_item_pos <- c(8:15, 18:21) # h1..h8 then h9..h12
  make_items <- function(dropmask) {
    m <- matrix(as.character(sample(0:4, n * 12L, replace = TRUE)), n, 12L)
    if (p_item_na > 0) {
      m[stats::runif(n * 12L) < p_item_na] <- "9"
    }
    m[dropmask, ] <- ""
    m
  }

  build_ph <- function(time_point, dropmask, date8) {
    g <- matrix("", nrow = n, ncol = 21)
    g[, 1] <- "PH"
    g[, 2] <- "PSY01"
    g[, 3] <- fid
    g[, 4] <- as.character(time_point)
    g[, 5] <- ifelse(dropmask, sample(c("1", "2"), n, replace = TRUE), "0")
    g[, 7] <- date8
    g[, ph_item_pos] <- make_items(dropmask)
    do.call(paste, c(asplit(g, 2), sep = "|"))
  }

  # admission rows: every case gets one (dropout or not)
  drop_adm <- stats::runif(n) < p_dropout_adm
  ph_adm <- build_ph(1L, drop_adm, format(adm_date, "%Y%m%d"))

  ph_lines <- ph_adm

  # discharge rows: dropout if discharge date is missing
  if (include_discharge) {
    drop_dis <- missing_dis | (stats::runif(n) < p_dropout_dis)
    dis_date8 <- ifelse(
      missing_dis,
      format(adm_date, "%Y%m%d"),
      format(dis_date, "%Y%m%d")
    )
    ph_dis <- build_ph(2L, drop_dis, dis_date8)
    ph_lines <- c(ph_lines, ph_dis)
  }

  # -- write, read back through the real parser --------------------------------
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c(mb_lines, ph_lines), tmp, useBytes = TRUE)

  # data is valid by construction; skip validation (and its empty-MP warnings)
  suppressMessages(import_anq(tmp, validate = FALSE))
}
