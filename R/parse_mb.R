# parse_mb.R

#' Parse MB records into a data frame
#' @param records list of character vectors, MB rows from read_anq_raw()
#' @return data.frame with one row per case
#' @noRd
parse_mb <- function(records) {
  rows <- lapply(records, parse_mb_row)
  rows <- Filter(Negate(is.null), rows)

  if (length(rows) == 0) {
    warning("No valid MB rows found", call. = FALSE)
    return(empty_mb())
  }

  do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
}

#' Parse a single MB row
#' @noRd
parse_mb_row <- function(r) {
  if (length(r) < 52) {
    warning(
      "MB row has only ",
      length(r),
      " fields (52 expected), ",
      "skipping. First fields: ",
      paste(r[seq_len(min(3, length(r)))], collapse = "|"),
      call. = FALSE
    )
    return(NULL)
  }

  list(
    # identification
    fid = trimws(r[[51]]), # ANQ: FID
    pid = trimws(r[[52]]), # ANQ: PID
    facility = trimws(r[[2]]), # ANQ: 0.1.V02
    location = trimws(r[[3]]), # ANQ: 0.1.V03
    kanton = trimws(r[[4]]), # ANQ: 0.1.V04

    # dataset flags
    flag_psychiatry = parse_int(r[[8]]), # ANQ: 0.3.V02, should be 1
    flag_cantonal = parse_int(r[[10]]), # ANQ: 0.3.V04

    # patient
    sex = parse_int(r[[11]]), # ANQ: 1.1.V01, 1=male 2=female
    age_admission = parse_int(r[[13]]), # ANQ: 1.1.V03
    residence_region = trimws(r[[14]]), # ANQ: 1.1.V04
    nationality = trimws(r[[15]]), # ANQ: 1.1.V05

    # admission
    admission = parse_datetime_anq(r[[16]]), # ANQ: 1.2.V01
    location_before_admission = parse_int(r[[17]]), # ANQ: 1.2.V02
    admission_type = parse_int(r[[18]]), # ANQ: 1.2.V03
    referring_institution = parse_int(r[[19]]), # ANQ: 1.2.V04

    # treatment
    treatment_type = parse_int(r[[20]]), # ANQ: 1.3.V01
    insurance_class = parse_int(r[[21]]), # ANQ: 1.3.V02
    intensive_care_hours = parse_int(r[[22]]), # ANQ: 1.3.V03
    leave_hours = parse_int(r[[23]]), # ANQ: 1.3.V04
    main_cost_centre = trimws(r[[24]]), # ANQ: 1.4.V01, should be M500
    main_insurer = parse_int(r[[25]]), # ANQ: 1.4.V02

    # discharge
    discharge = parse_datetime_anq(r[[26]]), # ANQ: 1.5.V01
    discharge_decision = parse_int(r[[27]]), # ANQ: 1.5.V02
    residence_after_discharge = parse_int(r[[28]]), # ANQ: 1.5.V03
    treatment_after_discharge = parse_int(r[[29]]), # ANQ: 1.5.V04

    # diagnoses
    main_diagnosis = trimws(r[[30]]), # ANQ: 1.6.V01
    main_diagnosis_supplement = trimws(r[[31]]), # ANQ: 1.6.V02
    secondary_diagnosis_1 = trimws(r[[32]]), # ANQ: 1.6.V03
    secondary_diagnosis_2 = trimws(r[[33]]), # ANQ: 1.6.V04
    secondary_diagnosis_3 = trimws(r[[34]]), # ANQ: 1.6.V05
    secondary_diagnosis_4 = trimws(r[[35]]), # ANQ: 1.6.V06
    secondary_diagnosis_5 = trimws(r[[36]]), # ANQ: 1.6.V07
    secondary_diagnosis_6 = trimws(r[[37]]), # ANQ: 1.6.V08
    secondary_diagnosis_7 = trimws(r[[38]]), # ANQ: 1.6.V09
    secondary_diagnosis_8 = trimws(r[[39]]), # ANQ: 1.6.V10

    # treatments
    main_treatment = trimws(r[[40]]), # ANQ: 1.7.V01
    main_treatment_start = parse_datetime_anq(r[[41]]), # ANQ: 1.7.V02
    additional_treatment_1 = trimws(r[[42]]), # ANQ: 1.7.V03
    additional_treatment_2 = trimws(r[[43]]), # ANQ: 1.7.V04
    additional_treatment_3 = trimws(r[[44]]), # ANQ: 1.7.V05
    additional_treatment_4 = trimws(r[[45]]), # ANQ: 1.7.V06
    additional_treatment_5 = trimws(r[[46]]), # ANQ: 1.7.V07
    additional_treatment_6 = trimws(r[[47]]), # ANQ: 1.7.V08
    additional_treatment_7 = trimws(r[[48]]), # ANQ: 1.7.V09
    additional_treatment_8 = trimws(r[[49]]), # ANQ: 1.7.V10
    additional_treatment_9 = trimws(r[[50]]) # ANQ: 1.7.V11
  )
}

#' Empty MB data frame with correct column types
#' Returned when no valid rows are found
#' @noRd
empty_mb <- function() {
  data.frame(
    fid = character(),
    pid = character(),
    facility = character(),
    location = character(),
    kanton = character(),
    flag_psychiatry = integer(),
    flag_cantonal = integer(),
    sex = integer(),
    age_admission = integer(),
    residence_region = character(),
    nationality = character(),
    admission = as.POSIXct(character()),
    location_before_admission = integer(),
    admission_type = integer(),
    referring_institution = integer(),
    treatment_type = integer(),
    insurance_class = integer(),
    intensive_care_hours = integer(),
    leave_hours = integer(),
    main_cost_centre = character(),
    main_insurer = integer(),
    discharge = as.POSIXct(character()),
    discharge_decision = integer(),
    residence_after_discharge = integer(),
    treatment_after_discharge = integer(),
    main_diagnosis = character(),
    main_diagnosis_supplement = character(),
    secondary_diagnosis_1 = character(),
    secondary_diagnosis_2 = character(),
    secondary_diagnosis_3 = character(),
    secondary_diagnosis_4 = character(),
    secondary_diagnosis_5 = character(),
    secondary_diagnosis_6 = character(),
    secondary_diagnosis_7 = character(),
    secondary_diagnosis_8 = character(),
    main_treatment = character(),
    main_treatment_start = as.POSIXct(character()),
    additional_treatment_1 = character(),
    additional_treatment_2 = character(),
    additional_treatment_3 = character(),
    additional_treatment_4 = character(),
    additional_treatment_5 = character(),
    additional_treatment_6 = character(),
    additional_treatment_7 = character(),
    additional_treatment_8 = character(),
    additional_treatment_9 = character(),
    stringsAsFactors = FALSE
  )
}
