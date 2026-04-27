# parse_mp.R

#' Parse MP records into a data frame
#' @param records list of character vectors, MP rows from read_anq_raw()
#' @return data.frame with one row per case
#' @noRd
parse_mp <- function(records) {
  rows <- lapply(records, parse_mp_row)
  rows <- Filter(Negate(is.null), rows)

  if (length(rows) == 0) {
    warning("No valid MP rows found", call. = FALSE)
    return(empty_mp())
  }

  do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
}

#' Parse a single MP row
#' @noRd
parse_mp_row <- function(r) {
  if (length(r) < 35) {
    warning(
      "MP row has only ",
      length(r),
      " fields (35 expected), ",
      "skipping. First fields: ",
      paste(r[seq_len(min(3, length(r)))], collapse = "|"),
      call. = FALSE
    )
    return(NULL)
  }

  list(
    # identification
    fid = trimws(r[[35]]), # ANQ: FID

    # sociodemographics
    civil_status = parse_int(r[[2]]), # ANQ: 3.2.V01
    location_before_admission = parse_int(r[[3]]), # ANQ: 3.2.V02, more detailed than MB

    # employment before admission (multiple answers possible, 0/1)
    employment_part_time = parse_int(r[[4]]), # ANQ: 3.2.V03
    employment_full_time = parse_int(r[[5]]), # ANQ: 3.2.V04
    employment_unemployed = parse_int(r[[6]]), # ANQ: 3.2.V05
    employment_homemaker = parse_int(r[[7]]), # ANQ: 3.2.V06
    employment_in_education = parse_int(r[[8]]), # ANQ: 3.2.V07
    employment_rehabilitation = parse_int(r[[9]]), # ANQ: 3.2.V08
    employment_pension = parse_int(r[[10]]), # ANQ: 3.2.V09
    employment_sheltered = parse_int(r[[11]]), # ANQ: 3.2.V10
    employment_unknown = parse_int(r[[12]]), # ANQ: 3.2.V11

    # education
    highest_education = parse_int(r[[13]]), # ANQ: 3.2.V12

    # admission
    # more detailed than MB
    referring_institution = parse_int(r[[14]]), # ANQ: 3.3.V01
    # voluntary admission (r[[15]]) no longer relevant since involuntary
    # placement (FU) reform in 2013, but still read in
    voluntary_admission = parse_int(r[[15]]), # ANQ: 3.3.V02
    involuntary_placement = parse_int(r[[16]]), # ANQ: 3.3.V03

    # treatment
    # number of days (r[[17]]) no longer mandatory from 2019, but still read in
    number_of_days = parse_int(r[[17]]), # ANQ: 3.4.V01
    treatment = parse_int(r[[18]]), # ANQ: 3.4.V02

    # psychopharmacotherapy (multiple answers possible, 0/1)
    pharma_neuroleptics = parse_int(r[[19]]), # ANQ: 3.4.V03
    pharma_depot_neuroleptics = parse_int(r[[20]]), # ANQ: 3.4.V04
    pharma_antidepressants = parse_int(r[[21]]), # ANQ: 3.4.V05
    pharma_tranquilizers = parse_int(r[[22]]), # ANQ: 3.4.V06
    pharma_hypnotics = parse_int(r[[23]]), # ANQ: 3.4.V07
    pharma_antiepileptics = parse_int(r[[24]]), # ANQ: 3.4.V08
    pharma_lithium = parse_int(r[[25]]), # ANQ: 3.4.V09
    pharma_addiction_substitution = parse_int(r[[26]]), # ANQ: 3.4.V10
    pharma_addiction_aversion = parse_int(r[[27]]), # ANQ: 3.4.V11
    pharma_antiparkinson = parse_int(r[[28]]), # ANQ: 3.4.V12
    pharma_other = parse_int(r[[29]]), # ANQ: 3.4.V13
    pharma_somatic = parse_int(r[[30]]), # ANQ: 3.4.V14

    # discharge, more detailed than MB
    discharge_decision = parse_int(r[[31]]), # ANQ: 3.5.V01
    residence_after_discharge = parse_int(r[[32]]), # ANQ: 3.5.V02
    treatment_after_discharge = parse_int(r[[33]]), # ANQ: 3.5.V03

    # treatment area -- key classification variable
    # 1=general, 2=child & adolescent, 3=geriatric, 4=addiction, 5=forensic
    treatment_area = parse_int(r[[34]]) # ANQ: 3.5.V04
  )
}

#' Empty MP data frame with correct column types
#' @noRd
empty_mp <- function() {
  data.frame(
    fid = character(),
    civil_status = integer(),
    location_before_admission = integer(),
    employment_part_time = integer(),
    employment_full_time = integer(),
    employment_unemployed = integer(),
    employment_homemaker = integer(),
    employment_in_education = integer(),
    employment_rehabilitation = integer(),
    employment_pension = integer(),
    employment_sheltered = integer(),
    employment_unknown = integer(),
    highest_education = integer(),
    referring_institution = integer(),
    voluntary_admission = integer(),
    involuntary_placement = integer(),
    number_of_days = integer(),
    treatment = integer(),
    pharma_neuroleptics = integer(),
    pharma_depot_neuroleptics = integer(),
    pharma_antidepressants = integer(),
    pharma_tranquilizers = integer(),
    pharma_hypnotics = integer(),
    pharma_antiepileptics = integer(),
    pharma_lithium = integer(),
    pharma_addiction_substitution = integer(),
    pharma_addiction_aversion = integer(),
    pharma_antiparkinson = integer(),
    pharma_other = integer(),
    pharma_somatic = integer(),
    discharge_decision = integer(),
    residence_after_discharge = integer(),
    treatment_after_discharge = integer(),
    treatment_area = integer(),
    stringsAsFactors = FALSE
  )
}
