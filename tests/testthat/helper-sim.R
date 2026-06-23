# Shared helper for summarise_* tests: synthetic data with units attached.
sim_units <- function(n = 200, units = c("A", "B", "C"), seed = 1, ...) {
  set.seed(seed)
  x <- sample_cases(n = n, ...)
  lookup <- data.frame(
    fid = x$mb$fid,
    unit = sample(units, nrow(x$mb), replace = TRUE),
    stringsAsFactors = FALSE
  )
  # all fids are assigned, so no unmatched-FID warnings expected
  suppressWarnings(assign_units(x, import_unit_assignments(lookup)))
}
