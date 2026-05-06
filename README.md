
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The {honupsy} Package: Tools for Working with Routine Psychiatric Patient Data <img src="man/figures/hex.png" width = "150" align="right" alt="Hex sticker of the {honupsy} R package."/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/michaket/honupsy/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/michaket/honupsy/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/michaket/honupsy/graph/badge.svg?token=IUR4493DS7)](https://codecov.io/gh/michaket/honupsy)
<!-- badges: end -->

The goal of honupsy is to provide tools for working with routinely
collected inpatient psychiatric data, with a particular focus on the
Health of the Nation Outcome Scales (HoNOS) and unit-level indicators
relevant to nursing workload and patient composition.

The package is built around the data infrastructure of the Swiss
national quality measurement programme in inpatient psychiatry,
coordinated by [ANQ](https://www.anq.ch). Participating clinics are
required by law to collect and submit these data to ANQ. honupsy allows
clinics and researchers to work with these data directly in R, without
any additional data preparation steps.

The package currently supports:

- importing ANQ data in all standard submission formats (TXT with pipe,
  semicolon or tab delimiter; XLSX; official ANQ data entry template)
- parsing all relevant record types: MB (minimal dataset), MP
  (psychiatry supplementary data), PH (HoNOS), PB (BSCL), and FM
  (coercive measures)
- validating imported data for completeness and plausibility
- computing HoNOS total scores and handling missing or not-applicable
  items
- generating realistic synthetic psychiatric case data
- summarising unit-level HoNOS metrics

Future versions will add:

- descriptive summaries at case and unit level
- time-series statistics (e.g. admission-to-discharge change scores)
- estimation procedures for workload and staffing-related analyses.

## Installation

You can install the development version of honupsy from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("michaket/honupsy")
#> ℹ Loading metadata database✔ Loading metadata database ... done
#>  
#> ℹ No downloads are needed
#> ✔ 1 pkg + 18 deps: kept 13 [5.6s]
```

## Examples

### Importing ANQ data

``` r
library(honupsy)

# path to the example file shipped with the package
path <- system.file("extdata", "example.txt", package = "honupsy")

# import -- uses the same file format submitted to ANQ
data <- import_anq(path)
#> ANQ import completed:
#>   MB: 3 cases
#>   MP: 3 cases
#>   PH: 6 assessments (3 admission, 1 discharge, 1 dropout)
#>   PB: 6 assessments (2 admission, 1 discharge, 3 dropout)
#>   FM: 4 measures for 2 cases

# the result is a named list of data frames, one per record type
names(data)
#> [1] "mb"          "mp"          "ph"          "pb"          "fm"         
#> [6] ".validation"

# MB: one row per case
head(data$mb[, c("fid", "sex", "age_admission", "main_diagnosis")])
#>       fid sex age_admission main_diagnosis
#> 1 5050286   2            61           F200
#> 2 5050297   1            67           F312
#> 3 5050292   1            46           F102

# PH: two rows per case (admission and discharge)
head(data$ph[, c("fid", "time_point_label", "honos_total", "is_dropout")])
#>       fid time_point_label honos_total is_dropout
#> 1 5050286        admission          31      FALSE
#> 2 5050286        discharge          24      FALSE
#> 3 5050297        admission          32      FALSE
#> 4 5050297        discharge          NA       TRUE
#> 5 5050292        admission          33      FALSE
#> 6 5050292            other          35      FALSE

# FM: one row per coercive measure
head(data$fm[, c("fid", "measure_label", "duration_min")])
#>       fid         measure_label duration_min
#> 1 5050286 Psychiatric isolation          210
#> 2 5050286    Physical restraint         1320
#> 3 5050292    Safety measure bed        17340
#> 4 5050292  Safety measure chair         1320
```

### Working with synthetic data

``` r
# generate synthetic case data for testing and demonstration
cases <- sample_cases(n = 200)

# summarise HoNOS metrics at unit level
summarise_honos_units(cases)
#> # A tibble: 5 × 16
#>   unit  prop_honos_1_severe prop_honos_2_severe prop_honos_3_severe
#>   <chr>               <dbl>               <dbl>               <dbl>
#> 1 A                   0.205               0.227               0.136
#> 2 B                   0.227               0.341               0.318
#> 3 C                   0.324               0.243               0.270
#> 4 D                   0.229               0.257               0.143
#> 5 E                   0.2                 0.275               0.3  
#> # ℹ 12 more variables: prop_honos_4_severe <dbl>, prop_honos_5_severe <dbl>,
#> #   prop_honos_6_severe <dbl>, prop_honos_7_severe <dbl>,
#> #   prop_honos_8_severe <dbl>, prop_honos_9_severe <dbl>,
#> #   prop_honos_10_severe <dbl>, prop_honos_11_severe <dbl>,
#> #   prop_honos_12_severe <dbl>, age_mean <dbl>, los_mean <dbl>,
#> #   honos_total_mean <dbl>
```

## Data Format

honupsy works with the standard ANQ data submission format. No
reformatting is required: the same files your clinic prepares for
submission to ANQ can be loaded directly into R with `import_anq()`.
Supported formats are:

- Pipe-, semicolon- or tab-delimited TXT files
- XLSX files with one worksheet per record type
- The official ANQ data entry template (Excel)

Both single mixed files (all record types in one file) and separate
files per record type are supported.

## About the Hex Sticker

The honupsy hex sticker features a Valais Blacknose Sheep (Walliser
Schwarznasenschaf). These sheep are known for their exceptionally gentle
and approachable nature and are used in animal-assisted therapy and
support for children and adults with emotional or physical impairments.
