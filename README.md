
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
#> 
#> ✔ Updated metadata database: 7.74 MB in 4 files.
#> 
#> ℹ Updating metadata database
#> ✔ Updating metadata database ... done
#> 
#> 
#> → Will install 19 packages.
#> → Will update 1 package.
#> → Will download 19 CRAN packages (12.60 MB).
#> → Will download 1 package with unknown size.
#> + cellranger              1.1.0       ⬇ (104.28 kB)
#> + cli                     3.6.6      🔧 ⬇ (1.47 MB)
#> + dplyr                   1.2.1      🔧 ⬇ (1.64 MB)
#> + generics                0.1.4       ⬇ (82.60 kB)
#> + glue                    1.8.1      🔧 ⬇ (179.73 kB)
#> + honupsy    0.0.0.9000 → 0.0.0.9000 👷🏻‍♂️🔧 ⬇ (GitHub: 453e48e)
#> + lifecycle               1.0.5       ⬇ (132.98 kB)
#> + magrittr                2.0.5      🔧 ⬇ (231.62 kB)
#> + pillar                  1.11.1      ⬇ (660.11 kB)
#> + pkgconfig               2.0.3       ⬇ (18.47 kB)
#> + purrr                   1.2.2      🔧 ⬇ (585.36 kB)
#> + R6                      2.6.1       ⬇ (87.28 kB)
#> + readxl                  1.5.0      🔧 ⬇ (1.53 MB)
#> + rematch                 2.0.0       ⬇ (16.67 kB)
#> + rlang                   1.2.0      🔧 ⬇ (1.92 MB)
#> + tibble                  3.3.1      🔧 ⬇ (659.83 kB)
#> + tidyselect              1.2.1       ⬇ (226.89 kB)
#> + utf8                    1.2.6      🔧 ⬇ (209.74 kB)
#> + vctrs                   0.7.3      🔧 ⬇ (2.62 MB)
#> + withr                   3.0.2       ⬇ (224.91 kB)
#> ℹ Getting 19 pkgs (12.60 MB) and 1 pkg with unknown size
#> ✔ Got generics 0.1.4 (aarch64-apple-darwin20) (82.60 kB)
#> ✔ Got glue 1.8.1 (aarch64-apple-darwin20) (179.73 kB)
#> ✔ Got cellranger 1.1.0 (aarch64-apple-darwin20) (104.28 kB)
#> ✔ Got R6 2.6.1 (aarch64-apple-darwin20) (87.28 kB)
#> ✔ Got tidyselect 1.2.1 (aarch64-apple-darwin20) (226.89 kB)
#> ✔ Got pkgconfig 2.0.3 (aarch64-apple-darwin20) (18.47 kB)
#> ✔ Got tibble 3.3.1 (aarch64-apple-darwin20) (659.83 kB)
#> ✔ Got rematch 2.0.0 (aarch64-apple-darwin20) (16.67 kB)
#> ✔ Got lifecycle 1.0.5 (aarch64-apple-darwin20) (132.98 kB)
#> ✔ Got magrittr 2.0.5 (aarch64-apple-darwin20) (231.62 kB)
#> ✔ Got utf8 1.2.6 (aarch64-apple-darwin20) (209.74 kB)
#> ✔ Got withr 3.0.2 (aarch64-apple-darwin20) (224.91 kB)
#> ✔ Got cli 3.6.6 (aarch64-apple-darwin20) (1.47 MB)
#> ✔ Got pillar 1.11.1 (aarch64-apple-darwin20) (660.11 kB)
#> ✔ Got purrr 1.2.2 (aarch64-apple-darwin20) (585.36 kB)
#> ✔ Got dplyr 1.2.1 (aarch64-apple-darwin20) (1.64 MB)
#> ✔ Got rlang 1.2.0 (aarch64-apple-darwin20) (1.92 MB)
#> ✔ Got vctrs 0.7.3 (aarch64-apple-darwin20) (2.62 MB)
#> ✔ Got readxl 1.5.0 (aarch64-apple-darwin20) (1.53 MB)
#> ✔ Got honupsy 0.0.0.9000 (source) (1.92 MB)
#> ✔ Installed R6 2.6.1  (208ms)
#> ✔ Installed cellranger 1.1.0  (201ms)
#> ✔ Installed cli 3.6.6  (229ms)
#> ✔ Installed dplyr 1.2.1  (231ms)
#> ✔ Installed generics 0.1.4  (245ms)
#> ✔ Installed glue 1.8.1  (266ms)
#> ✔ Installed lifecycle 1.0.5  (296ms)
#> ✔ Installed magrittr 2.0.5  (340ms)
#> ✔ Installed pillar 1.11.1  (139ms)
#> ✔ Installed pkgconfig 2.0.3  (82ms)
#> ✔ Installed purrr 1.2.2  (135ms)
#> ✔ Installed readxl 1.5.0  (129ms)
#> ✔ Installed rematch 2.0.0  (84ms)
#> ✔ Installed rlang 1.2.0  (108ms)
#> ✔ Installed tidyselect 1.2.1  (83ms)
#> ✔ Installed tibble 3.3.1  (214ms)
#> ✔ Installed utf8 1.2.6  (107ms)
#> ✔ Installed vctrs 0.7.3  (133ms)
#> ✔ Installed withr 3.0.2  (149ms)
#> ℹ Packaging honupsy 0.0.0.9000
#> ✔ Packaged honupsy 0.0.0.9000 (2.3s)
#> ℹ Building honupsy 0.0.0.9000
#> ✔ Built honupsy 0.0.0.9000 (5s)
#> ✔ Installed honupsy 0.0.0.9000 (github::michaket/honupsy@453e48e) (152ms)
#> ✔ 1 pkg + 19 deps: upd 1, added 19, dld 20 (NA B) [27s]
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
#> 1 A                   0.256               0.205               0.154
#> 2 B                   0.188               0.25                0.25 
#> 3 C                   0.217               0.391               0.239
#> 4 D                   0.2                 0.6                 0.233
#> 5 E                   0.351               0.189               0.351
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
