
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The {honupsy} Package: Tools for Working with Routine Psychiatric Patient Data <img src="man/figures/hex.png" width = "150" align="right" alt="Hex sticker of the {comorbidity} R package."/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/michaket/honupsy/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/michaket/honupsy/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/michaket/honupsy/graph/badge.svg?token=IUR4493DS7)](https://codecov.io/gh/michaket/honupsy)
<!-- badges: end -->

The goal of honupsy is to provide tools for working with routinely
collected inpatient psychiatric data, with a particular focus on the
Health of the Nation Outcome Scales (HoNOS) and unit-level indicators
relevant to nursing workload and patient composition.

The package supports:

- generating realistic synthetic psychiatric case data,
- preparing case-level data,
- summarizing unit-level HoNOS metrics,
- supporting exploratory work for and workload- and nurse
  staffing-related evaluation.

## Installation

You can install the development version of honupsy from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("michaket/honupsy")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(honupsy)

# generate demo data
cases <- sample_cases(n = 200)

# summarise
summarise_honos_units(cases)
#> # A tibble: 5 × 15
#>   unit  prop_honos_1_severe prop_honos_2_severe prop_honos_3_severe
#>   <chr>               <dbl>               <dbl>               <dbl>
#> 1 A                   0.316              0.316                0.263
#> 2 B                   0.289              0.356                0.267
#> 3 C                   0.341              0.0976               0.220
#> 4 D                   0.244              0.293                0.293
#> 5 E                   0.229              0.314                0.257
#> # ℹ 11 more variables: prop_honos_4_severe <dbl>, prop_honos_5_severe <dbl>,
#> #   prop_honos_6_severe <dbl>, prop_honos_7_severe <dbl>,
#> #   prop_honos_8_severe <dbl>, prop_honos_9_severe <dbl>,
#> #   prop_honos_10_severe <dbl>, prop_honos_11_severe <dbl>,
#> #   prop_honos_12_severe <dbl>, age_mean <dbl>, los_mean <dbl>
```

## About the Hex Sticker

The honupsy hex sticker features a Valais Blacknose Sheep (Walliser
Schwarznasenschaf). These sheep are known for their exceptionally gentle
and approachable nature and are used in animal-assisted therapy and
support for children and adults with emotional or physical impairments..
