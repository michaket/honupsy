
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The {honupsy} Package: Tools for Working with Routine Psychiatric Patient Data <img src="man/figures/hex.png" width = "150" align="right" alt="Hex sticker of the {comorbidity} R package."/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/michaket/honupsy/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/michaket/honupsy/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/michaket/honupsy/graph/badge.svg)](https://app.codecov.io/gh/michaket/honupsy)
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
#> 1 A                   0.245               0.143               0.306
#> 2 B                   0.306               0.224               0.143
#> 3 C                   0.233               0.302               0.209
#> 4 D                   0.346               0.308               0.115
#> 5 E                   0.333               0.273               0.121
#> # ℹ 11 more variables: prop_honos_4_severe <dbl>, prop_honos_5_severe <dbl>,
#> #   prop_honos_6_severe <dbl>, prop_honos_7_severe <dbl>,
#> #   prop_honos_8_severe <dbl>, prop_honos_9_severe <dbl>,
#> #   prop_honos_10_severe <dbl>, prop_honos_11_severe <dbl>,
#> #   prop_honos_12_severe <dbl>, age_mean <dbl>, los_mean <dbl>
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
