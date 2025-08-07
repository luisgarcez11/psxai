
<!-- README.md is generated from README.Rmd. Please edit that file -->

# psxai

Explaining AI models through Propensity Score Methodology
<!-- badges: start --> <!-- badges: end -->

The goal of psxai is to explain AI models through IPTW.

## Installation

You can install the development version of psxai from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("luisgarcez11/psxai")
```

## 1st step

Preprocess the data.

``` r
library(psxai)
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
obj1 <- preprocess_data(data = psxai::heart_preprocess,
                indeps = NULL,
                y = "Heart Disease",
                pred = "predictions")
```

## 2nd step

Estimating PS, applying weights and estimating variable effects.

``` r
obj2 <- obj1 %>% 
  ps_calc()
```

## 3rd step

Plot it.

``` r
obj3 <- obj2 %>% 
  plot_forest()
```
