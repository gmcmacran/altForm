
<!-- README.md is generated from README.Rmd. Please edit that file -->

# altForm

<!-- badges: start -->
<!-- badges: end -->

altForm provides alternative formulations of standard distributions. All
functions are parameterized by mu. Sigma and size are included where
appropriate. For some distributions, this is the typical
parameterization (i.e. normal). For others, this parameterization is new
(i. e. gamma). This package is a work in progress. At the moment,
probability density functions and cumulative density functions are
implemented and tested.

- Bernoulli
  - pdf
  - cdf
- Binomial
  - pdf
  - cdf
- Geometric
  - pdf
  - cdf
- Negative Binomial
  - pdf
  - cdf
- Normal
  - pdf
  - cdf
- Poisson
  - pdf
  - cdf
- Gamma
  - pdf
  - cdf
- Inverse Gaussian
  - pdf
  - cdf

## Example

Probability density calculations are done here.

``` r
library(altForm)
set.seed(1)
x <- rgamma(n = 3, shape = 2, rate = 1)

dgammaalt(x = x, mu = 1, sigma = 2)
#> [1] 0.18207321 0.03074991 0.03232136
```

Cumulative density calculations rely on numerical integration of pdf
functions instead of closed form formulas.

``` r
pgammaalt(q = x, mu = 1, sigma = 2)
#> [1] 0.7155729 0.9200817 0.9166898
```

## Installation

You can install the development version of altForm from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
# devtools::install_github("gmcmacran/altForm")
```
