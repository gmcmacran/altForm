
<!-- README.md is generated from README.Rmd. Please edit that file -->

# altForm

<!-- badges: start -->
<!-- badges: end -->

altForm provides alternative formulations of standard distributions. All
functions are parameterized by
![\mu](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmu "\mu").
![\sigma](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Csigma "\sigma")
and size are included where appropriate. For some distributions, this is
the typical parameterization (i.e. normal). For others, this
parameterization is uncommon (i. e. gamma). All pdfs, pmfs, cdfs and
cmfs are tested against the typical R functions.

- Bernoulli
  - pmf
  - cmf
  - random number generator
- Binomial
  - pmf
  - cmf
  - random number generator
- Geometric
  - pmf
  - cmf
  - random number generator
- Negative Binomial
  - pmf
  - cmf
  - random number generator
- Normal
  - pdf
  - cdf
  - random number generator
- Poisson
  - pmf
  - cmf
  - random number generator
- Gamma
  - pdf
  - cdf
  - random number generator
- Inverse Gaussian
  - pdf
  - cdf
  - random number generator

## Example

All random generators convert parameters and call R’s typical random
number generators.

``` r
library(altForm)
set.seed(1)
x <- rgammaalt(n = 3, mu = 1, sigma = 2)
```

All probability density functions rely on the exponential formulations.

``` r
dgammaalt(x = x, mu = 1, sigma = 2)
#> [1] 2.80945630 0.01280777 0.17022078
```

All cumulative density calculations rely on numerical integration of pdf
functions instead of closed form formulas.

``` r
pgammaalt(q = x, mu = 1, sigma = 2)
#> [1] 0.3194150 0.9627359 0.7261375
```

## Installation

There are no plans to submit to CRAN. You can install a stable version
of altForm from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
# devtools::install_github("gmcmacran/altForm")
```
