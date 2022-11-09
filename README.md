
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Overview

<!-- badges: start -->
<!-- badges: end -->

altForm provides alternative formulations of standard distributions by
using the exponential form from generalized linear models. All functions
are parameterized by
![\mu](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmu "\mu").
![\sigma](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Csigma "\sigma")
and size are included where appropriate. For some distributions, this is
the typical parameterization (i.e. normal). For others, this
parameterization is uncommon (i. e. gamma). All functions are tested
against the typical R functions.

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
- Poisson
  - pmf
  - cmf
  - random number generator
- Normal
  - pdf
  - cdf
  - random number generator
- Gamma
  - pdf
  - cdf (Numerical integration is inaccurate for some parameters)
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
x <- rinvgaussalt(n = 3, mu = 1, sigma = 2)
```

All probability density functions rely on the exponential formulation.

``` r
dinvgaussalt(x = x, mu = 1, sigma = 2)
#> [1] 0.02780787 0.11340072 1.37646961
```

All cumulative density calculations rely on numerical integration of pdf
functions instead of closed form formulas.

``` r
pinvgaussalt(q = x, mu = 1, sigma = 2)
#> [1] 0.9306832 0.8278161 0.3603827
```

## Design Choices

This package is an exploration of functional programming. All pdf
functions are results of a single function factory. All cdf functions
are results of an additional function factory that takes a pdf function
as an arguments. The implementations for pmf functions and cmf functions
are similar.

- Looking back, what are the advantages of this approach?
  - Extreme code reuse.
  - Easy to understand dependencies.
  - Speed of implementation. All functions are one call to a function
    factory.
  - Error checking at definition time.
- What are the disadvantages?
  - Cutting edge compute speed cannot be achieved due to generality.
  - Extremely high numerical accuracy cannot be achieved due to the
    numerical integration dependency.

## Installation

There are no plans to submit to CRAN. You can install a stable version
of altForm from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
# devtools::install_github("gmcmacran/altForm")
```
