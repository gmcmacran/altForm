% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inverse_gaussian.R
\name{dinvgaussalt}
\alias{dinvgaussalt}
\title{The Inverse Gaussian Distribution}
\source{
Hardin, James William., and Joseph Hilbe. Generalized Linear Models and Extensions. Stata Press, 2012.
}
\usage{
dinvgaussalt(x, mu, sigma, log = FALSE)
}
\arguments{
\item{x}{a numeric vector.}

\item{mu}{a number indicating the mean.}

\item{sigma}{a number indicating the standard deviation.}

\item{log}{logical; if TRUE, probabilities p are given as log(p).}
}
\value{
dnormalt gives the density
}
\description{
The Inverse Gaussian Distribution
}
\examples{
library(altForm)

set.seed(1)
x <- statmod::rinvgauss(n = 100, mean = 2, dispersion = 1)
dinvgaussalt(x, 1, 1)

}