% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inverse_gaussian.R
\name{dinvgaussalt}
\alias{dinvgaussalt}
\alias{pinvgaussalt}
\alias{rinvgaussalt}
\title{The Inverse Gaussian Distribution}
\source{
Hardin, James William., and Joseph Hilbe. Generalized Linear Models and Extensions. Stata Press, 2012.
}
\usage{
dinvgaussalt(x, mu, sigma, log = FALSE)

pinvgaussalt(q, mu, sigma, lower.tail = TRUE, log.p = FALSE)

rinvgaussalt(n, mu, sigma)
}
\arguments{
\item{x}{a numeric vector.}

\item{mu}{a number indicating the mean.}

\item{sigma}{a number indicating the standard deviation.}

\item{log}{logical; if TRUE, probabilities p are given as log(p).}

\item{q}{a numeric vector.}

\item{lower.tail}{logical; if TRUE (default),}

\item{log.p}{logical; if TRUE, probabilities p are given as log(p).}

\item{n}{number of observations. Must be length 1}
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
x <- rinvgaussalt(10, 2, 3)

dinvgaussalt(x, 2, 3)

pinvgaussalt(x, 2, 3)

}
