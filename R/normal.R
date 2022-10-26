#' @keywords internal
a <- function(phi) {
  return(phi)
}

#' @keywords internal
b <- function(theta) {
  return(theta^2 / 2)
}

#' @keywords internal
c2 <- function(x, phi) {
  out <- -(1 / 2) * (x^2 / phi + log(2 * pi * phi))
  return(out)
}

#' @keywords internal
link <- function(mu) {
  return(mu)
}

#' @keywords internal
calc_phi <- function(sigma, mu) {
  return(sigma^2)
}

#' The Normal Distribution.
#'
#' @param x a numeric vector.
#' @param mu a number indicating the mean.
#' @param sigma a number indicating the standard deviation.
#' @param log logical; if TRUE, probabilities p are given as log(p).
#' @param lower.tail logical; if TRUE (default),
#' @return dnormalt gives the density, pnormalt gives the cumulative distribution function.
#' @source Hardin, James William., and Joseph Hilbe. Generalized Linear Models and Extensions. Stata Press, 2012.
#' @examples
#' library(altForm)
#'
#' set.seed(1)
#' x <- rnorm(10, 0, 1)
#' dnormalt(x, 0, 1)
#'
#' pnormalt(x, 0, 1)
#'
#' @export
dnormalt <- altForm:::create_pdf_exponential_form(a, b, c2, link, calc_phi, TRUE, -Inf, Inf, -Inf, Inf)

#' @rdname dnormalt
#' @param q a numeric vector.
#' @param log.p logical; if TRUE, probabilities p are given as log(p).
#' @export
pnormalt <- altForm:::create_cdf_exponential_form(dnormalt, -Inf, Inf, -Inf, Inf)
