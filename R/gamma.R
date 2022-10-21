#' @keywords internal
a <- function(phi) {
  return(-1 * phi)
}

#' @keywords internal
b <- function(theta) {
  return(-log(1 / theta))
}

#' @keywords internal
c2 <- function(x, phi) {
  out <- (1 - phi) / phi * log(x)
  out <- out - log(phi) / phi
  out <- out - lgamma(1 / phi)
  return(out)
}

#' @keywords internal
link <- function(mu) {
  return(1 / mu)
}

#' @keywords internal
calc_phi <- function(sigma, mu) {
  return(sigma^2 / mu^2)
}

#' The Inverse Gaussian Distribution
#'
#' @inheritParams dnormalt
#' @return dgammaalt gives the density
#' @inherit dnormalt source
#' @examples
#' library(altForm)
#'
#' set.seed(1)
#' x <- rgamma(n = 100, shape = 2, rate = 1)
#' dgammaalt(x, 1, 1)
#'
#' @export
dgammaalt <- create_pdf_exponential_form(a, b, c2, link, calc_phi, TRUE, .Machine$double.eps, Inf, 0, Inf)
