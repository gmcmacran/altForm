#' @keywords internal
a <- function(phi) {
  return(-1 * phi)
}

#' @keywords internal
b <- function(theta) {
  return((2 * theta)^.5)
}

#' @keywords internal
c2 <- function(x, phi) {
  out <- -1 / (2 * x * phi)
  out <- out - (1 / 2) * (log(2 * pi * x^3 * phi))
  return(out)
}

#' @keywords internal
link <- function(mu) {
  return(1 / (2 * mu^2))
}

#' @keywords internal
calc_phi <- function(sigma, mu) {
  return(sigma^2)
}

#' The Inverse Gaussian Distribution
#'
#' @inheritParams dnormalt
#' @return dnormalt gives the density
#' @inherit dnormalt source
#' @examples
#' library(altForm)
#'
#' set.seed(1)
#' x <- statmod::rinvgauss(n = 10, mean = 2, dispersion = 1)
#' dinvgaussalt(x, 1, 1)
#'
#' @export
dinvgaussalt <- create_pdf_exponential_form(a, b, c2, link, calc_phi, TRUE, .Machine$double.eps, Inf, 0, Inf)
