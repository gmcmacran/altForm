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

#' The Normal Distribution
#'
#' @param x a numeric vector.
#' @param mu a number indicating the mean.
#' @param sigma a number indicating the standard deviation.
#' @param log logical; if TRUE, probabilities p are given as log(p).
#' @return dnormalt gives the density
#' @examples
#' library(altForm)
#'
#' set.seed(1)
#' x <- rnorm(100, 0, 1)
#' dnormalt(x, 0, 1)
#'
#' @export
dnormalt <- create_pdf_exponential_form(a, b, c2, link, calc_phi, TRUE, -Inf)
