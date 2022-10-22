#' @keywords internal
a <- function(phi) {
  return(phi)
}

#' @keywords internal
b <- function(theta) {
  return(exp(theta))
}

#' @keywords internal
c2 <- function(x, phi) {
  out <- -lgamma(x + 1)
  return(out)
}

#' @keywords internal
link <- function(mu) {
  return(log(mu))
}

#' @keywords internal
calc_phi <- function() {
  return(1)
}

#' The Poisson Distribution
#'
#' @inheritParams dnormalt
#' @return dpoisalt gives the density
#' @inherit dnormalt source
#' @examples
#' library(altForm)
#'
#' set.seed(1)
#' x <- rpois(10, 1)
#' dpoisalt(x, 1)
#'
#' @export
dpoisalt <- create_pdf_exponential_form(a, b, c2, link, calc_phi, FALSE, 0, Inf, 0, 143) # 143 for numerical accuracy.
