#' @keywords internal
a <- function(phi) {
  return(phi)
}

#' @keywords internal
b <- function(theta) {
  return(log(1 + exp(theta)))
}

#' @keywords internal
c2 <- function(x, phi) {
  out <- lchoose(1, 1 * x)
  return(out)
}

#' @keywords internal
link <- function(mu) {
  return(log(mu / (1 - mu)))
}

#' @keywords internal
calc_phi <- function() {
  return(1)
}

#' The Binomial Distribution
#'
#' @inheritParams dnormalt
#' @return dbinomalt gives the density
#' @examples
#' library(altForm)
#'
#' set.seed(1)
#' x <- rbinom(100, 10, .5)
#' dbinomalt(x, .5)
#'
#' @export
dbinomalt <- create_pdf_exponential_form(a, b, c2, link, calc_phi, FALSE, 0)
