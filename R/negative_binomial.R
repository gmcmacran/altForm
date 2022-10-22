#' @keywords internal
a <- function(phi) {
  return(phi)
}

#' @keywords internal
b <- function(theta, size) {
  return(-1 * size * log(1 - exp(theta)))
}

#' @keywords internal
c2 <- function(x, size) {
  out <- lchoose(x + size - 1, size - 1)
  return(out)
}

#' @keywords internal
link <- function(mu) {
  return(log(1 - mu))
}

#' @keywords internal
calc_phi <- function() {
  return(1)
}

#' The Negative Binomial Distribution
#'
#' @inheritParams dnormalt
#' @param size target for number of successful trials.
#' @return dnbinomalt gives the density
#' @inherit dnormalt source
#' @examples
#' library(altForm)
#'
#' set.seed(1)
#' x <- rnbinom(10, 7, .5)
#' dnbinomalt(x, .5, 7)
#'
#' @export
dnbinomalt <- create_pdf_exponential_form_with_size(a, b, c2, link, calc_phi, 0)

#' The Geometric Distribution
#'
#' @inheritParams dnormalt
#' @return dgeomalt gives the density
#' @examples
#' library(altForm)
#'
#' set.seed(1)
#' x <- rgeom(10, .5)
#' dgeomalt(x, .5)
#'
#' @export
dgeomalt <- function(x, mu, log = FALSE) {
  out <- dnbinomalt(x, mu, 1, log)
  return(out)
}
