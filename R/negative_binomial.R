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
#' pnbinomalt(x, .5, 7)
#'
#' @export
dnbinomalt <- altForm:::create_pmf_exponential_form(a, b, c2, link, calc_phi, 0)

#' @rdname dnbinomalt
#' @inheritParams pnormalt
#' @export
pnbinomalt <- altForm:::create_cmf_exponential_form(dnbinomalt, 0, Inf, 0, 1)
