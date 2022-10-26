#' @keywords internal
a <- function(phi) {
  return(phi)
}

#' @keywords internal
b <- function(theta, size) {
  return(size * log(1 + exp(theta)))
}

#' @keywords internal
c2 <- function(x, size) {
  out <- lchoose(size, x)
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
#' @param size number of trials (zero or more).
#' @return dbinomalt gives the density
#' @inherit dnormalt source
#' @examples
#' library(altForm)
#'
#' set.seed(1)
#' x <- rbinom(10, 2, .5)
#' dbinomalt(x, .5, 2)
#'
#' pbinomalt(x, .5, 2)
#'
#' @export
dbinomalt <- altForm:::create_pmf_exponential_form(a, b, c2, link, calc_phi, 0)

#' @rdname dbinomalt
#' @inheritParams pnormalt
#' @export
pbinomalt <- altForm:::create_cmf_exponential_form(dbinomalt, 0, Inf, 0, 1)
