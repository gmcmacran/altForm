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
#' x <- rbinom(100, 2, .5)
#' dbinomalt(x, .5, 2)
#'
#' @export
dbinomalt <- create_pdf_exponential_form_with_size(a, b, c2, link, calc_phi, 0)

#' The Bernoulli Distribution
#'
#' @inheritParams dnormalt
#' @return dbernalt gives the density
#' @examples
#' library(altForm)
#'
#' set.seed(1)
#' x <- rbinom(100, 1, .5)
#' dbernalt(x, .5)
#'
#' @export
dbernalt <- function(x, mu, log = FALSE) {
  out <- dbinomalt(x, mu, 1, log)
  return(out)
}