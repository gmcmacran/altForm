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
#' x <- rbinomalt(10, .5, 2)
#'
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

#' @rdname dbinomalt
#' @param n number of observations. Must be length 1
#' @export
rbinomalt <- function(n, mu, size) {
  if (length(n) != 1) {
    stop("Argument n must have length one.")
  }
  if (!is.numeric(n)) {
    stop("Argument n must be numeric.")
  }
  if (n <= 0) {
    stop("Argument n must be positive.")
  }
  if (length(mu) != 1) {
    stop("Argument mu must have length one.")
  }
  if (!is.numeric(mu)) {
    stop("Argument mu must be numeric.")
  }
  if (mu <= 0) {
    stop("Argument mu must be greater than zero.")
  }
  if (mu >= 1) {
    stop("Argument mu must be less than one.")
  }
  if (length(size) != 1) {
    stop("Argument size must have length one.")
  }
  if (!is.numeric(size)) {
    stop("Argument size must be numeric.")
  }
  if (size <= 0) {
    stop("Argument size must be greater than 0.")
  }
  if (size >= 10000) {
    stop("Argument size must be less than 10,000.")
  }

  out <- stats::rbinom(n = n, prob = mu, size = size)
  return(out)
}
