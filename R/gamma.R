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
#' x <- rgammaalt(10, 3, 4)
#' dgammaalt(x, 3, 4)
#'
#' pgammaalt(x, 3, 4)
#'
#' @export
dgammaalt <- altForm:::create_pdf_exponential_form(a, b, c2, link, calc_phi, TRUE, .Machine$double.eps, Inf, 0, Inf)

#' @rdname dgammaalt
#' @inheritParams pnormalt
#' @export
pgammaalt <- altForm:::create_cdf_exponential_form(dgammaalt, .Machine$double.eps, Inf, 0, Inf)

#' @rdname dgammaalt
#' @param n number of observations. Must be length 1
#' @export
rgammaalt <- function(n, mu, sigma) {
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
  if (mu < .Machine$double.eps) {
    stop("Argument mu must be positive.")
  }
  if (length(sigma) != 1) {
    stop("Argument sigma must have length one.")
  }
  if (!is.numeric(sigma)) {
    stop("Argument sigma must be numeric.")
  }
  if (sigma <= 0) {
    stop("Argument sigma must be positive.")
  }

  # convert mu/sigma to shape/rate
  shape <- mu^2 / sigma^2
  rate <- mu / sigma^2

  out <- stats::rgamma(n, shape = shape, rate = rate)
  return(out)
}
