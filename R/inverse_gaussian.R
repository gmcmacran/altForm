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
#' x <- rinvgaussalt(10, 2, 3)
#' dinvgaussalt(x, 2, 3)
#'
#' pinvgaussalt(x, 2, 3)
#'
#' @export
dinvgaussalt <- altForm:::create_pdf_exponential_form(a, b, c2, link, calc_phi, TRUE, .Machine$double.eps, Inf, 0, Inf)

#' @rdname dinvgaussalt
#' @inheritParams pnormalt
#' @export
pinvgaussalt <- altForm:::create_cdf_exponential_form(dinvgaussalt, .Machine$double.eps, Inf, 0, Inf)

#' @rdname dinvgaussalt
#' @param n number of observations. Must be length 1
#' @export
rinvgaussalt <- function(n, mu, sigma) {
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

  # convert mu/sigma to mu/dispersion
  dispersion <- sigma^2 / mu^3

  out <- statmod::rinvgauss(n, mean = mu, dispersion = dispersion)

  return(out)
}
