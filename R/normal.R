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

#' The Normal Distribution.
#'
#' @param x a numeric vector.
#' @param mu a number indicating the mean.
#' @param sigma a number indicating the standard deviation.
#' @param log logical; if TRUE, probabilities p are given as log(p).
#' @param lower.tail logical; if TRUE (default),
#' @return dnormalt gives the density, pnormalt gives the cumulative distribution function.
#' @source Hardin, James William., and Joseph Hilbe. Generalized Linear Models and Extensions. Stata Press, 2012.
#' @examples
#' library(altForm)
#'
#' set.seed(1)
#' x <- rnormalt(10, 0, 1)
#' dnormalt(x, 0, 1)
#'
#' pnormalt(x, 0, 1)
#'
#' @export
dnormalt <- altForm:::create_pdf_exponential_form(a, b, c2, link, calc_phi, TRUE, -Inf, Inf, -Inf, Inf)

#' @rdname dnormalt
#' @param q a numeric vector.
#' @param log.p logical; if TRUE, probabilities p are given as log(p).
#' @export
pnormalt <- altForm:::create_cdf_exponential_form(dnormalt, -Inf, Inf, -Inf, Inf)

#' @rdname dnormalt
#' @param n number of observations. Must be length 1
#' @export
rnormalt <- function(n, mu, sigma) {
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
  if (length(sigma) != 1) {
    stop("Argument sigma must have length one.")
  }
  if (!is.numeric(sigma)) {
    stop("Argument sigma must be numeric.")
  }
  if (sigma <= 0) {
    stop("Argument sigma must be positive.")
  }

  box_muller <- function(n) {
    halfN <- ceiling(n / 2)

    U <- stats::runif(halfN, 0, 1)
    V <- stats::runif(halfN, 0, 1)

    X <- (-2 * log(U))^.5 * cos(2 * pi * V)
    Y <- (-2 * log(U))^.5 * sin(2 * pi * V)

    out <- c(X, Y)
    # remove last element if n is odd
    if (n %% 2 != 0) {
      out <- out[-length(out)]
    }

    return(out)
  }
  out <- box_muller(n)
  out <- out * sigma + mu
  return(out)
}
