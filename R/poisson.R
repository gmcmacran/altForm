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
#' x <- rpoisalt(10, 1)
#'
#' dpoisalt(x, 1)
#'
#' ppoisalt(x, 1)
#'
#' @export
dpoisalt <- altForm:::create_pdf_exponential_form(a, b, c2, link, calc_phi, FALSE, 0, Inf, 0, 143) # 143 for numerical accuracy.


#' @rdname dpoisalt
#' @inheritParams pnormalt
#' @export
ppoisalt <- altForm:::create_cmf_exponential_form(dpoisalt, 0, Inf, 0, 143)

#' @rdname dpoisalt
#' @param n number of observations. Must be length 1
#' @export
rpoisalt <- function(n, mu) {
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
  if (mu >= 143) {
    stop("Argument mu must be less than 143.")
  }

  out <- stats::rpois(n = n, lambda = mu)
  return(out)
}
