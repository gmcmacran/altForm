#' The Geometric Distribution
#'
#' @inheritParams dnormalt
#' @return dgeomalt gives the density
#' @examples
#' library(altForm)
#'
#' set.seed(1)
#' x <- rgeomalt(10, .5)
#'
#' dgeomalt(x, .5)
#'
#' pgeomaltalt(x, .5)
#'
#' @export
dgeomalt <- function(x, mu, log = FALSE) {
  out <- dnbinomalt(x, mu, 1, log)
  return(out)
}

#' @rdname dgeomalt
#' @inheritParams pnormalt
#' @export
pgeomaltalt <- function(q, mu, lower.tail = TRUE, log.p = FALSE) {
  out <- pnbinomalt(q, mu, 1, lower.tail, log.p)
  return(out)
}

#' @rdname dgeomalt
#' @param n number of observations. Must be length 1
#' @export
rgeomalt <- function(n, mu) {
  out <- rnbinomalt(n, mu, 1)
  return(out)
}
