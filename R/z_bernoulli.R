#' The Bernoulli Distribution
#'
#' @inheritParams dnormalt
#' @return dbernalt gives the density
#' @examples
#' library(altForm)
#'
#' set.seed(1)
#' x <- rbernalt(10, .25)
#'
#' dbernalt(x, .25)
#'
#' pbernalt(x, .25)
#'
#' @export
dbernalt <- function(x, mu, log = FALSE) {
  out <- dbinomalt(x, mu, 1, log)
  return(out)
}

#' @rdname dbernalt
#' @inheritParams pnormalt
#' @export
pbernalt <- function(q, mu, lower.tail = TRUE, log.p = FALSE) {
  out <- pbinomalt(q, mu, 1, lower.tail, log.p)
  return(out)
}

#' @rdname dbernalt
#' @param n number of observations. Must be length 1
#' @export
rbernalt <- function(n, mu) {
  out <- rbinomalt(n, mu, 1)
  return(out)
}
