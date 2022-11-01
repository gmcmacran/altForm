# https://en.wikipedia.org/wiki/Normal_distribution
internal_normal <- function(n, mu, sigma){
  box_muller <- function(n) {
    halfN <- ceiling(n/2)

    U <- runif(halfN, 0, 1)
    V <- runif(halfN, 0, 1)

    X <- (-2*log(U)) ^ .5*cos(2*pi*V)
    Y <- (-2*log(U))^.5*sin(2*pi*V)

    out <- c(X, Y)
    # remove last element if n is odd
    if (n %% 2 != 0)
      out <- out[-length(out)]

    return(out)
  }
  out <- box_muller(n)
  out <- out*sigma + mu
  return(out)
}

# http://www.columbia.edu/~ks20/4703-Sigman/4703-07-Notes-ARM.pdf
create_acceptance_rejection <- function(pdf) {

  # gamma(1, 1) shape/scale or exponential w/ lambda = 1
  # inverse transform
  candidate <- function(n, mu, sigma) {
    U <- runif(n, 0, 1)
    out <- -1 * log(U)
    return(out)
  }

  random_generator <- function(n, mu, sigma) {
    # find M
    helper <- function(x, mu, sigma) {
      ratio <- pdf(x, mu, sigma) / stats::dgamma(x, 1, 1)
      ratio <- -1 * ratio # max problem to min problem
      return(ratio)
    }
    M <- stats::optim(.Machine$double.eps, helper, lower = .Machine$double.eps, upper = 99, method="L-BFGS-B", mu = mu, sigma = sigma)
    M <- M$value

    calc_ratio <- function(x, mu, sigma, M) {
      return(pdf(x, mu, sigma) / (M * stats::dgamma(x, 1, 1)))
    }

    out <- c()
    while(length(out) < n) {
      U <- runif(n*2, 0, 1)

      temp <- candidate(n*2, mu, sigma)
      ratio <- calc_ratio(temp, mu, sigma = sigma, M)

      bools <- U <= ratio
      out <- c(out, temp[bools])
    }
    out <- out[1:n]
    return(out)
  }
}

pdf <- altForm::dgammaalt
n <- 100
mu <- 3
sigma <- 2

x <- c(.5)
helper(x, mu, sigma)
helper(XLB, mu, sigma)
helper(99, mu, sigma)

holder1 <- c(.01, 1:100)
holder2 <- calc_ratio(holder1, 3, 2)

holder1 <- holder1[which(is.finite(holder2))]
holder2 <- holder1[which(is.finite(holder2))]
plot(holder1, holder2)
