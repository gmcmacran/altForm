###############################################
# Density
###############################################
# Generalized linear models and extenstions
ref_pdf <- function(x, mu, sigma, log = FALSE) {
  phi <- sigma^2 / mu^2

  out <- 1 / (x * gamma(1 / phi))
  out <- out * (x / (mu * phi))^(1 / phi)
  out <- out * exp(-x / (mu * phi))

  if (log) {
    out <- log(out)
  }
  return(out)
}

test_that("Check structure.", {
  expect_true(class(dgammaalt) == "function")
  expect_true(all(names(formals(dgammaalt)) == c("x", "mu", "sigma", "log")))
})

for (mu in seq(.5, 3, .5)) {
  for (sigma in seq(1, 5, .5)) {
    set.seed(1)
    x <- rgamma(n = 100, shape = mu, rate = sigma)

    d1 <- round(dgammaalt(x, mu, sigma), 10)
    d2 <- round(ref_pdf(x, mu, sigma), 10)

    d3 <- round(dgammaalt(x, mu, sigma, TRUE), 10)
    d4 <- round(ref_pdf(x, mu, sigma, TRUE), 10)

    test_that("Test results of density", {
      expect_equal(d1, d2)
      expect_equal(d3, d4)
    })
  }
}

###############################################
# Density Input checking
###############################################
test_that("x input checking works", {
  expect_error(dgammaalt(c()), "Argument x must have positive length.")
  expect_error(dgammaalt(rep("foo", 50)), "Argument x must be numeric.")
  expect_error(dgammaalt(-1), NULL)
})

set.seed(1)
x <- statmod::rinvgauss(100, 1, 2)
test_that("mu input checking works", {
  expect_error(dgammaalt(x, c(1, 2)), "Argument mu must have length one.")
  expect_error(dgammaalt(x, "foo"), "Argument mu must be numeric.")
  expect_error(dgammaalt(x, -1), "Argument mu must be greater than 0")
})

test_that("sigma input checking works", {
  expect_error(dgammaalt(x, 1, c(1, 2)), "Argument sigma must have length one.")
  expect_error(dgammaalt(x, 1, "foo"), "Argument sigma must be numeric.")
  expect_error(dgammaalt(x, 1, -1), "sigma must be above 0.")
})

test_that("log input checking works", {
  expect_error(dgammaalt(x, 1, 1, c(TRUE, FALSE)), "Argument log must have length one.")
  expect_error(dgammaalt(x, 1, 1, "foo"), "Argument log must be logical.")
})

###############################################
# cdf
###############################################
ref_cdf <- function(q, mu, sigma, lower.tail = TRUE, log.p = FALSE) {
  helper <- function(q) {
    ref_pdf(q, mu = mu, sigma = sigma, log = FALSE)
  }

  p <- vector(mode = "numeric", length = length(q))
  for (i in seq_along(p)) {
    utils::capture.output(p[i] <- pracma::quadgr(f = helper, a = .Machine$double.eps, b = q[i])$value)
  }

  if (!lower.tail) {
    p <- 1 - p
  }

  if (log.p) {
    p <- log(p)
  }

  return(p)
}

test_that("Check structure.", {
  expect_true(class(pgammaalt) == "function")
  expect_true(all(names(formals(pgammaalt)) == c("q", "mu", "sigma", "lower.tail", "log.p")))
})

for (mu in seq(.5, 3.5, 1)) {
  for (sigma in seq(1, 3, 1)) {
    set.seed(1)
    q <- rgamma(n = 100, shape = mu, rate = sigma)

    d1 <- round(pgammaalt(q, mu, sigma), 10)
    d2 <- round(ref_cdf(q, mu, sigma), 10)

    d3 <- round(pgammaalt(q, mu, sigma, TRUE, TRUE), 10)
    d4 <- round(ref_cdf(q, mu, sigma, TRUE, TRUE), 10)

    test_that("Test results of cdf", {
      expect_equal(d1, d2)
      expect_equal(d3, d4)
    })
  }
}

# Total area is 1.
# for (mu in seq(.5, 3.5, 1)) {
#   for (sigma in seq(1, 3, 1)) {
#     d1 <- pgammaalt(Inf, mu, sigma)
#
#     d3 <- pgammaalt(Inf, mu, sigma, TRUE, TRUE)
#
#     test_that("Test results of cdf", {
#       expect_equal(d1, 1)
#       expect_equal(d3, 0)
#     })
#   }
# }

###############################################
# cdf Input checking
###############################################
test_that("x input checking works", {
  expect_error(pgammaalt(c()), "Argument q must have positive length.")
  expect_error(pgammaalt(rep("foo", 50)), "Argument q must be numeric.")
  expect_error(pgammaalt(-1), NULL)
})

set.seed(1)
q <- rgamma(n = 100, shape = 1, rate = 2)
test_that("mu input checking works", {
  expect_error(pgammaalt(q, c(1, 2)), "Argument mu must have length one.")
  expect_error(pgammaalt(q, "foo"), "Argument mu must be numeric.")
  expect_error(pgammaalt(q, -1), "Argument mu must be greater than 0")
})

test_that("sigma input checking works", {
  expect_error(pgammaalt(q, 1, c(1, 2)), "Argument sigma must have length one.")
  expect_error(pgammaalt(q, 1, "foo"), "Argument sigma must be numeric.")
  expect_error(pgammaalt(q, 1, -1), "sigma must be above 0.")
})

test_that("lower.tail input checking works", {
  expect_error(pgammaalt(q, 1, 1, c(TRUE, FALSE)), "Argument lower.tail must have length one.")
  expect_error(pgammaalt(q, 1, 1, "foo"), "Argument lower.tail must be logical.")
})

test_that("log.p input checking works", {
  expect_error(pgammaalt(q, 1, 1, TRUE, c(TRUE, FALSE)), "Argument log.p must have length one.")
  expect_error(pgammaalt(q, 1, 1, TRUE, "foo"), "Argument log.p must be logical.")
})
