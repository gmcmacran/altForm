###############################################
# Density
###############################################
# https://www.sfu.ca/sasdoc/sashtml/insight/chap39/sect4.htm
ref_pdf <- function(x, mu, sigma, log = FALSE) {
  out <- (-1 / (2 * mu^2 * x)) * ((x - mu) / sigma)^2
  out <- exp(out)
  out <- out / ((2 * pi * x^3)^.5 * sigma)
  if (log) {
    out <- log(out)
  }
  return(out)
}

test_that("Check structure.", {
  expect_true(class(dinvgaussalt) == "function")
  expect_true(all(names(formals(dinvgaussalt)) == c("x", "mu", "sigma", "log")))
})

for (mu in seq(.5, 3, .5)) {
  for (sigma in seq(1, 5, .5)) {
    set.seed(1)
    x <- statmod::rinvgauss(100, mu, 10)

    d1 <- round(dinvgaussalt(x, mu, sigma), 10)
    d2 <- round(ref_pdf(x, mu, sigma), 10)

    d3 <- round(dinvgaussalt(x, mu, sigma, TRUE), 10)
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
  expect_error(dinvgaussalt(c()), "Argument x must have positive length.")
  expect_error(dinvgaussalt(rep("foo", 50)), "Argument x must be numeric.")
  expect_error(dinvgaussalt(-1), NULL)
})

set.seed(1)
x <- statmod::rinvgauss(100, 1, 2)
test_that("mu input checking works", {
  expect_error(dinvgaussalt(x, c(1, 2)), "Argument mu must have length one.")
  expect_error(dinvgaussalt(x, "foo"), "Argument mu must be numeric.")
  expect_error(dinvgaussalt(x, -1), "Argument mu must be greater than 0")
})

test_that("sigma input checking works", {
  expect_error(dinvgaussalt(x, 1, c(1, 2)), "Argument sigma must have length one.")
  expect_error(dinvgaussalt(x, 1, "foo"), "Argument sigma must be numeric.")
  expect_error(dinvgaussalt(x, 1, -1), "sigma must be above 0.")
})

test_that("log input checking works", {
  expect_error(dinvgaussalt(x, 1, 1, c(TRUE, FALSE)), "Argument log must have length one.")
  expect_error(dinvgaussalt(x, 1, 1, "foo"), "Argument log must be logical.")
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
  expect_true(class(pinvgaussalt) == "function")
  expect_true(all(names(formals(pinvgaussalt)) == c("q", "mu", "sigma", "lower.tail", "log.p")))
})

for (mu in seq(.5, 3.5, 1)) {
  for (sigma in seq(1, 3, 1)) {
    set.seed(1)
    q <- statmod::rinvgauss(100, mu, 10)

    d1 <- round(pinvgaussalt(q, mu, sigma), 10)
    d2 <- round(ref_cdf(q, mu, sigma), 10)

    d3 <- round(pinvgaussalt(q, mu, sigma, TRUE, TRUE), 10)
    d4 <- round(ref_cdf(q, mu, sigma, TRUE, TRUE), 10)

    test_that("Test results of cdf", {
      expect_equal(d1, d2)
      expect_equal(d3, d4)
    })
  }
}

# Total area is 1.
for (mu in seq(.5, 3.5, 1)) {
  for (sigma in seq(1, 3, 1)) {
    d1 <- pinvgaussalt(Inf, mu, sigma)

    d3 <- pinvgaussalt(Inf, mu, sigma, TRUE, TRUE)

    test_that("Test results of cdf", {
      expect_equal(d1, 1)
      expect_equal(d3, 0)
    })
  }
}

###############################################
# cdf Input checking
###############################################
test_that("x input checking works", {
  expect_error(pinvgaussalt(c()), "Argument q must have positive length.")
  expect_error(pinvgaussalt(rep("foo", 50)), "Argument q must be numeric.")
  expect_error(pinvgaussalt(-1), NULL)
})

set.seed(1)
q <- statmod::rinvgauss(100, mu, 10)
test_that("mu input checking works", {
  expect_error(pinvgaussalt(q, c(1, 2)), "Argument mu must have length one.")
  expect_error(pinvgaussalt(q, "foo"), "Argument mu must be numeric.")
  expect_error(pinvgaussalt(x, -1), "Argument mu must be greater than 0")
})

test_that("sigma input checking works", {
  expect_error(pinvgaussalt(q, 1, c(1, 2)), "Argument sigma must have length one.")
  expect_error(pinvgaussalt(q, 1, "foo"), "Argument sigma must be numeric.")
  expect_error(pinvgaussalt(q, 1, -1), "sigma must be above 0.")
})

test_that("lower.tail input checking works", {
  expect_error(pinvgaussalt(q, 1, 1, c(TRUE, FALSE)), "Argument lower.tail must have length one.")
  expect_error(pinvgaussalt(q, 1, 1, "foo"), "Argument lower.tail must be logical.")
})

test_that("log.p input checking works", {
  expect_error(pinvgaussalt(q, 1, 1, TRUE, c(TRUE, FALSE)), "Argument log.p must have length one.")
  expect_error(pinvgaussalt(q, 1, 1, TRUE, "foo"), "Argument log.p must be logical.")
})

###############################################
# random number generator
###############################################
test_that("Check structure.", {
  expect_true(class(rinvgaussalt) == "function")
  expect_true(all(names(formals(rinvgaussalt)) == c("n", "mu", "sigma")))
})

for (mu in seq(1, 3, 1)) {
  for (sigma in seq(1, 3, 1)) {
    set.seed(1)
    x <- rinvgaussalt(50000, mu, sigma)

    xbar <- mean(x)
    sd_x <- sd(x)

    test_that("Test results of random generator", {
      expect_equal(length(x), 50000)
      expect_true(abs(mu - xbar) <= .1)
      expect_true(abs(sigma - sd_x) <= .1)
    })
  }
}

###############################################
# random number generator Input checking
###############################################
test_that("x input checking works", {
  expect_error(rinvgaussalt(c()), "Argument n must have length one.")
  expect_error(rinvgaussalt(c(5, 10)), "Argument n must have length one.")
  expect_error(rinvgaussalt("foo"), "Argument n must be numeric.")
  expect_error(rinvgaussalt(-10), "Argument n must be positive.")
})

test_that("mu input checking works", {
  expect_error(rinvgaussalt(10, c(1, 2)), "Argument mu must have length one.")
  expect_error(rinvgaussalt(10, "foo"), "Argument mu must be numeric.")
  expect_error(rinvgaussalt(10, -1), "Argument mu must be positive.")
})

test_that("sigma input checking works", {
  expect_error(rinvgaussalt(10, 1, c(1, 2)), "Argument sigma must have length one.")
  expect_error(rinvgaussalt(10, 1, "foo"), "Argument sigma must be numeric.")
  expect_error(rinvgaussalt(10, 1, -1), "sigma must be positive.")
})
