
###############################################
# Density
###############################################
test_that("Check structure.", {
  expect_true(class(dnormalt) == "function")
  expect_true(all(names(formals(dnormalt)) == c("x", "mu", "sigma", "log")))
})

for (mu in seq(-3, 3, .5)) {
  for (sigma in seq(1, 5, .5)) {
    set.seed(1)
    x <- rnorm(100, mu, sigma)

    d1 <- round(dnormalt(x, mu, sigma), 10)
    d2 <- round(dnorm(x, mu, sigma), 10)

    d3 <- round(dnormalt(x, mu, sigma, TRUE), 10)
    d4 <- round(dnorm(x, mu, sigma, TRUE), 10)

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
  expect_error(dnormalt(c()), "Argument x must have positive length.")
  expect_error(dnormalt(rep("foo", 50)), "Argument x must be numeric.")
})

set.seed(1)
x <- rnorm(50, 1, 2)
test_that("mu input checking works", {
  expect_error(dnormalt(x, c(1, 2)), "Argument mu must have length one.")
  expect_error(dnormalt(x, "foo"), "Argument mu must be numeric.")
})

test_that("sigma input checking works", {
  expect_error(dnormalt(x, 1, c(1, 2)), "Argument sigma must have length one.")
  expect_error(dnormalt(x, 1, "foo"), "Argument sigma must be numeric.")
  expect_error(dnormalt(x, 1, -1), "sigma must be above 0.")
})

test_that("log input checking works", {
  expect_error(dnormalt(x, 1, 1, c(TRUE, FALSE)), "Argument log must have length one.")
  expect_error(dnormalt(x, 1, 1, "foo"), "Argument log must be logical.")
})

###############################################
# cdf
###############################################
test_that("Check structure.", {
  expect_true(class(pnormalt) == "function")
  expect_true(all(names(formals(pnormalt)) == c("q", "mu", "sigma", "lower.tail", "log.p")))
})

for (mu in seq(-1, 1, 1)) {
  for (sigma in seq(1, 3, 1)) {
    set.seed(1)
    q <- rnorm(100, mu, sigma)

    d1 <- round(pnormalt(q, mu, sigma), 10)
    d2 <- round(pnorm(q, mu, sigma), 10)

    d3 <- round(pnormalt(q, mu, sigma, TRUE, TRUE), 10)
    d4 <- round(pnorm(q, mu, sigma, TRUE, TRUE), 10)

    d5 <- round(pnormalt(q, mu, sigma, FALSE), 10)
    d6 <- round(pnorm(q, mu, sigma, FALSE), 10)

    test_that("Test results of cdf", {
      expect_equal(d1, d2)
      expect_equal(d3, d4)
      expect_equal(d5, d6)
    })
  }
}

# Total area is 1.
for (mu in seq(-1, 1, 1)) {
  for (sigma in seq(1, 3, 1)) {
    d1 <- pnormalt(Inf, mu, sigma)

    d3 <- pnormalt(Inf, mu, sigma, TRUE, TRUE)

    test_that("Test results of cdf", {
      expect_equal(d1, 1)
      expect_equal(d3, 0)
    })
  }
}

###############################################
# cdf Input checking
###############################################
test_that("q input checking works", {
  expect_error(pnormalt(c()), "Argument q must have positive length.")
  expect_error(pnormalt(rep("foo", 50)), "Argument q must be numeric.")
})

set.seed(1)
q <- rnorm(50, 1, 2)
test_that("mu input checking works", {
  expect_error(pnormalt(q, c(1, 2)), "Argument mu must have length one.")
  expect_error(pnormalt(q, "foo"), "Argument mu must be numeric.")
})

test_that("sigma input checking works", {
  expect_error(pnormalt(q, 1, c(1, 2)), "Argument sigma must have length one.")
  expect_error(pnormalt(q, 1, "foo"), "Argument sigma must be numeric.")
  expect_error(pnormalt(q, 1, -1), "sigma must be above 0.")
})

test_that("lower.tail input checking works", {
  expect_error(pnormalt(q, 1, 1, c(TRUE, FALSE)), "Argument lower.tail must have length one.")
  expect_error(pnormalt(q, 1, 1, "foo"), "Argument lower.tail must be logical.")
})

test_that("log.p input checking works", {
  expect_error(pnormalt(q, 1, 1, TRUE, c(TRUE, FALSE)), "Argument log.p must have length one.")
  expect_error(pnormalt(q, 1, 1, TRUE, "foo"), "Argument log.p must be logical.")
})

###############################################
# random number generator
###############################################
test_that("Check structure.", {
  expect_true(class(rnormalt) == "function")
  expect_true(all(names(formals(rnormalt)) == c("n", "mu", "sigma")))
})

for (mu in seq(-1, 1, 1)) {
  for (sigma in seq(1, 3, 1)) {
    set.seed(1)
    x <- rnormalt(50000, mu, sigma)

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
test_that("n input checking works", {
  expect_error(rnormalt(c()), "Argument n must have length one.")
  expect_error(rnormalt(c(5, 10)), "Argument n must have length one.")
  expect_error(rnormalt("foo"), "Argument n must be numeric.")
  expect_error(rnormalt(0), "Argument n must be positive.")
})

test_that("mu input checking works", {
  expect_error(rnormalt(10, c(1, 2)), "Argument mu must have length one.")
  expect_error(rnormalt(10, "foo"), "Argument mu must be numeric.")
})

test_that("sigma input checking works", {
  expect_error(rnormalt(10, 1, c(1, 2)), "Argument sigma must have length one.")
  expect_error(rnormalt(10, 1, "foo"), "Argument sigma must be numeric.")
  expect_error(rnormalt(10, 1, -1), "sigma must be positive.")
})
