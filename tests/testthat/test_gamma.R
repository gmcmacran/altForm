###############################################
# Density
###############################################
test_that("Check structure.", {
  expect_true(class(dgammaalt) == "function")
  expect_true(all(names(formals(dgammaalt)) == c("x", "mu", "sigma", "log")))
})

for (mu in seq(.5, 3, .5)) {
  for (sigma in seq(1, 5, .5)) {
    set.seed(1)
    x <- rgamma(n = 100, shape = mu, rate = sigma)

    # convert mu/sigma to shape/rate
    shape <- mu^2 / sigma^2
    rate <- mu / sigma^2

    d1 <- round(dgammaalt(x, mu, sigma), 10)
    d2 <- round(dgamma(x, shape, rate), 10)

    d3 <- round(dgammaalt(x, mu, sigma, TRUE), 10)
    d4 <- round(dgamma(x = x, shape = shape, rate = rate, log = TRUE), 10)

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
test_that("Check structure.", {
  expect_true(class(pgammaalt) == "function")
  expect_true(all(names(formals(pgammaalt)) == c("q", "mu", "sigma", "lower.tail", "log.p")))
})

# for (mu in seq(.5, 3.5, 1)) {
#   for (sigma in seq(1, 3, 1)) {
#     set.seed(1)
#     q <- rgamma(n = 100, shape = mu, rate = sigma)
#
#     # convert mu/sigma to shape/rate
#     shape <- mu^2 / sigma^2
#     rate <- mu / sigma^2
#
#     d1 <- round(pgammaalt(q, mu, sigma), 10)
#     d2 <- round(pgamma(q, shape, rate), 10)
#
#     d3 <- round(pgammaalt(q, mu, sigma, TRUE, TRUE), 10)
#     d4 <- round(pgamma(q = q, shape = shape, rate = rate, lower.tail = TRUE, log.p = TRUE), 10)
#
#     d5 <- round(pgammaalt(q, mu, sigma, FALSE), 10)
#     d6 <- round(pgamma(q = q, shape = shape, rate = rate, lower.tail = FALSE), 10)
#
#     test_that("Test results of cdf", {
#       expect_equal(d1, d2)
#       expect_equal(d3, d4)
#       expect_equal(d5, d6)
#     })
#   }
# }

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
test_that("q input checking works", {
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

###############################################
# random number generator
###############################################
test_that("Check structure.", {
  expect_true(class(rgammaalt) == "function")
  expect_true(all(names(formals(rgammaalt)) == c("n", "mu", "sigma")))
})

for (mu in seq(1, 3, 1)) {
  for (sigma in seq(1, 3, 1)) {
    set.seed(1)
    x <- rgammaalt(50000, mu, sigma)

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
  expect_error(rgammaalt(c()), "Argument n must have length one.")
  expect_error(rgammaalt(c(5, 10)), "Argument n must have length one.")
  expect_error(rgammaalt("foo"), "Argument n must be numeric.")
  expect_error(rgammaalt(0), "Argument n must be positive.")
})

test_that("mu input checking works", {
  expect_error(rgammaalt(10, c(1, 2)), "Argument mu must have length one.")
  expect_error(rgammaalt(10, "foo"), "Argument mu must be numeric.")
  expect_error(rgammaalt(10, -1), "Argument mu must be positive.")
})

test_that("sigma input checking works", {
  expect_error(rgammaalt(10, 1, c(1, 2)), "Argument sigma must have length one.")
  expect_error(rgammaalt(10, 1, "foo"), "Argument sigma must be numeric.")
  expect_error(rgammaalt(10, 1, -1), "sigma must be positive.")
})
