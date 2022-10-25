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

    d1 <- round(dgammaalt(x, mu, sigma), 10)
    d2 <- round(ref_pdf(x, mu, sigma), 10)

    d3 <- round(dgammaalt(x, mu, sigma, TRUE), 10)
    d4 <- round(ref_pdf(x, mu, sigma, TRUE), 10)

    test_that("Test results of density", {
      expect_true(all(d1 == d2))
      expect_true(all(d3 == d4))
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
