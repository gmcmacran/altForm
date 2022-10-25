
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
      expect_true(all(d1 == d2))
      expect_true(all(d3 == d4))
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
