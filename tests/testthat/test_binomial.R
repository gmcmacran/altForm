
###############################################
# Density
###############################################
test_that("Check structure.", {
  expect_true(class(dbinomalt) == "function")
  expect_true(all(names(formals(dbinomalt)) == c("x", "mu", "log")))
})

for (mu in seq(.01, .99, .01)) {
  set.seed(1)
  x <- rbinom(100, 1, mu)

  d1 <- round(dbinomalt(x, mu), 10)
  d2 <- round(dbinom(x, 1, mu), 10)

  d3 <- round(dbinomalt(x, mu, TRUE), 10)
  d4 <- round(dbinom(x, 1, mu, TRUE), 10)

  test_that("Test results of density", {
    expect_true(all(d1 == d2))
    expect_true(all(d3 == d4))
  })
}

###############################################
# Density Input checking
###############################################
test_that("x input checking works", {
  expect_error(dbinomalt(c()), "Argument x must have positive length.")
  expect_error(dbinomalt(rep("foo", 50)), "Argument x must be numeric.")
  expect_error(dbinomalt(rep(-10, 50)), "All elements in x must be greater than or equal to 0")
  expect_error(dbinomalt(rep(10, 50)), "All elements in x must be less than or equal to 1")
})

set.seed(1)
x <- rbinom(100, 1, .5)
test_that("mu input checking works", {
  expect_error(dbinomalt(x, c(1, 2)), "Argument mu must have length one.")
  expect_error(dbinomalt(x, "foo"), "Argument mu must be numeric.")
  expect_error(dbinomalt(x, 0), "Argument mu must be greater than 0")
  expect_error(dbinomalt(x, 1), "Argument mu must be less than 1")
})

test_that("log input checking works", {
  expect_error(dbinomalt(x, .5, c(TRUE, FALSE)), "Argument log must have length one.")
  expect_error(dbinomalt(x, .5, "foo"), "Argument log must be logical.")
})
