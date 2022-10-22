
###############################################
# Density
###############################################
test_that("Check structure.", {
  expect_true(class(dbinomalt) == "function")
  expect_true(all(names(formals(dbinomalt)) == c("x", "mu", "size", "log")))
})

for (mu in seq(.01, .99, .05)) {
  for (n in c(1, 3, 5, 7, 9, 500, 9999)) {
    set.seed(1)
    x <- rbinom(100, n, mu)

    d1 <- round(dbinomalt(x, mu, n), 10)
    d2 <- round(dbinom(x, n, mu), 10)

    d3 <- round(dbinomalt(x, mu, n, TRUE), 10)
    d4 <- round(dbinom(x, n, mu, TRUE), 10)

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
  expect_error(dbinomalt(c()), "Argument x must have positive length.")
  expect_error(dbinomalt(rep("foo", 50)), "Argument x must be numeric.")
  expect_error(dbinomalt(rep(-10, 50)), "All elements in x must be greater than or equal to 0")
})

set.seed(1)
x <- rbinom(100, 1, .5)
test_that("mu input checking works", {
  expect_error(dbinomalt(x, c(1, 2)), "Argument mu must have length one.")
  expect_error(dbinomalt(x, "foo"), "Argument mu must be numeric.")
  expect_error(dbinomalt(x, 0), "Argument mu must be greater than zero.")
  expect_error(dbinomalt(x, 1), "Argument mu must be less than one.")
})

test_that("size input checking works", {
  expect_error(dbinomalt(x, .5, c(10, 11)), "Argument size must have length one.")
  expect_error(dbinomalt(x, .5, "foo"), "Argument size must be numeric.")
  expect_error(dbinomalt(x, .5, -1), "Argument size must be greater than 0.")
  expect_error(dbinomalt(x, .5, 10000), "Argument size must be less than 10,000.")
})

test_that("log input checking works", {
  expect_error(dbinomalt(x, .5, 10, c(TRUE, FALSE)), "Argument log must have length one.")
  expect_error(dbinomalt(x, .5, 10, "foo"), "Argument log must be logical.")
})

###############################################
# Density
###############################################
test_that("Check structure.", {
  expect_true(class(dbernalt) == "function")
  expect_true(all(names(formals(dbernalt)) == c("x", "mu", "log")))
})

for (mu in seq(.01, .99, .05)) {
  set.seed(1)
  x <- Rlab::rbern(100, mu)

  d1 <- round(dbernalt(x, mu), 10)
  d2 <- round(Rlab::dbern(x, mu), 10)

  d3 <- round(dbernalt(x, mu, TRUE), 10)
  d4 <- round(Rlab::dbern(x, mu, TRUE), 10)

  test_that("Test results of density", {
    expect_equal(d1, d2)
    expect_equal(d3, d4)
  })
}

###############################################
# Density Input checking
###############################################
test_that("x input checking works", {
  expect_error(dbernalt(c()), "Argument x must have positive length.")
  expect_error(dbernalt(rep("foo", 50)), "Argument x must be numeric.")
  expect_error(dbernalt(rep(-10, 50)), "All elements in x must be greater than or equal to 0")
})

set.seed(1)
x <- rbinom(100, 1, .5)
test_that("mu input checking works", {
  expect_error(dbernalt(x, c(1, 2)), "Argument mu must have length one.")
  expect_error(dbernalt(x, "foo"), "Argument mu must be numeric.")
  expect_error(dbernalt(x, 0), "Argument mu must be greater than zero.")
  expect_error(dbernalt(x, 1), "Argument mu must be less than one.")
})

test_that("log input checking works", {
  expect_error(dbernalt(x, .5, c(TRUE, FALSE)), "Argument log must have length one.")
  expect_error(dbernalt(x, .5, "foo"), "Argument log must be logical.")
})
