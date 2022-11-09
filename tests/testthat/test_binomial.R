
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
# cdf
###############################################
test_that("Check structure.", {
  expect_true(class(pbinomalt) == "function")
  expect_true(all(names(formals(pbinomalt)) == c("q", "mu", "size", "lower.tail", "log.p")))
})

for (mu in seq(.01, .99, .05)) {
  for (n in c(1, 3, 5, 7, 9, 500, 9999)) {
    set.seed(1)
    q <- rbinom(100, n, mu)

    d1 <- round(pbinomalt(q, mu, n), 10)
    d2 <- round(pbinom(q, n, mu), 10)

    d3 <- round(pbinomalt(q, mu, n, TRUE, TRUE), 10)
    d4 <- round(pbinom(q, n, mu, TRUE, TRUE), 10)

    d5 <- round(pbinomalt(q, mu, n, FALSE), 10)
    d6 <- round(pbinom(q, n, mu, FALSE), 10)

    test_that("Test results of cdf", {
      expect_equal(d1, d2)
      expect_equal(d3, d4)
      expect_equal(d5, d6)
    })
  }
}

# Total area is 1.
for (mu in seq(.01, .99, .05)) {
  for (n in c(1, 3, 5, 7, 9, 500, 9999)) {
    d1 <- pbinomalt(n, mu, n)

    d3 <- pbinomalt(n, mu, n, TRUE, TRUE)

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
  expect_error(pbinomalt(c()), "Argument q must have positive length.")
  expect_error(pbinomalt(rep("foo", 50)), "Argument q must be numeric.")
  expect_error(pbinomalt(rep(-10, 50)), "All elements in q must be greater than or equal to 0")
})

set.seed(1)
q <- rbinom(100, 10, .5)
test_that("mu input checking works", {
  expect_error(pbinomalt(q, c(1, 2)), "Argument mu must have length one.")
  expect_error(pbinomalt(q, "foo"), "Argument mu must be numeric.")
  expect_error(pbinomalt(q, 0), "Argument mu must be greater than 0")
  expect_error(pbinomalt(q, 1), "Argument mu must be less than 1")
})

test_that("size input checking works", {
  expect_error(pbinomalt(q, .5, c(10, 20)), "Argument size must have length one.")
  expect_error(pbinomalt(q, .5, "foo"), "Argument size must be numeric.")
  expect_error(pbinomalt(q, .5, 0), "Argument size must be greater than 0.")
  expect_error(pbinomalt(q, .5, 10000), "Argument size must be less than 10,000.")
})

test_that("lower.tail input checking works", {
  expect_error(pbinomalt(q, .5, 10, c(TRUE, FALSE)), "Argument lower.tail must have length one.")
  expect_error(pbinomalt(q, .5, 10, "foo"), "Argument lower.tail must be logical.")
})

test_that("log.p input checking works", {
  expect_error(pbinomalt(q, .5, 10, TRUE, c(TRUE, FALSE)), "Argument log.p must have length one.")
  expect_error(pbinomalt(q, .5, 10, TRUE, "foo"), "Argument log.p must be logical.")
})

###############################################
# random number generator
###############################################
test_that("Check structure.", {
  expect_true(class(rbinomalt) == "function")
  expect_true(all(names(formals(rbinomalt)) == c("n", "mu", "size")))
})

for (mu in seq(.01, .99, .05)) {
  for (size in c(2, 5, 10)) {
    set.seed(1)
    x <- rbinomalt(50000, mu, size)

    xbar <- mean(x)

    test_that("Test results of random generator", {
      expect_equal(length(x), 50000)
      expect_true(abs(mu * size - xbar) <= .1)
    })
  }
}

###############################################
# random number generator Input checking
###############################################
test_that("n input checking works", {
  expect_error(rbinomalt(c()), "Argument n must have length one.")
  expect_error(rbinomalt(c(5, 10)), "Argument n must have length one.")
  expect_error(rbinomalt("foo"), "Argument n must be numeric.")
  expect_error(rbinomalt(0), "Argument n must be positive.")
})

test_that("mu input checking works", {
  expect_error(rbinomalt(10, c(1, 2)), "Argument mu must have length one.")
  expect_error(rbinomalt(10, "foo"), "Argument mu must be numeric.")
  expect_error(rbinomalt(10, 0), "Argument mu must be greater than zero.")
  expect_error(rbinomalt(10, 1), "Argument mu must be less than one.")
})

test_that("size input checking works", {
  expect_error(rbinomalt(10, .5, c(1, 2)), "Argument size must have length one.")
  expect_error(rbinomalt(10, .5, "foo"), "Argument size must be numeric.")
  expect_error(rbinomalt(10, .5, 0), "Argument size must be greater than 0.")
  expect_error(rbinomalt(10, .5, 10000), "Argument size must be less than 10,000.")
})
