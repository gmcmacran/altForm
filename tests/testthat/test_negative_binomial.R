
###############################################
# Density
###############################################
test_that("Check structure.", {
  expect_true(class(dnbinomalt) == "function")
  expect_true(all(names(formals(dnbinomalt)) == c("x", "mu", "size", "log")))
})

for (mu in seq(.01, .99, .05)) {
  for (n in c(1, 3, 5, 7, 9, 500, 9999)) {
    set.seed(1)
    x <- rnbinom(100, n, mu)

    d1 <- round(dnbinomalt(x, mu, n), 10)
    d2 <- round(dnbinom(x, n, mu), 10)

    d3 <- round(dnbinomalt(x, mu, n, TRUE), 10)
    d4 <- round(dnbinom(x, n, mu, log = TRUE), 10)

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
  expect_error(dnbinomalt(c()), "Argument x must have positive length.")
  expect_error(dnbinomalt(rep("foo", 50)), "Argument x must be numeric.")
  expect_error(dnbinomalt(rep(-10, 50)), "All elements in x must be greater than or equal to 0")
})

set.seed(1)
x <- rnbinom(100, 1, .5)
test_that("mu input checking works", {
  expect_error(dnbinomalt(x, c(1, 2)), "Argument mu must have length one.")
  expect_error(dnbinomalt(x, "foo"), "Argument mu must be numeric.")
  expect_error(dnbinomalt(x, 0), "Argument mu must be greater than zero.")
  expect_error(dnbinomalt(x, 1), "Argument mu must be less than one.")
})

test_that("size input checking works", {
  expect_error(dnbinomalt(x, .5, c(10, 11)), "Argument size must have length one.")
  expect_error(dnbinomalt(x, .5, "foo"), "Argument size must be numeric.")
  expect_error(dnbinomalt(x, .5, -1), "Argument size must be greater than 0.")
  expect_error(dnbinomalt(x, .5, 10000), "Argument size must be less than 10,000.")
})

test_that("log input checking works", {
  expect_error(dnbinomalt(x, .5, 10, c(TRUE, FALSE)), "Argument log must have length one.")
  expect_error(dnbinomalt(x, .5, 10, "foo"), "Argument log must be logical.")
})

###############################################
# cdf
###############################################
test_that("Check structure.", {
  expect_true(class(pnbinomalt) == "function")
  expect_true(all(names(formals(pnbinomalt)) == c("q", "mu", "size", "lower.tail", "log.p")))
})

for (mu in seq(.01, .99, .05)) {
  for (n in c(1, 3, 5, 7, 9, 500, 9999)) {
    set.seed(1)
    q <- rnbinom(100, n, mu)

    d1 <- round(pnbinomalt(q, mu, n), 10)
    d2 <- round(pnbinom(q = q, size = n, prob = mu), 10)

    d3 <- round(pnbinomalt(q, mu, n, TRUE, TRUE), 10)
    d4 <- round(pnbinom(q = q, size = n, prob = mu, log.p = TRUE), 10)

    test_that("Test results of cdf", {
      expect_equal(d1, d2)
      expect_equal(d3, d4)
    })
  }
}

# Total area is 1.
for (mu in seq(.01, .99, .05)) {
  for (n in c(1, 3, 5, 7, 9, 500, 9999)) {
    d1 <- pnbinomalt(Inf, mu, n)

    d3 <- pnbinomalt(Inf, mu, n, TRUE, TRUE)

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
  expect_error(pnbinomalt(c()), "Argument q must have positive length.")
  expect_error(pnbinomalt(rep("foo", 50)), "Argument q must be numeric.")
  expect_error(pnbinomalt(rep(-10, 50)), "All elements in q must be greater than or equal to 0")
})

set.seed(1)
q <- rnbinom(100, 10, .5)
test_that("mu input checking works", {
  expect_error(pnbinomalt(q, c(1, 2)), "Argument mu must have length one.")
  expect_error(pnbinomalt(q, "foo"), "Argument mu must be numeric.")
})

test_that("size input checking works", {
  expect_error(pnbinomalt(q, .5, c(10, 20)), "Argument size must have length one.")
  expect_error(pnbinomalt(q, .5, "foo"), "Argument size must be numeric.")
  expect_error(pnbinomalt(q, .5, 0), "Argument size must be greater than 0.")
  expect_error(pnbinomalt(q, .5, 10000), "Argument size must be less than 10,000.")
})

test_that("lower.tail input checking works", {
  expect_error(pnbinomalt(q, .5, 10, c(TRUE, FALSE)), "Argument lower.tail must have length one.")
  expect_error(pnbinomalt(q, .5, 10, "foo"), "Argument lower.tail must be logical.")
})

test_that("log.p input checking works", {
  expect_error(pnbinomalt(q, .5, 10, TRUE, c(TRUE, FALSE)), "Argument log.p must have length one.")
  expect_error(pnbinomalt(q, .5, 10, TRUE, "foo"), "Argument log.p must be logical.")
})

###############################################
# random number generator
###############################################
test_that("Check structure.", {
  expect_true(class(rnbinomalt) == "function")
  expect_true(all(names(formals(rnbinomalt)) == c("n", "mu", "size")))
})

for (mu in seq(.05, .95, .05)) {
  for (size in c(2, 5, 10)) {
    set.seed(1)
    x <- rnbinomalt(50000, mu, size)

    xbar <- mean(x)
    ref <- size * (1 - mu) / mu

    test_that("Test results of random generator", {
      expect_equal(length(x), 50000)
      expect_true(abs(ref - xbar) <= .2)
    })
  }
}

###############################################
# random number generator Input checking
###############################################
test_that("x input checking works", {
  expect_error(rnbinomalt(c()), "Argument n must have length one.")
  expect_error(rnbinomalt(c(5, 10)), "Argument n must have length one.")
  expect_error(rnbinomalt("foo"), "Argument n must be numeric.")
  expect_error(rnbinomalt(-10), "Argument n must be positive.")
})

test_that("mu input checking works", {
  expect_error(rnbinomalt(10, c(1, 2)), "Argument mu must have length one.")
  expect_error(rnbinomalt(10, "foo"), "Argument mu must be numeric.")
  expect_error(rnbinomalt(10, 0), "Argument mu must be greater than zero.")
  expect_error(rnbinomalt(10, 1), "Argument mu must be less than one.")
})

test_that("size input checking works", {
  expect_error(rnbinomalt(10, .5, c(1, 2)), "Argument size must have length one.")
  expect_error(rnbinomalt(10, .5, "foo"), "Argument size must be numeric.")
  expect_error(rnbinomalt(10, .5, 0), "Argument size must be greater than 0.")
  expect_error(rnbinomalt(10, .5, 10000), "Argument size must be less than 10,000.")
})
