
###############################################
# Density
###############################################
test_that("Check structure.", {
  expect_true(class(dpoisalt) == "function")
  expect_true(all(names(formals(dpoisalt)) == c("x", "mu", "log")))
})

mus <- c(.0001, seq(.5, 3, .1), 100, 142)
for (mu in mus) {
  set.seed(1)
  x <- rpois(100, mu)

  d1 <- round(dpoisalt(x, mu), 10)
  d2 <- round(dpois(x, mu), 10)

  d3 <- round(dpoisalt(x, mu, TRUE), 10)
  d4 <- round(dpois(x, mu, TRUE), 10)

  test_that("Test results of density", {
    expect_equal(d1, d2)
    expect_equal(d3, d4)
  })
}
rm(mus)

###############################################
# Density Input checking
###############################################
test_that("x input checking works", {
  expect_error(dpoisalt(c()), "Argument x must have positive length.")
  expect_error(dpoisalt(rep("foo", 50)), "Argument x must be numeric.")
  expect_error(dpoisalt(rep(-10, 50)), "All elements in x must be greater than or equal to 0")
})

set.seed(1)
x <- rpois(100, .5)
test_that("mu input checking works", {
  expect_error(dpoisalt(x, c(1, 2)), "Argument mu must have length one.")
  expect_error(dpoisalt(x, "foo"), "Argument mu must be numeric.")
  expect_error(dpoisalt(x, -10), "Argument mu must be greater than 0")
  expect_error(dpoisalt(x, 143), "Argument mu must be less than 143")
})

test_that("log input checking works", {
  expect_error(dpoisalt(x, 1, c(TRUE, FALSE)), "Argument log must have length one.")
  expect_error(dpoisalt(x, 1, "foo"), "Argument log must be logical.")
})

###############################################
# cdf
###############################################
test_that("Check structure.", {
  expect_true(class(pnormalt) == "function")
  expect_true(all(names(formals(ppoisalt)) == c("q", "mu", "lower.tail", "log.p")))
})

mus <- c(.0001, seq(.5, 3, .1), 100, 142)
for (mu in mus) {
  set.seed(1)
  q <- rpois(100, mu)

  d1 <- round(ppoisalt(q, mu), 10)
  d2 <- round(ppois(q, mu), 10)

  d3 <- round(ppoisalt(q, mu, TRUE, TRUE), 10)
  d4 <- round(ppois(q, mu, TRUE, TRUE), 10)

  d5 <- round(ppoisalt(q, mu, FALSE), 10)
  d6 <- round(ppois(q, mu, FALSE), 10)

  test_that("Test results of cdf", {
    expect_equal(d1, d2)
    expect_equal(d3, d4)
    expect_equal(d5, d6)
  })
}

# Total area is 1.
for (mu in mus) {
  d1 <- ppoisalt(9999, mu)

  d3 <- ppoisalt(9999, mu, TRUE, TRUE)

  test_that("Test results of cdf", {
    expect_equal(d1, 1)
    expect_equal(d3, 0)
  })
}

###############################################
# cdf Input checking
###############################################
test_that("q input checking works", {
  expect_error(ppoisalt(c()), "Argument q must have positive length.")
  expect_error(ppoisalt(rep("foo", 50)), "Argument q must be numeric.")
  expect_error(ppoisalt(rep(-10, 50)), "All elements in q must be greater than or equal to 0")
})

set.seed(1)
q <- rpois(100, mu)
test_that("mu input checking works", {
  expect_error(ppoisalt(q, c(1, 2)), "Argument mu must have length one.")
  expect_error(ppoisalt(q, "foo"), "Argument mu must be numeric.")
  expect_error(ppoisalt(q, 0), "Argument mu must be greater than 0")
  expect_error(ppoisalt(q, 143), "Argument mu must be less than 143")
})

test_that("lower.tail input checking works", {
  expect_error(ppoisalt(q, 1, c(TRUE, FALSE)), "Argument lower.tail must have length one.")
  expect_error(ppoisalt(q, 1, "foo"), "Argument lower.tail must be logical.")
})

test_that("log.p input checking works", {
  expect_error(ppoisalt(q, 1, TRUE, c(TRUE, FALSE)), "Argument log.p must have length one.")
  expect_error(ppoisalt(q, 1, TRUE, "foo"), "Argument log.p must be logical.")
})

###############################################
# random number generator
###############################################
test_that("Check structure.", {
  expect_true(class(rpoisalt) == "function")
  expect_true(all(names(formals(rpoisalt)) == c("n", "mu")))
})

for (mu in seq(.25, 1.25, .25)) {
  set.seed(1)
  x <- rpoisalt(50000, mu)

  xbar <- mean(x)

  test_that("Test results of random generator", {
    expect_equal(length(x), 50000)
    expect_true(abs(mu - xbar) <= .1)
  })
}

###############################################
# random number generator Input checking
###############################################
test_that("n input checking works", {
  expect_error(rpoisalt(c()), "Argument n must have length one.")
  expect_error(rpoisalt(c(5, 10)), "Argument n must have length one.")
  expect_error(rpoisalt("foo"), "Argument n must be numeric.")
  expect_error(rpoisalt(0), "Argument n must be positive.")
})

test_that("mu input checking works", {
  expect_error(rpoisalt(10, c(1, 2)), "Argument mu must have length one.")
  expect_error(rpoisalt(10, "foo"), "Argument mu must be numeric.")
  expect_error(rpoisalt(10, 0), "Argument mu must be positive.")
  expect_error(rpoisalt(10, 143), "Argument mu must be less than 143.")
})
