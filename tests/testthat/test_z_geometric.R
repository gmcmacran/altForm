###############################################
# Density
###############################################
test_that("Check structure.", {
  expect_true(class(dgeomalt) == "function")
  expect_true(all(names(formals(dgeomalt)) == c("x", "mu", "log")))
})

for (mu in seq(.01, .99, .05)) {
  set.seed(1)
  x <- rgeom(100, mu)

  d1 <- round(dgeomalt(x, mu), 10)
  d2 <- round(dgeom(x, mu), 10)

  d3 <- round(dgeomalt(x, mu, TRUE), 10)
  d4 <- round(dgeom(x, mu, log = TRUE), 10)

  test_that("Test results of density", {
    expect_equal(d1, d2)
    expect_equal(d3, d4)
  })
}

###############################################
# Density Input checking
###############################################
test_that("x input checking works", {
  expect_error(dgeomalt(c()), "Argument x must have positive length.")
  expect_error(dgeomalt(rep("foo", 50)), "Argument x must be numeric.")
  expect_error(dgeomalt(rep(-10, 50)), "All elements in x must be greater than or equal to 0")
})

set.seed(1)
x <- rgeom(100, .5)
test_that("mu input checking works", {
  expect_error(dgeomalt(x, c(1, 2)), "Argument mu must have length one.")
  expect_error(dgeomalt(x, "foo"), "Argument mu must be numeric.")
  expect_error(dgeomalt(x, 0), "Argument mu must be greater than zero.")
  expect_error(dgeomalt(x, 1), "Argument mu must be less than one.")
})

test_that("log input checking works", {
  expect_error(dgeomalt(x, .5, c(TRUE, FALSE)), "Argument log must have length one.")
  expect_error(dgeomalt(x, .5, "foo"), "Argument log must be logical.")
})

###############################################
# cdf
###############################################
test_that("Check structure.", {
  expect_true(class(pgeomaltalt) == "function")
  expect_true(all(names(formals(pgeomaltalt)) == c("q", "mu", "lower.tail", "log.p")))
})

for (mu in seq(.01, .99, .05)) {
  set.seed(1)
  q <- rgeom(100, mu)

  d1 <- round(pgeomaltalt(q, mu), 10)
  d2 <- round(pgeom(q = q, prob = mu), 10)

  d3 <- round(pgeomaltalt(q, mu, TRUE, TRUE), 10)
  d4 <- round(pgeom(q = q, prob = mu, log.p = TRUE), 10)

  test_that("Test results of cdf", {
    expect_equal(d1, d2)
    expect_equal(d3, d4)
  })
}

# Total area is 1.
for (mu in seq(.01, .99, .05)) {
  d1 <- pgeomaltalt(Inf, mu)

  d3 <- pgeomaltalt(Inf, mu, TRUE, TRUE)

  test_that("Test results of cdf", {
    expect_equal(d1, 1)
    expect_equal(d3, 0)
  })
}

###############################################
# cdf Input checking
###############################################
test_that("x input checking works", {
  expect_error(pgeomaltalt(c()), "Argument q must have positive length.")
  expect_error(pgeomaltalt(rep("foo", 50)), "Argument q must be numeric.")
  expect_error(pgeomaltalt(rep(-10, 50)), "All elements in q must be greater than or equal to 0")
})

set.seed(1)
q <- rgeom(100, .5)
test_that("mu input checking works", {
  expect_error(pgeomaltalt(q, c(1, 2)), "Argument mu must have length one.")
  expect_error(pgeomaltalt(q, "foo"), "Argument mu must be numeric.")
})

test_that("lower.tail input checking works", {
  expect_error(pgeomaltalt(q, .5, c(TRUE, FALSE)), "Argument lower.tail must have length one.")
  expect_error(pgeomaltalt(q, .5, "foo"), "Argument lower.tail must be logical.")
})

test_that("log.p input checking works", {
  expect_error(pgeomaltalt(q, .5, TRUE, c(TRUE, FALSE)), "Argument log.p must have length one.")
  expect_error(pgeomaltalt(q, .5, TRUE, "foo"), "Argument log.p must be logical.")
})
