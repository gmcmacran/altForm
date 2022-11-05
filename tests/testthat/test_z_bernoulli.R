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

###############################################
# cdf
###############################################
test_that("Check structure.", {
  expect_true(class(pbernalt) == "function")
  expect_true(all(names(formals(pbernalt)) == c("q", "mu", "lower.tail", "log.p")))
})

for (mu in seq(.01, .99, .05)) {
  set.seed(1)
  q <- Rlab::rbern(100, mu)

  d1 <- round(pbernalt(q, mu), 10)
  d2 <- round(Rlab::pbern(q, mu), 10)

  d3 <- round(pbernalt(q, mu, TRUE, TRUE), 10)
  d4 <- round(Rlab::pbern(q, mu, TRUE, TRUE), 10)

  test_that("Test results of cdf", {
    expect_equal(d1, d2)
    expect_equal(d3, d4)
  })
}

# Total area is 1.
for (mu in seq(.01, .99, .05)) {
  for (n in c(1, 3, 5, 7, 9, 500, 9999)) {
    d1 <- pbernalt(1, mu)

    d3 <- pbernalt(1, mu, TRUE, TRUE)

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
  expect_error(pbernalt(c()), "Argument q must have positive length.")
  expect_error(pbernalt(rep("foo", 50)), "Argument q must be numeric.")
  expect_error(pbernalt(rep(-10, 50)), "All elements in q must be greater than or equal to 0")
})

set.seed(1)
q <- Rlab::rbern(100, .5)
test_that("mu input checking works", {
  expect_error(pbernalt(q, c(1, 2)), "Argument mu must have length one.")
  expect_error(pbernalt(q, "foo"), "Argument mu must be numeric.")
})

test_that("lower.tail input checking works", {
  expect_error(pbernalt(q, .5, c(TRUE, FALSE)), "Argument lower.tail must have length one.")
  expect_error(pbernalt(q, .5, "foo"), "Argument lower.tail must be logical.")
})

test_that("log.p input checking works", {
  expect_error(pbernalt(q, .5, TRUE, c(TRUE, FALSE)), "Argument log.p must have length one.")
  expect_error(pbernalt(q, .5, TRUE, "foo"), "Argument log.p must be logical.")
})

###############################################
# random number generator
###############################################
test_that("Check structure.", {
  expect_true(class(rbernalt) == "function")
  expect_true(all(names(formals(rbernalt)) == c("n", "mu")))
})

for (mu in seq(.01, .99, .05)) {
  set.seed(1)
  x <- rbernalt(50000, mu)

  xbar <- mean(x)

  test_that("Test results of random generator", {
    expect_equal(length(x), 50000)
    expect_true(abs(mu - xbar) <= .1)
  })
}

###############################################
# random number generator Input checking
###############################################
test_that("x input checking works", {
  expect_error(rbernalt(c()), "Argument n must have length one.")
  expect_error(rbernalt(c(5, 10)), "Argument n must have length one.")
  expect_error(rbernalt("foo"), "Argument n must be numeric.")
  expect_error(rbernalt(-10), "Argument n must be positive.")
})

test_that("mu input checking works", {
  expect_error(rbernalt(10, c(1, 2)), "Argument mu must have length one.")
  expect_error(rbernalt(10, "foo"), "Argument mu must be numeric.")
  expect_error(rbernalt(10, 0), "Argument mu must be greater than zero.")
  expect_error(rbernalt(10, 1), "Argument mu must be less than one.")
})
