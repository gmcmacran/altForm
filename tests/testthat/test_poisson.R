
###############################################
# Density
###############################################
for (mu in seq(.5, 3, .5)) {
  set.seed(1)
  x <- rpois(100, mu)

  d1 <- round(dpoisalt(x, mu), 10)
  d2 <- round(dpois(x, mu), 10)

  d3 <- round(dpoisalt(x, mu, TRUE), 10)
  d4 <- round(dpois(x, mu, TRUE), 10)

  test_that("Test results of density", {
    expect_true(all(d1 == d2))
    expect_true(all(d3 == d4))
  })
}

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
  expect_error(dpoisalt(x, -10), "Argument mu must be greater than or equal to 0")
})

test_that("log input checking works", {
  expect_error(dpoisalt(x, 1, c(TRUE, FALSE)), "Argument log must have length one.")
  expect_error(dpoisalt(x, 1, "foo"), "Argument log must be logical.")
})
