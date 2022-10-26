###############################################
# Test Results
###############################################
# noraml's arguments
pdf <- dnormalt
f <- altForm:::create_cdf_exponential_form(pdf, -Inf, Inf, -Inf, Inf)

test_that("Check structure.", {
  expect_true(class(f) == "function")
  expect_true(all(names(formals(f)) == c("q", "mu", "sigma", "lower.tail", "log.p")))
})
rm(f)

###############################################
# Input checking
###############################################
pdf_helper_one <- function(typo, mu, sigma, log) {}
pdf_helper_two <- function(x, typo, sigma, log) {}
pdf_helper_three <- function(x, mu, typo, log) {}
pdf_helper_four <- function(x, mu, sigma, typo) {}
pdf_helper_five <- function(x, mu, sigma, log, extra) {}
test_that("a input checking works", {
  expect_error(altForm:::create_cdf_exponential_form(1), "Argument pdf must be a function.")
  expect_error(altForm:::create_cdf_exponential_form(pdf_helper_one), "pdf's first argument is not x.")
  expect_error(altForm:::create_cdf_exponential_form(pdf_helper_two), "pdf's second argument is not mu.")
  expect_error(altForm:::create_cdf_exponential_form(pdf_helper_three), "pdf's third argument is not sigma.")
  expect_error(altForm:::create_cdf_exponential_form(pdf_helper_four), "pdf's fourth argument is not log.")
  expect_error(altForm:::create_cdf_exponential_form(pdf_helper_five), "pdf has too many arguments.")
})
rm(pdf_helper_one, pdf_helper_two, pdf_helper_three, pdf_helper_four, pdf_helper_five)

test_that("XLB input checking works", {
  expect_error(altForm:::create_cdf_exponential_form(pdf, "foo"), "Argument XLB should be numeric.")
  expect_error(altForm:::create_cdf_exponential_form(pdf, c(1, 2)), "Argument XLB should have length one.")
})

test_that("XUB input checking works", {
  expect_error(altForm:::create_cdf_exponential_form(pdf, -Inf, "foo"), "Argument XUB should be numeric.")
  expect_error(altForm:::create_cdf_exponential_form(pdf, -Inf, c(1, 2)), "Argument XUB should have length one.")
  expect_error(altForm:::create_cdf_exponential_form(pdf, 1, 1), "Argument XLB should be less than XUB.")
})

test_that("MLB input checking works", {
  expect_error(altForm:::create_cdf_exponential_form(pdf, -Inf, Inf, "foo"), "Argument MLB should be numeric.")
  expect_error(altForm:::create_cdf_exponential_form(pdf, -Inf, Inf, c(1, 2)), "Argument MLB should have length one.")
})

###############################################
# Test Results
###############################################
# binomial's arguments
pmf <- dbinomalt
f <- altForm:::create_cmf_exponential_form(pmf, 0, Inf, 0, 1)

test_that("Check structure.", {
  expect_true(class(f) == "function")
  expect_true(all(names(formals(f)) == c("q", "mu", "size", "lower.tail", "log.p")))
})
rm(f)

# poisson's arguments
pmf <- dpoisalt
f <- altForm:::create_cmf_exponential_form(pmf, 0, Inf, 0, 143)

test_that("Check structure.", {
  expect_true(class(f) == "function")
  expect_true(all(names(formals(f)) == c("q", "mu", "lower.tail", "log.p")))
})
rm(f)

###############################################
# Input checking
###############################################
# based on binomial
pmf_helper_one <- function(typo, mu, size, log) {}
pmf_helper_two <- function(x, typo, size, log) {}
pmf_helper_three <- function(x, mu, typo, log) {}
pmf_helper_four <- function(x, mu, size, typo) {}
pmf_helper_five <- function(x, mu, size, log, extra) {}
test_that("pmf input checking works", {
  expect_error(altForm:::create_cmf_exponential_form(1), "Argument pmf must be a function.")
  expect_error(altForm:::create_cmf_exponential_form(pmf_helper_one), "pmf's first argument is not x.")
  expect_error(altForm:::create_cmf_exponential_form(pmf_helper_two), "pmf's second argument is not mu.")
  expect_error(altForm:::create_cmf_exponential_form(pmf_helper_three), "pmf's third argument is not size")
  expect_error(altForm:::create_cmf_exponential_form(pmf_helper_four), "pmf's fourth argument is not log.")
  expect_error(altForm:::create_cmf_exponential_form(pmf_helper_five), "pmf has too many arguments.")
})
rm(pmf_helper_one, pmf_helper_two, pmf_helper_three, pmf_helper_four, pmf_helper_five)

# based on poisson
pmf_helper_one <- function(typo, mu, log) {}
pmf_helper_two <- function(x, typo, log) {}
pmf_helper_three <- function(x, mu, typo) {}
pmf_helper_four <- function(x, mu, log, extra) {}
test_that("pmf input checking works", {
  expect_error(altForm:::create_cmf_exponential_form(1), "Argument pmf must be a function.")
  expect_error(altForm:::create_cmf_exponential_form(pmf_helper_one), "pmf's first argument is not x.")
  expect_error(altForm:::create_cmf_exponential_form(pmf_helper_two), "pmf's second argument is not mu.")
  expect_error(altForm:::create_cmf_exponential_form(pmf_helper_three), "pmf's third argument is not log.")
  expect_error(altForm:::create_cmf_exponential_form(pmf_helper_four), "pmf's third argument is not size.")
})
rm(pmf_helper_one, pmf_helper_two, pmf_helper_three, pmf_helper_four)

test_that("XLB input checking works", {
  expect_error(altForm:::create_cmf_exponential_form(pmf, "foo"), "Argument XLB should be numeric.")
  expect_error(altForm:::create_cmf_exponential_form(pmf, c(1, 2)), "Argument XLB should have length one.")
})

test_that("XUB input checking works", {
  expect_error(altForm:::create_cmf_exponential_form(pmf, -Inf, "foo"), "Argument XUB should be numeric.")
  expect_error(altForm:::create_cmf_exponential_form(pmf, -Inf, c(1, 2)), "Argument XUB should have length one.")
  expect_error(altForm:::create_cmf_exponential_form(pmf, 1, 1), "Argument XLB should be less than XUB.")
})

test_that("MLB input checking works", {
  expect_error(altForm:::create_cmf_exponential_form(pmf, -Inf, Inf, "foo"), "Argument MLB should be numeric.")
  expect_error(altForm:::create_cmf_exponential_form(pmf, -Inf, Inf, c(1, 2)), "Argument MLB should have length one.")
})

test_that("MUB input checking works", {
  expect_error(altForm:::create_cmf_exponential_form(pmf, -Inf, Inf, Inf, "foo"), "Argument MUB should be numeric.")
  expect_error(altForm:::create_cmf_exponential_form(pmf, -Inf, Inf, Inf, c(1, 2)), "Argument MUB should have length one.")
  expect_error(altForm:::create_cmf_exponential_form(pmf, -Inf, Inf, 1, 1), "Argument MLB should be less than MUB")
})
