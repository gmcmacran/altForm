###################################################################################################
# pdf/pmf
###################################################################################################

###############################################
# Test Results
###############################################
# noraml's arguments
a <- function(phi) {
  return(phi)
}

b <- function(theta) {
  return(theta^2 / 2)
}

c2 <- function(x, phi) {
  out <- -(1 / 2) * (x^2 / phi + log(2 * pi * phi))
  return(out)
}

link <- function(mu) {
  return(mu)
}

calc_phi <- function(sigma, mu) {
  return(sigma^2)
}

f <- altForm:::create_pdf_exponential_form(a, b, c2, link, calc_phi, TRUE, -Inf, Inf, -Inf, Inf)
test_that("Check structure.", {
  expect_true(class(f) == "function")
  expect_true(all(names(formals(f)) == c("x", "mu", "sigma", "log")))
})
rm(f)

f <- altForm:::create_pdf_exponential_form(a, b, c2, link, calc_phi, FALSE, -Inf, Inf, -Inf, Inf)
test_that("Check structure.", {
  expect_true(class(f) == "function")
  expect_true(all(names(formals(f)) == c("x", "mu", "log")))
})
rm(f)

###############################################
# Input checking
###############################################
a_helper_one <- function(typo) {}
a_helper_two <- function(phi, extra) {}
test_that("a input checking works", {
  expect_error(altForm:::create_pdf_exponential_form(1), "Argument a must be a function.")
  expect_error(altForm:::create_pdf_exponential_form(a_helper_one), "a's first argument is not phi.")
  expect_error(altForm:::create_pdf_exponential_form(a_helper_two), "a has too many arguments.")
})
rm(a_helper_one, a_helper_two)

b_helper_one <- function(typo) {}
b_helper_two <- function(theta, extra) {}
test_that("b input checking works", {
  expect_error(altForm:::create_pdf_exponential_form(a, 1), "Argument b must be a function.")
  expect_error(altForm:::create_pdf_exponential_form(a, b_helper_one), "b's first argument is not theta.")
  expect_error(altForm:::create_pdf_exponential_form(a, b_helper_two), "b has too many arguments.")
})
rm(b_helper_one, b_helper_two)

c2_helper_one <- function(typo, phi) {}
c2_helper_two <- function(x, typo) {}
c2_helper_three <- function(x, phi, extra) {}
test_that("c2 input checking works", {
  expect_error(altForm:::create_pdf_exponential_form(a, b, 1), "Argument c2 must be a function.")
  expect_error(altForm:::create_pdf_exponential_form(a, b, c2_helper_one), "c2's first argument is not x.")
  expect_error(altForm:::create_pdf_exponential_form(a, b, c2_helper_two), "c2's first argument is not phi.")
  expect_error(altForm:::create_pdf_exponential_form(a, b, c2_helper_three), "c2 has too many arguments.")
})
rm(c2_helper_one, c2_helper_two, c2_helper_three)

link_helper_one <- function(typo) {}
link_helper_two <- function(mu, extra) {}
test_that("link input checking works", {
  expect_error(altForm:::create_pdf_exponential_form(a, b, c2, 1), "Argument link must be a function.")
  expect_error(altForm:::create_pdf_exponential_form(a, b, c2, link_helper_one), "link's first argument is not mu.")
  expect_error(altForm:::create_pdf_exponential_form(a, b, c2, link_helper_two), "link has too many arguments.")
})
rm(link_helper_one, link_helper_two)

calc_phi_helper_one <- function(sigma, mu, extra) {}
test_that("calc_phi input checking works", {
  expect_error(altForm:::create_pdf_exponential_form(a, b, c2, link, 1), "Argument calc_phi must be a function.")
  expect_error(altForm:::create_pdf_exponential_form(a, b, c2, link, calc_phi_helper_one), "calc_phi has too many arguments.")
})
rm(calc_phi_helper_one)

test_that("hasNuisance input checking works", {
  expect_error(altForm:::create_pdf_exponential_form(a, b, c2, link, calc_phi, 1), "Argument hasNuisance must be logical.")
  expect_error(altForm:::create_pdf_exponential_form(a, b, c2, link, calc_phi, c(TRUE, FALSE)), "Argument hasNuisance must have length one.")
})

test_that("XLB input checking works", {
  expect_error(altForm:::create_pdf_exponential_form(a, b, c2, link, calc_phi, TRUE, "foo"), "Argument XLB should be numeric.")
  expect_error(altForm:::create_pdf_exponential_form(a, b, c2, link, calc_phi, TRUE, c(1, 2)), "Argument XLB should have length one.")
})

test_that("XUB input checking works", {
  expect_error(altForm:::create_pdf_exponential_form(a, b, c2, link, calc_phi, TRUE, -Inf, "foo"), "Argument XUB should be numeric.")
  expect_error(altForm:::create_pdf_exponential_form(a, b, c2, link, calc_phi, TRUE, -Inf, c(1, 2)), "Argument XUB should have length one.")
  expect_error(altForm:::create_pdf_exponential_form(a, b, c2, link, calc_phi, TRUE, 1, 1), "Argument XLB should be less than XUB.")
})

test_that("MLB input checking works", {
  expect_error(altForm:::create_pdf_exponential_form(a, b, c2, link, calc_phi, TRUE, -Inf, Inf, "foo"), "Argument MLB should be numeric.")
  expect_error(altForm:::create_pdf_exponential_form(a, b, c2, link, calc_phi, TRUE, -Inf, Inf, c(1, 2)), "Argument MLB should have length one.")
})

test_that("MUB input checking works", {
  expect_error(altForm:::create_pdf_exponential_form(a, b, c2, link, calc_phi, TRUE, -Inf, Inf, Inf, "foo"), "Argument MUB should be numeric.")
  expect_error(altForm:::create_pdf_exponential_form(a, b, c2, link, calc_phi, TRUE, -Inf, Inf, Inf, c(1, 2)), "Argument MUB should have length one.")
  expect_error(altForm:::create_pdf_exponential_form(a, b, c2, link, calc_phi, TRUE, -Inf, Inf, 1, 1), "Argument MLB should be less than MUB")
})

###############################################
# Test Results
###############################################
# binomials's arguments
a <- function(phi) {
  return(phi)
}

#' @keywords internal
b <- function(theta, size) {
  return(size * log(1 + exp(theta)))
}

#' @keywords internal
c2 <- function(x, size) {
  out <- lchoose(size, x)
  return(out)
}

#' @keywords internal
link <- function(mu) {
  return(log(mu / (1 - mu)))
}

#' @keywords internal
calc_phi <- function() {
  return(1)
}

f <- altForm:::create_pmf_exponential_form(a, b, c2, link, calc_phi, 0)
test_that("Check structure.", {
  expect_true(class(f) == "function")
  expect_true(all(names(formals(f)) == c("x", "mu", "size", "log")))
})
rm(f)

###############################################
# Input checking
###############################################
a_helper_one <- function(typo) {}
a_helper_two <- function(phi, extra) {}
test_that("a input checking works", {
  expect_error(altForm:::create_pmf_exponential_form(1), "Argument a must be a function.")
  expect_error(altForm:::create_pmf_exponential_form(a_helper_one), "a's first argument is not phi.")
  expect_error(altForm:::create_pmf_exponential_form(a_helper_two), "a has too many arguments.")
})
rm(a_helper_one, a_helper_two)

b_helper_one <- function(typo, size) {}
b_helper_two <- function(theta, typo) {}
b_helper_three <- function(theta, size, extra) {}
test_that("b input checking works", {
  expect_error(altForm:::create_pmf_exponential_form(a, 1), "Argument b must be a function.")
  expect_error(altForm:::create_pmf_exponential_form(a, b_helper_one), "b's first argument is not theta.")
  expect_error(altForm:::create_pmf_exponential_form(a, b_helper_two), "b's second argument is not size.")
  expect_error(altForm:::create_pmf_exponential_form(a, b_helper_three), "b has too many arguments.")
})
rm(b_helper_one, b_helper_two, b_helper_three)

c2_helper_one <- function(typo, size) {}
c2_helper_two <- function(x, typo) {}
c2_helper_three <- function(x, size, extra) {}
test_that("c2 input checking works", {
  expect_error(altForm:::create_pmf_exponential_form(a, b, 1), "Argument c2 must be a function.")
  expect_error(altForm:::create_pmf_exponential_form(a, b, c2_helper_one), "c2's first argument is not x.")
  expect_error(altForm:::create_pmf_exponential_form(a, b, c2_helper_two), "c2's first argument is not size.")
  expect_error(altForm:::create_pmf_exponential_form(a, b, c2_helper_three), "c2 has too many arguments.")
})
rm(c2_helper_one, c2_helper_two, c2_helper_three)

link_helper_one <- function(typo) {}
link_helper_two <- function(mu, extra) {}
test_that("link input checking works", {
  expect_error(altForm:::create_pmf_exponential_form(a, b, c2, 1), "Argument link must be a function.")
  expect_error(altForm:::create_pmf_exponential_form(a, b, c2, link_helper_one), "link's first argument is not mu.")
  expect_error(altForm:::create_pmf_exponential_form(a, b, c2, link_helper_two), "link has too many arguments.")
})
rm(link_helper_one, link_helper_two)

calc_phi_helper_one <- function(sigma, mu, extra) {}
test_that("calc_phi input checking works", {
  expect_error(altForm:::create_pmf_exponential_form(a, b, c2, link, 1), "Argument calc_phi must be a function.")
  expect_error(altForm:::create_pmf_exponential_form(a, b, c2, link, calc_phi_helper_one), "calc_phi has too many arguments.")
})
rm(calc_phi_helper_one)

test_that("XLB input checking works", {
  expect_error(altForm:::create_pmf_exponential_form(a, b, c2, link, calc_phi, "foo"), "Argument XLB should be numeric.")
  expect_error(altForm:::create_pmf_exponential_form(a, b, c2, link, calc_phi, c(1, 2)), "Argument XLB should have length one.")
})

###################################################################################################
# cdf/cmf
###################################################################################################
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

test_that("MUB input checking works", {
  expect_error(altForm:::create_cdf_exponential_form(pdf, -Inf, Inf, Inf, "foo"), "Argument MUB should be numeric.")
  expect_error(altForm:::create_cdf_exponential_form(pdf, -Inf, Inf, Inf, c(1, 2)), "Argument MUB should have length one.")
  expect_error(altForm:::create_cdf_exponential_form(pdf, -Inf, Inf, 1, 1), "Argument MLB should be less than MUB")
})
