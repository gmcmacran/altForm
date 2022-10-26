# fix check.
# Not actually global.
utils::globalVariables(c("x", "mu", "sigma", "log", "size"))

#' @keywords internal
#' A function factory that returns pdfs
create_pdf_exponential_form <- function(a, b, c2, link, calc_phi, hasNuisance, XLB, XUB, MLB, MUB) {
  force(a)
  if (!inherits(a, "function")) {
    stop("Argument a must be a function.")
  }
  args <- names(formals(a))
  if (args[1] != "phi") {
    stop("a's first argument is not phi.")
  }
  if (length(args) != 1) {
    stop("a has too many arguments.")
  }
  rm(args)

  force(b)
  if (!inherits(b, "function")) {
    stop("Argument b must be a function.")
  }
  args <- names(formals(b))
  if (args[1] != "theta") {
    stop("b's first argument is not theta.")
  }
  if (length(args) != 1) {
    stop("b has too many arguments.")
  }
  rm(args)

  force(c2)
  if (!inherits(c2, "function")) {
    stop("Argument c2 must be a function.")
  }
  args <- names(formals(c2))
  if (args[1] != "x") {
    stop("c2's first argument is not x.")
  }
  if (args[2] != "phi") {
    stop("c2's first argument is not phi.")
  }
  if (length(args) != 2) {
    stop("c2 has too many arguments.")
  }
  rm(args)

  force(link)
  if (!inherits(link, "function")) {
    stop("Argument link must be a function.")
  }
  args <- names(formals(link))
  if (args[1] != "mu") {
    stop("link's first argument is not mu.")
  }
  if (length(args) != 1) {
    stop("link has too many arguments.")
  }
  rm(args)

  force(calc_phi)
  if (!inherits(calc_phi, "function")) {
    stop("Argument calc_phi must be a function.")
  }
  args <- names(formals(calc_phi))
  if (length(args) > 2) {
    stop("calc_phi has too many arguments.")
  }
  rm(args)

  if (!is.logical(hasNuisance)) {
    stop("Argument hasNuisance must be logical.")
  }
  if (length(hasNuisance) != 1) {
    stop("Argument hasNuisance must have length one.")
  }

  force(XLB)
  if (!is.numeric(XLB)) {
    stop("Argument XLB should be numeric.")
  }
  if (length(XLB) != 1) {
    stop("Argument XLB should have length one.")
  }

  force(XUB)
  if (!is.numeric(XUB)) {
    stop("Argument XUB should be numeric.")
  }
  if (length(XUB) != 1) {
    stop("Argument XUB should have length one.")
  }
  if (XLB >= XUB) {
    stop("Argument XLB should be less than XUB.")
  }

  force(MLB)
  if (!is.numeric(MLB)) {
    stop("Argument MLB should be numeric.")
  }
  if (length(MLB) != 1) {
    stop("Argument MLB should have length one.")
  }

  force(MUB)
  if (!is.numeric(MUB)) {
    stop("Argument MUB should be numeric.")
  }
  if (length(MUB) != 1) {
    stop("Argument MUB should have length one.")
  }
  if (MLB >= MUB) {
    stop("Argument MLB should be less than MUB")
  }

  if (hasNuisance) {
    args <- rlang::pairlist2(x = , mu = , sigma = , log = FALSE)

    sdCheck <- rlang::expr({
      if (length(sigma) != 1) {
        stop("Argument sigma must have length one.")
      }
      if (!is.numeric(sigma)) {
        stop("Argument sigma must be numeric.")
      }
      if (sigma <= 0) {
        stop("sigma must be above 0.")
      }
    })

    phi <- rlang::expr(phi <- calc_phi(sigma, mu))
  } else {
    args <- rlang::pairlist2(x = , mu = , log = FALSE)

    sdCheck <- rlang::expr({})

    phi <- rlang::expr(phi <- calc_phi())
  }

  body <- rlang::expr({
    # check inputs
    if (length(x) < 1) {
      stop("Argument x must have positive length.")
    }
    if (!is.numeric(x)) {
      stop("Argument x must be numeric.")
    }
    if (any(x < XLB)) {
      stop(paste("All elements in x must be greater than or equal to ", XLB, sep = ""))
    }
    if (any(x > XUB)) {
      stop(paste("All elements in x must be less than or equal to ", XUB, sep = ""))
    }

    if (length(mu) != 1) {
      stop("Argument mu must have length one.")
    }
    if (!is.numeric(mu)) {
      stop("Argument mu must be numeric.")
    }
    if (mu <= MLB) {
      stop(paste("Argument mu must be greater than ", MLB, sep = ""))
    }
    if (mu >= MUB) {
      stop(paste("Argument mu must be less than ", MUB, sep = ""))
    }

    !!sdCheck

    if (length(log) != 1) {
      stop("Argument log must have length one.")
    }
    if (!is.logical(log)) {
      stop("Argument log must be logical.")
    }

    theta <- link(mu)
    !!phi

    p <- (x * theta - b(theta)) / a(phi) + c2(x, phi)
    if (!log) {
      p <- exp(p)
    }

    return(p)
  })

  exec_globals <- list(a = a, b = b, c2 = c2, link = link, calc_phi = calc_phi, XLB = XLB, XUB = XUB, MLB = MLB, MUB = MUB)
  exec_env <- rlang::new_environment(data = exec_globals, parent = rlang::base_env())

  f <- rlang::new_function(args, body, env = exec_env)

  return(f)
}

#' @keywords internal
#' A function factory that returns exponential pmfs w/ size parameter (binomial and negative binomial)
create_pmf_exponential_form <- function(a, b, c2, link, calc_phi, XLB) {
  force(a)
  if (!inherits(a, "function")) {
    stop("Argument a must be a function.")
  }
  args <- names(formals(a))
  if (args[1] != "phi") {
    stop("a's first argument is not phi.")
  }
  if (length(args) != 1) {
    stop("a has too many arguments.")
  }
  rm(args)

  force(b)
  if (!inherits(b, "function")) {
    stop("Argument b must be a function.")
  }
  args <- names(formals(b))
  if (args[1] != "theta") {
    stop("b's first argument is not theta.")
  }
  if (args[2] != "size") {
    stop("b's second argument is not size.")
  }
  if (length(args) != 2) {
    stop("b has too many arguments.")
  }
  rm(args)

  force(c2)
  if (!inherits(c2, "function")) {
    stop("Argument c2 must be a function.")
  }
  args <- names(formals(c2))
  if (args[1] != "x") {
    stop("c2's first argument is not x.")
  }
  if (args[2] != "size") {
    stop("c2's first argument is not size.")
  }
  if (length(args) != 2) {
    stop("c2 has too many arguments.")
  }
  rm(args)

  force(link)
  if (!inherits(link, "function")) {
    stop("Argument link must be a function.")
  }
  args <- names(formals(link))
  if (args[1] != "mu") {
    stop("link's first argument is not mu.")
  }
  if (length(args) != 1) {
    stop("link has too many arguments.")
  }
  rm(args)

  force(calc_phi)
  if (!inherits(calc_phi, "function")) {
    stop("Argument calc_phi must be a function.")
  }
  args <- names(formals(calc_phi))
  if (length(args) > 2) {
    stop("calc_phi has too many arguments.")
  }
  rm(args)

  force(XLB)
  if (!is.numeric(XLB)) {
    stop("Argument XLB should be numeric.")
  }
  if (length(XLB) != 1) {
    stop("Argument XLB should have length one.")
  }

  args <- rlang::pairlist2(x = , mu = , size = , log = FALSE)

  body <- rlang::expr({
    # check inputs
    if (length(x) < 1) {
      stop("Argument x must have positive length.")
    }
    if (!is.numeric(x)) {
      stop("Argument x must be numeric.")
    }
    if (any(x < XLB)) {
      stop(paste("All elements in x must be greater than or equal to ", XLB, sep = ""))
    }

    if (length(mu) != 1) {
      stop("Argument mu must have length one.")
    }
    if (!is.numeric(mu)) {
      stop("Argument mu must be numeric.")
    }
    if (mu <= 0) {
      stop("Argument mu must be greater than zero.")
    }
    if (mu >= 1) {
      stop("Argument mu must be less than one.")
    }

    if (length(size) != 1) {
      stop("Argument size must have length one.")
    }
    if (!is.numeric(size)) {
      stop("Argument size must be numeric.")
    }
    if (size <= 0) {
      stop("Argument size must be greater than 0.")
    }
    if (size >= 10000) {
      stop("Argument size must be less than 10,000.")
    }

    if (length(log) != 1) {
      stop("Argument log must have length one.")
    }
    if (!is.logical(log)) {
      stop("Argument log must be logical.")
    }

    theta <- link(mu)
    phi <- calc_phi()

    p <- (x * theta - b(theta, size)) / a(phi) + c2(x, size)
    if (!log) {
      p <- exp(p)
    }

    return(p)
  })

  exec_globals <- list(a = a, b = b, c2 = c2, link = link, calc_phi = calc_phi, XLB = XLB)
  exec_env <- rlang::new_environment(data = exec_globals, parent = rlang::base_env())

  f <- rlang::new_function(args, body, env = exec_env)

  return(f)
}
