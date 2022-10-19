# https://www.sfu.ca/sasdoc/sashtml/insight/chap39/sect4.htm

# fix check.
# Not actually global.
utils::globalVariables(c("x", "mu", "sigma", "log"))

#' @keywords internal
#' A function factory that returns pdfs
create_pdf_exponential_form <- function(a, b, c2, link, calc_phi, hasNuisance, LB) {
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
    stop("link has too many arguments.")
  }
  rm(args)

  if (!is.logical(hasNuisance)) {
    stop("Argument hasNuisance must be logical.")
  }
  if (length(hasNuisance) != 1) {
    stop("Argument hasNuisance must have length one.")
  }

  force(LB)
  if (!is.numeric(LB)) {
    stop("Argument LB should be numeric.")
  }
  if (length(LB) != 1) {
    stop("Argument LB should have length one.")
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
      if (sigma <= LB) {
        stop("sigma must be above 0")
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
    if (any(x < LB)) {
      stop(paste("All elements in x must be greater than or equal to ", LB, sep = ""))
    }

    if (length(mu) != 1) {
      stop("Argument mu must have length one.")
    }
    if (!is.numeric(mu)) {
      stop("Argument mu must be numeric.")
    }
    if (mu < LB) {
      stop(paste("Argument mu must be greater than or equal to ", LB, sep = ""))
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

  exec_globals <- list(a = a, b = b, c2 = c2, link = link, calc_phi = calc_phi, LB = LB)
  exec_env <- rlang::new_environment(data = exec_globals, parent = rlang::base_env())

  f <- rlang::new_function(args, body, env = exec_env)

  return(f)
}
