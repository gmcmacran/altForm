# fix check.
# Not actually global.
utils::globalVariables(c("x", "mu", "sigma", "log", "size", "pdf", "log.p", "lower.tail"))

#' @keywords internal
#' A function factory that returns cdfs
create_cdf_exponential_form <- function(pdf, XLB, XUB, MLB, MUB) {
  force(pdf)
  if (!inherits(pdf, "function")) {
    stop("Argument pdf must be a function.")
  }
  args <- names(formals(pdf))
  if (args[1] != "x") {
    stop("pdf's first argument is not x.")
  }
  if (args[2] != "mu") {
    stop("pdf's second argument is not mu.")
  }
  if (args[3] != "sigma") {
    stop("pdf's third argument is not sigma.")
  }
  if (args[4] != "log") {
    stop("pdf's fourth argument is not log.")
  }
  if (length(args) > 4) {
    stop("pdf has too many arguments.")
  }
  rm(args)

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

  args <- rlang::pairlist2(q = , mu = , sigma = , lower.tail = TRUE, log.p = FALSE)

  body <- rlang::expr({
    # check inputs
    if (length(q) < 1) {
      stop("Argument q must have positive length.")
    }
    if (!is.numeric(q)) {
      stop("Argument q must be numeric.")
    }
    if (any(q < XLB)) {
      stop(paste("All elements in q must be greater than or equal to ", XLB, sep = ""))
    }
    if (any(q > XUB)) {
      stop(paste("All elements in q must be less than or equal to ", XUB, sep = ""))
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

    if (length(sigma) != 1) {
      stop("Argument sigma must have length one.")
    }
    if (!is.numeric(sigma)) {
      stop("Argument sigma must be numeric.")
    }
    if (sigma <= 0) {
      stop("sigma must be above 0.")
    }

    if (length(lower.tail) != 1) {
      stop("Argument lower.tail must have length one.")
    }
    if (!is.logical(lower.tail)) {
      stop("Argument lower.tail must be logical.")
    }

    if (length(log.p) != 1) {
      stop("Argument log.p must have length one.")
    }
    if (!is.logical(log.p)) {
      stop("Argument log.p must be logical.")
    }

    helper <- function(q) {
      pdf(q, mu = mu, sigma = sigma, log = FALSE)
    }

    p <- vector(mode = "numeric", length = length(q))
    for (i in seq_along(p)) {
      # utils::capture.output(p[i] <- pracma::integral(fun = pdf, xmin = XLB, xmax = q[i], no_intervals = 16, mu = mu, sigma = sigma))
      p[i] <- pracma::quadgr(f = helper, a = XLB, b = q[i])$value
    }

    if (!lower.tail) {
      p <- 1 - p
    }

    if (log.p) {
      p <- log(p)
    }

    return(p)
  })

  exec_globals <- list(pdf = pdf, XLB = XLB, XUB = XUB, MLB = MLB, MUB = MUB)
  exec_env <- rlang::new_environment(data = exec_globals, parent = rlang::base_env())

  f <- rlang::new_function(args, body, env = exec_env)

  return(f)
}

#' @keywords internal
#' A function factory that returns cmfs
create_cmf_exponential_form <- function(pmf, XLB, XUB, MLB, MUB) {
  force(pmf)
  if (!inherits(pmf, "function")) {
    stop("Argument pmf must be a function.")
  }
  args <- names(formals(pmf))
  if (args[1] != "x") {
    stop("pmf's first argument is not x.")
  }
  if (args[2] != "mu") {
    stop("pmf's second argument is not mu.")
  }
  if (length(args) == 3) {
    if (args[3] != "log") {
      stop("pmf's third argument is not log.")
    }
  }
  if (length(args) == 4) {
    if (args[3] != "size") {
      stop("pmf's third argument is not size.")
    }
    if (args[4] != "log") {
      stop("pmf's fourth argument is not log.")
    }
  }
  if (length(args) > 4) {
    stop("pmf has too many arguments.")
  }
  rm(args)

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

  if (length(names(formals(pmf))) == 3) {
    args <- rlang::pairlist2(q = , mu = , lower.tail = TRUE, log.p = FALSE)

    sizeCheck <- rlang::expr({})

    calcPmfs <- rlang::expr(pmf(x = tempXs, mu = mu, log = FALSE))
  }
  if (length(names(formals(pmf))) == 4) {
    args <- rlang::pairlist2(q = , mu = , size = , lower.tail = TRUE, log.p = FALSE)

    sizeCheck <- rlang::expr({
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
    })

    calcPmfs <- rlang::expr(pmf(x = tempXs, mu = mu, size = size, log = FALSE))
  }

  body <- rlang::expr({
    # check inputs
    if (length(q) < 1) {
      stop("Argument q must have positive length.")
    }
    if (!is.numeric(q)) {
      stop("Argument q must be numeric.")
    }
    if (any(q < XLB)) {
      stop(paste("All elements in q must be greater than or equal to ", XLB, sep = ""))
    }
    if (any(q > XUB)) {
      stop(paste("All elements in q must be less than or equal to ", XUB, sep = ""))
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

    !!sizeCheck

    if (length(lower.tail) != 1) {
      stop("Argument lower.tail must have length one.")
    }
    if (!is.logical(lower.tail)) {
      stop("Argument lower.tail must be logical.")
    }

    if (length(log.p) != 1) {
      stop("Argument log.p must have length one.")
    }
    if (!is.logical(log.p)) {
      stop("Argument log.p must be logical.")
    }

    p <- vector(mode = "numeric", length = length(q))
    for (i in seq_along(p)) {
      if (is.finite(q[i])) {
        tempXs <- XLB:q[i]
        pmfs <- !!calcPmfs
        p[i] <- sum(pmfs)
      } else {
        p[i] <- 1
      }
    }

    if (!lower.tail) {
      p <- 1 - p
    }

    if (log.p) {
      p <- log(p)
    }

    return(p)
  })

  exec_globals <- list(pmf = pmf, XLB = XLB, XUB = XUB, MLB = MLB, MUB = MUB)
  exec_env <- rlang::new_environment(data = exec_globals, parent = rlang::base_env())

  f <- rlang::new_function(args, body, env = exec_env)

  return(f)
}
