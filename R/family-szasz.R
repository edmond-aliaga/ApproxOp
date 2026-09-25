# Szasz-Mirakyan operator
#
# Internal implementation of the classical Szasz-Mirakyan operator.
#
# For n > 0 and x >= 0,
#
#   S_n(f; x) =
#     exp(-n*x) * sum_{k=0}^infinity
#     (n*x)^k / k! * f(k/n).
#
# Equivalently, if K ~ Poisson(n*x),
#
#   S_n(f; x) = E[f(K/n)].
#
# This file contains the evaluation engine used internally by ApproxOp.


# Evaluate the Szasz-Mirakyan operator at one point
szasz_evaluate_one <- function(f, x, n, control = list()) {

  if (!is.function(f)) {
    stop("`f` must be a function.", call. = FALSE)
  }

  if (!is.numeric(x) || length(x) != 1L || !is.finite(x)) {
    stop("`x` must be one finite numeric value.", call. = FALSE)
  }

  if (x < 0) {
    stop("For the Szasz-Mirakyan operator, `x` must be non-negative.",
         call. = FALSE)
  }

  if (!is.numeric(n) ||
      length(n) != 1L ||
      !is.finite(n) ||
      n <= 0 ||
      n != floor(n)) {
    stop("`n` must be a positive integer.", call. = FALSE)
  }

  lambda <- n * x

  # At x = 0 the Poisson distribution is concentrated at k = 0.
  if (lambda == 0) {
    return(f(0))
  }

  tol <- control$tol
  if (is.null(tol)) {
    tol <- 1e-12
  }

  if (!is.numeric(tol) ||
      length(tol) != 1L ||
      !is.finite(tol) ||
      tol <= 0 ||
      tol >= 1) {
    stop("`control$tol` must be a number between 0 and 1.",
         call. = FALSE)
  }

  # Adaptive truncation based on the Poisson tail probability.
  lower <- stats::qpois(tol / 2, lambda = lambda)
  upper <- stats::qpois(1 - tol / 2, lambda = lambda)

  k <- seq.int(lower, upper)

  weights <- stats::dpois(k, lambda = lambda)

  values <- vapply(
    k / n,
    f,
    numeric(1)
  )

  sum(weights * values)
}


# Vectorized evaluation engine
szasz_evaluate <- function(f, x, operator) {

  vapply(
    x,
    function(xi) {
      szasz_evaluate_one(
        f = f,
        x = xi,
        n = operator$n,
        control = operator$control
      )
    },
    numeric(1)
  )
}
