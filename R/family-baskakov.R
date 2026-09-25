# Baskakov operator
#
# Internal implementation of the classical Baskakov operator.
#
# For n > 0 and x >= 0,
#
#   V_n(f; x) =
#     sum_{k=0}^infinity
#     choose(n + k - 1, k) *
#     x^k / (1 + x)^(n + k) *
#     f(k/n).
#
# The weights correspond to a negative binomial distribution.
#
# If K has a negative binomial distribution with
#
#   size = n
#   prob = 1 / (1 + x),
#
# then
#
#   V_n(f; x) = E[f(K/n)].


# Evaluate the Baskakov operator at one point
baskakov_evaluate_one <- function(f, x, n, control = list()) {

  if (!is.function(f)) {
    stop("`f` must be a function.", call. = FALSE)
  }

  if (!is.numeric(x) ||
      length(x) != 1L ||
      !is.finite(x)) {
    stop("`x` must be one finite numeric value.", call. = FALSE)
  }

  if (x < 0) {
    stop(
      "For the Baskakov operator, `x` must be non-negative.",
      call. = FALSE
    )
  }

  if (!is.numeric(n) ||
      length(n) != 1L ||
      !is.finite(n) ||
      n <= 0 ||
      n != floor(n)) {
    stop("`n` must be a positive integer.", call. = FALSE)
  }

  # At x = 0 all mass is concentrated at k = 0.
  if (x == 0) {
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
    stop(
      "`control$tol` must be a number between 0 and 1.",
      call. = FALSE
    )
  }

  prob <- 1 / (1 + x)

  # Adaptive truncation using negative-binomial tail probabilities.
  lower <- stats::qnbinom(
    tol / 2,
    size = n,
    prob = prob
  )

  upper <- stats::qnbinom(
    1 - tol / 2,
    size = n,
    prob = prob
  )

  k <- seq.int(lower, upper)

  weights <- stats::dnbinom(
    k,
    size = n,
    prob = prob
  )

  values <- vapply(
    k / n,
    f,
    numeric(1)
  )

  sum(weights * values)
}


# Vectorized evaluation engine
baskakov_evaluate <- function(f, x, operator) {

  vapply(
    x,
    function(xi) {
      baskakov_evaluate_one(
        f = f,
        x = xi,
        n = operator$n,
        control = operator$control
      )
    },
    numeric(1)
  )
}
