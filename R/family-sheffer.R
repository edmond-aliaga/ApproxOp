# General Sheffer operator
#
# Internal implementation of the general Sheffer-based Szász-type operator.
#
# Let
#
#   A(t) * exp(x * H(t))
#     = sum_{k=0}^infinity p_k(x) t^k.
#
# The associated operator is
#
#   T_n(f; x) =
#     exp(-n*x*H(1)) / A(1) *
#     sum_{k=0}^infinity
#     p_k(n*x) * f(k/n).
#
# For numerical evaluation, the user supplies a coefficient function
#
#   coefficients(x, k)
#
# returning p_k(x).
#
# The functions A and H are retained as part of the mathematical
# specification of the Sheffer system.
#
# For the approximation framework considered here, the normalization
# condition
#
#   H'(1) = 1
#
# is supplied analytically through parameters$Hprime1.


sheffer_parameters <- function(operator) {

  parameters <- operator$parameters

  A <- parameters$A
  H <- parameters$H
  Hprime1 <- parameters$Hprime1
  coefficients <- parameters$coefficients

  if (is.null(A) || !is.function(A)) {
    stop(
      "`parameters$A` must be supplied as a function.",
      call. = FALSE
    )
  }

  if (is.null(H) || !is.function(H)) {
    stop(
      "`parameters$H` must be supplied as a function.",
      call. = FALSE
    )
  }

  if (is.null(Hprime1) ||
      !is.numeric(Hprime1) ||
      length(Hprime1) != 1L ||
      !is.finite(Hprime1)) {
    stop(
      "`parameters$Hprime1` must be supplied as one finite numeric value.",
      call. = FALSE
    )
  }

  if (!isTRUE(
    all.equal(
      Hprime1,
      1,
      tolerance = 1e-10
    )
  )) {
    stop(
      "The general Sheffer approximation framework requires `Hprime1 = 1`.",
      call. = FALSE
    )
  }

  if (is.null(coefficients) || !is.function(coefficients)) {
    stop(
      "`parameters$coefficients` must be supplied as a function.",
      call. = FALSE
    )
  }

  A1 <- A(1)
  H1 <- H(1)

  if (!is.numeric(A1) ||
      length(A1) != 1L ||
      !is.finite(A1) ||
      A1 == 0) {
    stop(
      "`A(1)` must be one finite non-zero numeric value.",
      call. = FALSE
    )
  }

  if (!is.numeric(H1) ||
      length(H1) != 1L ||
      !is.finite(H1)) {
    stop(
      "`H(1)` must be one finite numeric value.",
      call. = FALSE
    )
  }

  list(
    A = A,
    H = H,
    Hprime1 = Hprime1,
    coefficients = coefficients,
    A1 = A1,
    H1 = H1
  )
}


sheffer_control <- function(operator) {

  tol <- operator$control$tol

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

  max_terms <- operator$control$max_terms

  if (is.null(max_terms)) {
    max_terms <- 100000L
  }

  if (!is.numeric(max_terms) ||
      length(max_terms) != 1L ||
      !is.finite(max_terms) ||
      max_terms < 1 ||
      max_terms != floor(max_terms)) {
    stop(
      "`control$max_terms` must be a positive integer.",
      call. = FALSE
    )
  }

  list(
    tol = tol,
    max_terms = as.integer(max_terms)
  )
}


sheffer_evaluate_one <- function(f, x, operator) {

  if (!is.function(f)) {
    stop(
      "`f` must be a function.",
      call. = FALSE
    )
  }

  if (!is.numeric(x) ||
      length(x) != 1L ||
      !is.finite(x)) {
    stop(
      "`x` must be one finite numeric value.",
      call. = FALSE
    )
  }

  if (x < 0) {
    stop(
      "For the Sheffer operator, `x` must be non-negative.",
      call. = FALSE
    )
  }

  n <- operator$n

  pars <- sheffer_parameters(operator)
  ctrl <- sheffer_control(operator)

  A1 <- pars$A1
  H1 <- pars$H1
  coefficients <- pars$coefficients

  tol <- ctrl$tol
  max_terms <- ctrl$max_terms

  normalization <- exp(-n * x * H1) / A1

  if (!is.finite(normalization)) {
    stop(
      "The Sheffer normalization factor is non-finite.",
      call. = FALSE
    )
  }

  result <- 0
  weight_sum <- 0

  for (k in 0:(max_terms - 1L)) {

    pk <- coefficients(
      n * x,
      k
    )

    if (!is.numeric(pk) ||
        length(pk) != 1L ||
        !is.finite(pk)) {
      stop(
        paste0(
          "The Sheffer coefficient p_",
          k,
          "(n*x) must be one finite numeric value."
        ),
        call. = FALSE
      )
    }

    weight <- normalization * pk

    # Remove negligible negative round-off only.
    if (weight < 0 &&
        abs(weight) <=
        100 * .Machine$double.eps *
        max(1, abs(weight_sum))) {
      weight <- 0
    }

    if (weight < 0) {
      stop(
        paste0(
          "A negative Sheffer weight was generated at index ",
          k,
          "."
        ),
        call. = FALSE
      )
    }

    node <- k / n

    result <- result +
      weight * f(node)

    weight_sum <- weight_sum +
      weight

    # The normalized Sheffer weights must sum to 1.
    # Stop once the remaining mass is within the requested tolerance.
    if (weight_sum >= 1 - tol &&
        weight_sum <= 1 + tol) {
      return(result)
    }

    if (weight_sum > 1 + 100 * tol) {
      stop(
        paste0(
          "The cumulative Sheffer weights exceed 1 ",
          "beyond the allowed numerical tolerance."
        ),
        call. = FALSE
      )
    }
  }

  stop(
    paste0(
      "The Sheffer summation did not converge within ",
      max_terms,
      " terms. Increase `control$max_terms` or adjust ",
      "`control$tol`."
    ),
    call. = FALSE
  )
}


sheffer_evaluate <- function(f, x, operator) {

  vapply(
    x,
    function(xi) {
      sheffer_evaluate_one(
        f = f,
        x = xi,
        operator = operator
      )
    },
    numeric(1)
  )
}
