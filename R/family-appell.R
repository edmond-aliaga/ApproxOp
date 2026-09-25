# Appell / Jakimovski-Leviatan operator
#
# Internal implementation of the Appell-based Szász-type operator.
#
# Let
#
#   A(t) * exp(x * t)
#     = sum_{k=0}^infinity p_k(x) t^k.
#
# The associated Jakimovski-Leviatan operator is
#
#   P_n(f; x) =
#     exp(-n*x) / A(1) *
#     sum_{k=0}^infinity
#     p_k(n*x) * f(k/n),
#
# for x >= 0.
#
# For numerical evaluation, the user supplies
#
#   A(t)
#
# and a coefficient function
#
#   coefficients(x, k)
#
# returning p_k(x).


appell_parameters <- function(operator) {

  parameters <- operator$parameters

  A <- parameters$A
  coefficients <- parameters$coefficients

  if (is.null(A) || !is.function(A)) {
    stop(
      "`parameters$A` must be supplied as a function.",
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

  if (!is.numeric(A1) ||
      length(A1) != 1L ||
      !is.finite(A1) ||
      A1 == 0) {
    stop(
      "`A(1)` must be one finite non-zero numeric value.",
      call. = FALSE
    )
  }

  list(
    A = A,
    coefficients = coefficients,
    A1 = A1
  )
}


appell_control <- function(operator) {

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


appell_evaluate_one <- function(f, x, operator) {

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
      "For the Appell operator, `x` must be non-negative.",
      call. = FALSE
    )
  }

  n <- operator$n

  pars <- appell_parameters(operator)
  ctrl <- appell_control(operator)

  A1 <- pars$A1
  coefficients <- pars$coefficients

  tol <- ctrl$tol
  max_terms <- ctrl$max_terms

  normalization <- exp(-n * x) / A1

  if (!is.finite(normalization)) {
    stop(
      "The Appell normalization factor is non-finite.",
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
          "The Appell coefficient p_",
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
          "A negative Appell weight was generated at index ",
          k,
          "."
        ),
        call. = FALSE
      )
    }

    node <- k / n

    f_value <- f(node)

    if (!is.numeric(f_value) ||
        length(f_value) != 1L ||
        !is.finite(f_value)) {
      stop(
        paste0(
          "`f` must return one finite numeric value ",
          "at each Appell node."
        ),
        call. = FALSE
      )
    }

    result <- result +
      weight * f_value

    weight_sum <- weight_sum +
      weight

    # The normalized Appell weights must sum to 1.
    # Stop when the remaining mass is within tolerance.
    if (weight_sum >= 1 - tol &&
        weight_sum <= 1 + tol) {
      return(result)
    }

    if (weight_sum > 1 + 100 * tol) {
      stop(
        paste0(
          "The cumulative Appell weights exceed 1 ",
          "beyond the allowed numerical tolerance."
        ),
        call. = FALSE
      )
    }
  }

  stop(
    paste0(
      "The Appell summation did not converge within ",
      max_terms,
      " terms. Increase `control$max_terms` or adjust ",
      "`control$tol`."
    ),
    call. = FALSE
  )
}


appell_evaluate <- function(f, x, operator) {

  vapply(
    x,
    function(xi) {
      appell_evaluate_one(
        f = f,
        x = xi,
        operator = operator
      )
    },
    numeric(1)
  )
}
