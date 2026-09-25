# Meixner operator
#
# Internal implementation of the Meixner-based Szász-type operator.
#
# For n > 0, x >= 0, 0 <= alpha <= beta,
# theta > 0, and 1/2 < c < 1,
#
#   M_n^{alpha,beta}(f; x) =
#     sum_{s=0}^infinity
#     w_{n,s}(x; theta, c) *
#     f((s + alpha) / (n + beta)).
#
# The package notation alpha and beta corresponds to kappa and eta,
# respectively, in the original formulation of the operator.
#
# The Meixner weights are generated recursively. This avoids direct
# evaluation of high-order Meixner polynomials.
#
# The recurrence is
#
#   w_{s+1} =
#     ([2*c*theta + (2*c - 1)*n*x + 2*(c + 1)*s] * w_s
#       - (s + theta - 1) * w_{s-1}) /
#     (4*c*(s + 1)).
#
# The initial weight is
#
#   w_0 =
#     2^(-theta) *
#     ((2*c - 1)/c)^{
#       n*x*(1 - 2*c)/(2*(c - 1))
#     }.


meixner_parameters <- function(operator) {

  parameters <- operator$parameters

  alpha <- parameters$alpha
  beta  <- parameters$beta
  theta <- parameters$theta
  c     <- parameters$c

  if (is.null(alpha)) {
    alpha <- 0
  }

  if (is.null(beta)) {
    beta <- 0
  }

  if (is.null(theta)) {
    stop(
      "`parameters$theta` must be supplied for the Meixner operator.",
      call. = FALSE
    )
  }

  if (is.null(c)) {
    stop(
      "`parameters$c` must be supplied for the Meixner operator.",
      call. = FALSE
    )
  }

  values <- list(
    alpha = alpha,
    beta = beta,
    theta = theta,
    c = c
  )

  for (name in names(values)) {
    value <- values[[name]]

    if (!is.numeric(value) ||
        length(value) != 1L ||
        !is.finite(value)) {
      stop(
        paste0(
          "`parameters$",
          name,
          "` must be one finite numeric value."
        ),
        call. = FALSE
      )
    }
  }

  if (alpha < 0) {
    stop(
      "`parameters$alpha` must be non-negative.",
      call. = FALSE
    )
  }

  if (beta < alpha) {
    stop(
      "`parameters$beta` must satisfy beta >= alpha.",
      call. = FALSE
    )
  }

  if (theta <= 0) {
    stop(
      "`parameters$theta` must be positive.",
      call. = FALSE
    )
  }

  if (c <= 0.5 || c >= 1) {
    stop(
      "`parameters$c` must satisfy 0.5 < c < 1.",
      call. = FALSE
    )
  }

  values
}


meixner_initial_weight <- function(x, n, theta, c) {

  exponent <- n * x * (1 - 2 * c) /
    (2 * (c - 1))

  log_w0 <- -theta * log(2) +
    exponent * log((2 * c - 1) / c)

  exp(log_w0)
}


meixner_evaluate_one <- function(f, x, operator) {

  if (!is.function(f)) {
    stop("`f` must be a function.", call. = FALSE)
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
      "For the Meixner operator, `x` must be non-negative.",
      call. = FALSE
    )
  }

  n <- operator$n

  if (!is.numeric(n) ||
      length(n) != 1L ||
      !is.finite(n) ||
      n <= 0 ||
      n != floor(n)) {
    stop(
      "`n` must be a positive integer.",
      call. = FALSE
    )
  }

  pars <- meixner_parameters(operator)

  alpha <- pars$alpha
  beta  <- pars$beta
  theta <- pars$theta
  c     <- pars$c

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
      max_terms < 2 ||
      max_terms != floor(max_terms)) {
    stop(
      "`control$max_terms` must be an integer greater than or equal to 2.",
      call. = FALSE
    )
  }

  w_prev <- meixner_initial_weight(
    x = x,
    n = n,
    theta = theta,
    c = c
  )

  node0 <- alpha / (n + beta)

  result <- w_prev * f(node0)
  weight_sum <- w_prev

  numerator1 <-
    2 * c * theta +
    (2 * c - 1) * n * x

  w_curr <- numerator1 * w_prev / (4 * c)

  node1 <- (1 + alpha) / (n + beta)

  result <- result + w_curr * f(node1)
  weight_sum <- weight_sum + w_curr

  s <- 1L

  while (s < max_terms - 1L) {

    numerator <-
      (
        2 * c * theta +
          (2 * c - 1) * n * x +
          2 * (c + 1) * s
      ) * w_curr -
      (s + theta - 1) * w_prev

    w_next <- numerator /
      (4 * c * (s + 1))

    if (!is.finite(w_next)) {
      stop(
        "Non-finite Meixner weight encountered.",
        call. = FALSE
      )
    }

    # Remove negligible negative round-off only.
    if (w_next < 0 &&
        abs(w_next) <=
        100 * .Machine$double.eps *
        max(1, abs(w_curr), abs(w_prev))) {
      w_next <- 0
    }

    if (w_next < 0) {
      stop(
        paste0(
          "A negative Meixner weight was generated at index ",
          s + 1L,
          "."
        ),
        call. = FALSE
      )
    }

    node <- (s + 1 + alpha) / (n + beta)

    result <- result + w_next * f(node)
    weight_sum <- weight_sum + w_next

    if ((1 - weight_sum) <= tol &&
        w_next <= tol) {
      return(result)
    }

    w_prev <- w_curr
    w_curr <- w_next
    s <- s + 1L
  }

  stop(
    paste0(
      "The Meixner summation did not converge within ",
      max_terms,
      " terms. Increase `control$max_terms` or adjust `control$tol`."
    ),
    call. = FALSE
  )
}


meixner_evaluate <- function(f, x, operator) {

  vapply(
    x,
    function(xi) {
      meixner_evaluate_one(
        f = f,
        x = xi,
        operator = operator
      )
    },
    numeric(1)
  )
}
