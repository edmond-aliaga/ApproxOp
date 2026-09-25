#' Create Approximation Data
#'
#' Creates numerical data for studying and plotting the approximation
#' of a function by an approximation operator for one or more values
#' of n.
#'
#' For each evaluation point x and each value of n, the function
#' computes the exact value f(x), the approximation L_n(f; x),
#' the signed error, the absolute error, and the relative error.
#'
#' @param operator An object of class \code{"approx_operator"}.
#' @param f Function to be approximated.
#' @param x Numeric vector of evaluation points.
#' @param n Optional vector of positive integers. If \code{NULL},
#'   the value of \code{n} stored in \code{operator} is used.
#'
#' @return An object of class \code{"approximation_data"} and
#'   \code{"data.frame"} containing the columns \code{x}, \code{n},
#'   \code{exact}, \code{approximation}, \code{signed_error},
#'   \code{absolute_error}, and \code{relative_error}.
#'
#' @export
approximation_data <- function(
    operator,
    f,
    x,
    n = NULL
) {

  validate_approx_operator(operator)

  if (!is.function(f)) {
    stop(
      "`f` must be a function.",
      call. = FALSE
    )
  }

  if (!is.numeric(x) ||
      length(x) < 1L ||
      anyNA(x) ||
      any(!is.finite(x))) {
    stop(
      "`x` must be a non-empty vector of finite numeric values.",
      call. = FALSE
    )
  }

  if (any(x < operator$domain[1] |
          x > operator$domain[2])) {
    stop(
      "All evaluation points must belong to the operator domain.",
      call. = FALSE
    )
  }

  if (is.null(n)) {
    n <- operator$n
  }

  if (!is.numeric(n) ||
      length(n) < 1L ||
      anyNA(n) ||
      any(!is.finite(n)) ||
      any(n <= 0) ||
      any(n != floor(n))) {
    stop(
      "`n` must contain positive integers.",
      call. = FALSE
    )
  }

  n <- as.integer(n)

  exact <- vapply(
    x,
    function(xi) {

      value <- f(xi)

      if (!is.numeric(value) ||
          length(value) != 1L ||
          !is.finite(value)) {
        stop(
          "`f` must return one finite numeric value at each evaluation point.",
          call. = FALSE
        )
      }

      value
    },
    numeric(1)
  )

  pieces <- vector(
    "list",
    length(n)
  )

  for (j in seq_along(n)) {

    current_operator <- operator
    current_operator$n <- n[j]

    validate_approx_operator(
      current_operator
    )

    approximation <- approximate(
      operator = current_operator,
      f = f,
      x = x
    )

    signed_error <-
      approximation - exact

    absolute_error <-
      abs(signed_error)

    relative_error <- rep(
      NA_real_,
      length(x)
    )

    nonzero <- exact != 0

    relative_error[nonzero] <-
      absolute_error[nonzero] /
      abs(exact[nonzero])

    pieces[[j]] <- data.frame(
      x = x,
      n = rep(n[j], length(x)),
      exact = exact,
      approximation = approximation,
      signed_error = signed_error,
      absolute_error = absolute_error,
      relative_error = relative_error
    )
  }

  result <- do.call(
    rbind,
    pieces
  )

  rownames(result) <- NULL

  attr(
    result,
    "operator_family"
  ) <- operator$family

  attr(
    result,
    "operator_subfamily"
  ) <- operator$subfamily

  attr(
    result,
    "operator_variant"
  ) <- operator$variant

  attr(
    result,
    "n_values"
  ) <- n

  class(result) <- c(
    "approximation_data",
    "data.frame"
  )

  result
}
