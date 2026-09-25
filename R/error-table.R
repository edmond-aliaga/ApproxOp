#' Create an Approximation Error Table
#'
#' Creates a numerical error table for an approximation operator.
#' The table may be computed for the operator's current value of n
#' or for several values of n.
#'
#' @param operator An object of class \code{"approx_operator"}.
#' @param f Function to be approximated.
#' @param x Numeric vector of evaluation points.
#' @param n Optional vector of positive integers. If \code{NULL},
#'   the value of \code{n} stored in \code{operator} is used.
#' @param digits Number of digits used when printing the table.
#'
#' @return An object of class \code{"approx_error_table"} and
#'   \code{"data.frame"} containing pointwise absolute approximation
#'   errors.
#'
#' @export
error_table <- function(
    operator,
    f,
    x,
    n = NULL,
    digits = 6
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

  if (!is.numeric(digits) ||
      length(digits) != 1L ||
      !is.finite(digits) ||
      digits < 0 ||
      digits != floor(digits)) {
    stop(
      "`digits` must be a non-negative integer.",
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

  result <- data.frame(
    x = x,
    exact = exact
  )

  max_errors <- numeric(length(n))
  mae <- numeric(length(n))
  rmse <- numeric(length(n))

  for (j in seq_along(n)) {

    current_operator <- operator
    current_operator$n <- n[j]

    validate_approx_operator(
      current_operator
    )

    errors <- approx_error(
      operator = current_operator,
      f = f,
      x = x
    )

    error_name <- paste0(
      "error_n",
      n[j]
    )

    result[[error_name]] <-
      errors$absolute_error

    max_errors[j] <-
      attr(
        errors,
        "max_absolute_error"
      )

    mae[j] <-
      attr(
        errors,
        "mae"
      )

    rmse[j] <-
      attr(
        errors,
        "rmse"
      )
  }

  summary_table <- data.frame(
    n = n,
    max_absolute_error = max_errors,
    mae = mae,
    rmse = rmse
  )

  attr(
    result,
    "summary"
  ) <- summary_table

  attr(
    result,
    "digits"
  ) <- as.integer(digits)

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
    "approx_error_table",
    "data.frame"
  )

  result
}
