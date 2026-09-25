#' Study Numerical Convergence
#'
#' Computes numerical error measures for an approximation operator
#' over a grid of evaluation points and for one or more values of n.
#'
#' For each value of n, the function computes the maximum absolute
#' error, mean absolute error (MAE), and root mean squared error
#' (RMSE).
#'
#' @param operator An object of class \code{"approx_operator"}.
#' @param f Function to be approximated.
#' @param grid Numeric vector of evaluation points.
#' @param n Vector of positive integers.
#'
#' @return An object of class \code{"approx_convergence"} and
#'   \code{"data.frame"} containing the columns \code{n},
#'   \code{max_absolute_error}, \code{mae}, and \code{rmse}.
#'
#' @export
convergence <- function(
    operator,
    f,
    grid,
    n
) {

  validate_approx_operator(operator)

  if (!is.function(f)) {
    stop(
      "`f` must be a function.",
      call. = FALSE
    )
  }

  if (!is.numeric(grid) ||
      length(grid) < 1L ||
      anyNA(grid) ||
      any(!is.finite(grid))) {
    stop(
      "`grid` must be a non-empty vector of finite numeric values.",
      call. = FALSE
    )
  }

  if (any(grid < operator$domain[1] |
          grid > operator$domain[2])) {
    stop(
      "All evaluation points must belong to the operator domain.",
      call. = FALSE
    )
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

  max_absolute_error <- numeric(
    length(n)
  )

  mae <- numeric(
    length(n)
  )

  rmse <- numeric(
    length(n)
  )

  for (j in seq_along(n)) {

    current_operator <- operator
    current_operator$n <- n[j]

    validate_approx_operator(
      current_operator
    )

    errors <- approx_error(
      operator = current_operator,
      f = f,
      x = grid
    )

    max_absolute_error[j] <- attr(
      errors,
      "max_absolute_error"
    )

    mae[j] <- attr(
      errors,
      "mae"
    )

    rmse[j] <- attr(
      errors,
      "rmse"
    )
  }

  result <- data.frame(
    n = n,
    max_absolute_error = max_absolute_error,
    mae = mae,
    rmse = rmse
  )

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
    "grid_size"
  ) <- length(grid)

  attr(
    result,
    "grid_range"
  ) <- range(grid)

  class(result) <- c(
    "approx_convergence",
    "data.frame"
  )

  result
}
