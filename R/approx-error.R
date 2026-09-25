#' Compute Approximation Errors
#'
#' Computes pointwise approximation errors for an approximation
#' operator at one or more evaluation points.
#'
#' For an operator L_n and a function f, the signed error is
#'
#'   L_n(f; x) - f(x),
#'
#' while the absolute and relative errors are
#'
#'   |L_n(f; x) - f(x)|
#'
#' and
#'
#'   |L_n(f; x) - f(x)| / |f(x)|,
#'
#' respectively. If f(x) = 0, the relative error is returned as NA.
#'
#' @param operator An object of class \code{"approx_operator"}.
#' @param f Function to be approximated.
#' @param x Numeric vector of evaluation points.
#'
#' @return An object of class \code{"approx_error"} and
#'   \code{"data.frame"} containing the evaluation points, exact
#'   function values, approximated values, signed errors, absolute
#'   errors, and relative errors.
#'
#' @export
approx_error <- function(operator, f, x) {

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

  approximation <- approximate(
    operator = operator,
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

  result <- data.frame(
    x = x,
    exact = exact,
    approximation = approximation,
    signed_error = signed_error,
    absolute_error = absolute_error,
    relative_error = relative_error
  )

  attr(
    result,
    "max_absolute_error"
  ) <- max(
    absolute_error
  )

  attr(
    result,
    "mae"
  ) <- mean(
    absolute_error
  )

  attr(
    result,
    "rmse"
  ) <- sqrt(
    mean(
      signed_error^2
    )
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
    "n"
  ) <- operator$n

  class(result) <- c(
    "approx_error",
    "data.frame"
  )

  result
}
