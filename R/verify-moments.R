#' Verify moments of an approximation operator
#'
#' Compares moments computed by the moment engine with moments obtained
#' directly from the numerical evaluation of the operator.
#'
#' @param operator An object of class \code{approx_operator}.
#' @param order Maximum moment order to verify. Default is 8.
#' @param x Evaluation point or vector of evaluation points.
#' @param central Logical. If \code{TRUE}, central moments are verified.
#' @param tolerance Numerical tolerance used to determine whether the
#'   verification is successful.
#'
#' @return A data frame containing the moment order, computed value,
#'   numerical value, absolute error, relative error, and verification status.
#'
#' @export
verify_moments <- function(operator,
                           order = 8,
                           x,
                           central = FALSE,
                           tolerance = 1e-10) {

  validate_approx_operator(operator)

  if (length(order) != 1L ||
      !is.numeric(order) ||
      is.na(order) ||
      order < 0 ||
      order != as.integer(order)) {
    stop("`order` must be a non-negative integer.", call. = FALSE)
  }

  if (order > 8L) {
    stop(
      "Moment verification is currently supported up to order 8.",
      call. = FALSE
    )
  }

  if (!is.numeric(x) || any(!is.finite(x))) {
    stop("`x` must contain finite numeric values.", call. = FALSE)
  }

  if (length(central) != 1L || !is.logical(central) || is.na(central)) {
    stop("`central` must be TRUE or FALSE.", call. = FALSE)
  }

  if (length(tolerance) != 1L ||
      !is.numeric(tolerance) ||
      is.na(tolerance) ||
      tolerance <= 0) {
    stop("`tolerance` must be a positive number.", call. = FALSE)
  }

  computed <- moments(
    operator = operator,
    order = order,
    x = x,
    central = central
  )

  result <- vector("list", length(x) * (order + 1L))
  counter <- 1L

  for (i in seq_along(x)) {

    xi <- x[i]

    for (r in 0:order) {

      if (central) {
        f <- local({
          rr <- r
          xx <- xi
          function(t) (t - xx)^rr
        })
      } else {
        f <- local({
          rr <- r
          function(t) t^rr
        })
      }

      numerical <- approximate(
        operator = operator,
        f = f,
        x = xi
      )

      computed_value <- computed[i, r + 1L]

      abs_error <- abs(computed_value - numerical)

      scale <- max(abs(computed_value), abs(numerical))

      if (scale == 0) {
        rel_error <- 0
      } else {
        rel_error <- abs_error / scale
      }

      result[[counter]] <- data.frame(
        x = xi,
        order = r,
        computed = computed_value,
        numerical = numerical,
        abs_error = abs_error,
        rel_error = rel_error,
        status = abs_error <= tolerance,
        stringsAsFactors = FALSE
      )

      counter <- counter + 1L
    }
  }

  result <- do.call(rbind, result)
  rownames(result) <- NULL

  result
}
