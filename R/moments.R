#' Compute Moments of an Approximation Operator
#'
#' Computes ordinary or central moments of a positive linear
#' approximation operator.
#'
#' The ordinary moment of order r is
#' m_r(x) = L_n(t^r; x),
#' while the central moment is
#' mu_r(x) = L_n((t - x)^r; x).
#'
#' If a registered moment engine is available for the operator,
#' it is used to compute the ordinary moments. Otherwise, the
#' ordinary moments are computed through repeated calls to
#' \code{approximate()}.
#'
#' @param operator An object of class \code{"approx_operator"}.
#' @param order Non-negative integer specifying the maximum moment order.
#'   In version 0.1.0, orders from 0 to 8 are supported.
#' @param x Numeric vector of evaluation points.
#' @param central Logical. If \code{FALSE}, ordinary moments are returned.
#'   If \code{TRUE}, central moments are returned.
#'
#' @return A matrix whose rows correspond to evaluation points and whose
#'   columns correspond to moment orders from 0 to \code{order}.
#'
#' @export
moments <- function(operator, order = 8L, x, central = FALSE) {

  validate_approx_operator(operator)

  # Validate moment order
  if (length(order) != 1L ||
      !is.numeric(order) ||
      !is.finite(order) ||
      order != as.integer(order) ||
      order < 0L ||
      order > 8L) {

    stop(
      "`order` must be an integer between 0 and 8.",
      call. = FALSE
    )
  }

  order <- as.integer(order)

  # Validate evaluation points
  if (!is.numeric(x) ||
      length(x) < 1L ||
      anyNA(x) ||
      any(!is.finite(x))) {

    stop(
      "`x` must be a non-empty vector of finite numeric values.",
      call. = FALSE
    )
  }

  # Check domain
  if (any(x < operator$domain[1] |
          x > operator$domain[2])) {
    stop(
      "All evaluation points must belong to the operator domain.",
      call. = FALSE
    )
  }

  # Validate central argument
  if (length(central) != 1L ||
      !is.logical(central) ||
      is.na(central)) {

    stop(
      "`central` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  # Use a registered moment engine when one is available.
  if (!is.null(operator$moment_engine)) {

    if (!is.function(operator$moment_engine)) {
      stop(
        "`moment_engine` must be NULL or a function.",
        call. = FALSE
      )
    }

    ordinary <- operator$moment_engine(
      operator = operator,
      order = order,
      x = x
    )

    if (!is.numeric(ordinary) ||
        length(ordinary) !=
        length(x) * (order + 1L)) {
      stop(
        paste0(
          "The registered moment engine must return a numeric ",
          "matrix with `length(x)` rows and `order + 1` columns."
        ),
        call. = FALSE
      )
    }

    ordinary <- matrix(
      ordinary,
      nrow = length(x),
      ncol = order + 1L
    )

  } else {

    # Universal fallback:
    # compute ordinary moments through the evaluation engine.
    ordinary <- vapply(
      0:order,
      function(r) {
        approximate(
          operator = operator,
          f = function(t) t^r,
          x = x
        )
      },
      numeric(length(x))
    )

    # Ensure matrix structure also when x has length 1
    ordinary <- matrix(
      ordinary,
      nrow = length(x),
      ncol = order + 1L
    )
  }

  if (anyNA(ordinary) ||
      any(!is.finite(ordinary))) {
    stop(
      "Moment computation returned non-finite or missing values.",
      call. = FALSE
    )
  }

  colnames(ordinary) <- paste0(
    "m",
    0:order
  )

  rownames(ordinary) <- NULL

  # Return ordinary moments if requested
  if (!central) {
    return(ordinary)
  }

  # Compute central moments from ordinary moments
  central_moments <- matrix(
    0,
    nrow = length(x),
    ncol = order + 1L
  )

  for (r in 0:order) {

    for (j in 0:r) {

      central_moments[, r + 1L] <-
        central_moments[, r + 1L] +
        choose(r, j) *
        (-x)^(r - j) *
        ordinary[, j + 1L]
    }
  }

  colnames(central_moments) <- paste0(
    "mu",
    0:order
  )

  rownames(central_moments) <- NULL

  central_moments
}
