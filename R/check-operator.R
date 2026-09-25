#' Check an Approximation Operator
#'
#' Performs a collection of structural and numerical diagnostic checks
#' for an approximation operator.
#'
#' The function checks whether the operator can be evaluated successfully
#' on a set of points, whether the returned values are finite, whether
#' constants are reproduced within a specified tolerance, and optionally
#' whether the operator appears to preserve non-negativity for a collection
#' of simple non-negative test functions.
#'
#' This function is intended as a general diagnostic tool. Detailed
#' verification of ordinary or central moments is provided separately by
#' \code{verify_moments()}.
#'
#' @param operator An object of class \code{"approx_operator"}.
#' @param x Optional numeric vector of diagnostic evaluation points.
#'   If \code{NULL}, suitable points are selected automatically from the
#'   operator domain.
#' @param tolerance Positive numeric value used for numerical comparisons.
#' @param check_positivity Logical. If \code{TRUE}, simple numerical
#'   positivity checks are performed.
#'
#' @return An object of class \code{"approx_operator_check"} containing
#'   a data frame of diagnostic checks and an overall status.
#'
#' @export
check_operator <- function(
    operator,
    x = NULL,
    tolerance = 1e-10,
    check_positivity = TRUE
) {

  validate_approx_operator(operator)

  if (length(tolerance) != 1L ||
      !is.numeric(tolerance) ||
      is.na(tolerance) ||
      !is.finite(tolerance) ||
      tolerance <= 0) {
    stop(
      "`tolerance` must be one positive finite numeric value.",
      call. = FALSE
    )
  }

  if (!is.logical(check_positivity) ||
      length(check_positivity) != 1L ||
      is.na(check_positivity)) {
    stop(
      "`check_positivity` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (is.null(x)) {

    lower <- operator$domain[1]
    upper <- operator$domain[2]

    if (is.finite(lower) && is.finite(upper)) {

      x <- seq(
        lower,
        upper,
        length.out = 5L
      )

    } else if (is.finite(lower) &&
               is.infinite(upper)) {

      x <- lower + c(
        0,
        0.25,
        0.5,
        1,
        2
      )

    } else if (is.infinite(lower) &&
               is.finite(upper)) {

      x <- upper - c(
        2,
        1,
        0.5,
        0.25,
        0
      )

    } else {

      x <- c(
        -1,
        -0.5,
        0,
        0.5,
        1
      )
    }

  } else {

    if (!is.numeric(x) ||
        length(x) < 1L ||
        anyNA(x) ||
        any(!is.finite(x))) {
      stop(
        "`x` must be NULL or a non-empty vector of finite numeric values.",
        call. = FALSE
      )
    }

    if (any(
      x < operator$domain[1] |
      x > operator$domain[2]
    )) {
      stop(
        "All diagnostic points must belong to the operator domain.",
        call. = FALSE
      )
    }
  }

  add_check <- function(
    check,
    status,
    value = NA_real_,
    reference = NA_real_,
    error = NA_real_,
    message = ""
  ) {

    data.frame(
      check = check,
      status = isTRUE(status),
      value = value,
      reference = reference,
      error = error,
      message = message,
      stringsAsFactors = FALSE
    )
  }

  checks <- list()
  counter <- 1L

  evaluation <- tryCatch(
    approximate(
      operator = operator,
      f = function(t) t,
      x = x
    ),
    error = function(e) e
  )

  evaluation_ok <-
    !inherits(evaluation, "error") &&
    is.numeric(evaluation) &&
    length(evaluation) == length(x)

  evaluation_message <- ""

  if (inherits(evaluation, "error")) {
    evaluation_message <- conditionMessage(
      evaluation
    )
  } else if (!evaluation_ok) {
    evaluation_message <-
      "Operator evaluation did not return a numeric vector of the expected length."
  }

  checks[[counter]] <- add_check(
    check = "evaluation",
    status = evaluation_ok,
    message = evaluation_message
  )

  counter <- counter + 1L

  finite_ok <-
    evaluation_ok &&
    all(is.finite(evaluation))

  checks[[counter]] <- add_check(
    check = "finite_output",
    status = finite_ok,
    message = if (finite_ok) {
      ""
    } else {
      "The operator produced non-finite values or could not be evaluated."
    }
  )

  counter <- counter + 1L

  constant_result <- tryCatch(
    approximate(
      operator = operator,
      f = function(t) rep(1, length(t)),
      x = x
    ),
    error = function(e) e
  )

  constant_ok <-
    !inherits(constant_result, "error") &&
    is.numeric(constant_result) &&
    length(constant_result) == length(x) &&
    all(is.finite(constant_result))

  constant_error <- Inf

  if (constant_ok) {
    constant_error <- max(
      abs(constant_result - 1)
    )
  }

  constant_status <-
    constant_ok &&
    constant_error <= tolerance

  constant_message <- ""

  if (inherits(constant_result, "error")) {

    constant_message <- conditionMessage(
      constant_result
    )

  } else if (!constant_ok) {

    constant_message <-
      "The constant test did not return valid finite numeric values."

  } else if (!constant_status) {

    constant_message <-
      "The operator does not reproduce the constant function within tolerance."
  }

  checks[[counter]] <- add_check(
    check = "constant_reproduction",
    status = constant_status,
    value = if (constant_ok) {
      max(constant_result)
    } else {
      NA_real_
    },
    reference = 1,
    error = if (is.finite(constant_error)) {
      constant_error
    } else {
      NA_real_
    },
    message = constant_message
  )

  counter <- counter + 1L

  if (check_positivity) {

    positivity_functions <- list(
      square = function(t) t^2,
      shifted_square = function(t) (t - 1)^2,
      exponential = function(t) exp(-abs(t))
    )

    for (function_name in names(
      positivity_functions
    )) {

      positivity_result <- tryCatch(
        approximate(
          operator = operator,
          f = positivity_functions[[function_name]],
          x = x
        ),
        error = function(e) e
      )

      positivity_ok <-
        !inherits(positivity_result, "error") &&
        is.numeric(positivity_result) &&
        length(positivity_result) == length(x) &&
        all(is.finite(positivity_result))

      minimum_value <- NA_real_

      if (positivity_ok) {
        minimum_value <- min(
          positivity_result
        )
      }

      positivity_status <-
        positivity_ok &&
        minimum_value >= -tolerance

      positivity_message <- ""

      if (inherits(positivity_result, "error")) {

        positivity_message <- conditionMessage(
          positivity_result
        )

      } else if (!positivity_ok) {

        positivity_message <-
          "The positivity test did not return valid finite numeric values."

      } else if (!positivity_status) {

        positivity_message <-
          "A non-negative test function produced a negative operator value."
      }

      checks[[counter]] <- add_check(
        check = paste0(
          "positivity_",
          function_name
        ),
        status = positivity_status,
        value = minimum_value,
        reference = 0,
        error = if (is.finite(minimum_value)) {
          max(
            0,
            -minimum_value
          )
        } else {
          NA_real_
        },
        message = positivity_message
      )

      counter <- counter + 1L
    }
  }

  results <- do.call(
    rbind,
    checks
  )

  rownames(results) <- NULL

  overall <- all(
    results$status
  )

  structure(
    list(
      overall = overall,
      checks = results,
      x = x,
      tolerance = tolerance,
      check_positivity = check_positivity,
      operator = list(
        family = operator$family,
        subfamily = operator$subfamily,
        variant = operator$variant,
        n = operator$n
      )
    ),
    class = "approx_operator_check"
  )
}
