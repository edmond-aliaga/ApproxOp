#' Evaluate an Approximation Operator
#'
#' Evaluates a positive linear approximation operator for a given function
#' at one or more points.
#'
#' @param operator An object of class \code{"approx_operator"}.
#' @param f Function to be approximated.
#' @param x Numeric vector of evaluation points.
#'
#' @return A numeric vector containing the approximated values.
#'
#' @export
approximate <- function(operator, f, x) {

  validate_approx_operator(operator)

  if (!is.function(f)) {
    stop(
      "`f` must be a function.",
      call. = FALSE
    )
  }

  if (!is.numeric(x) ||
      length(x) < 1L ||
      anyNA(x)) {
    stop(
      "`x` must be a non-empty numeric vector.",
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

  # Use a registered evaluation engine when one is available.
  if (!is.null(operator$evaluation_engine)) {

    if (!is.function(operator$evaluation_engine)) {
      stop(
        "`evaluation_engine` must be NULL or a function.",
        call. = FALSE
      )
    }

    result <- operator$evaluation_engine(
      f = f,
      x = x,
      operator = operator
    )

    if (!is.numeric(result) ||
        length(result) != length(x) ||
        anyNA(result)) {
      stop(
        paste0(
          "The registered evaluation engine must return ",
          "a numeric vector of length equal to `length(x)` ",
          "without missing values."
        ),
        call. = FALSE
      )
    }

    return(result)
  }

  # Legacy built-in evaluation engines.
  #
  # These branches are retained as a fallback while built-in operators
  # are migrated to the operator implementation registry.

  if (operator$family == "bernstein" &&
      operator$variant == "discrete") {
    return(
      bernstein_evaluate(
        f = f,
        n = operator$n,
        x = x
      )
    )
  }

  if (operator$family == "szasz" &&
      operator$variant == "discrete") {
    return(
      szasz_evaluate(
        f = f,
        x = x,
        operator = operator
      )
    )
  }

  if (operator$family == "baskakov" &&
      operator$variant == "discrete") {
    return(
      baskakov_evaluate(
        f = f,
        x = x,
        operator = operator
      )
    )
  }

  if (operator$family == "sheffer" &&
      is.null(operator$subfamily) &&
      operator$variant == "discrete") {
    return(
      sheffer_evaluate(
        f = f,
        x = x,
        operator = operator
      )
    )
  }

  if (operator$family == "sheffer" &&
      identical(operator$subfamily, "appell") &&
      operator$variant == "discrete") {
    return(
      appell_evaluate(
        f = f,
        x = x,
        operator = operator
      )
    )
  }

  if (operator$family == "sheffer" &&
      identical(operator$subfamily, "charlier") &&
      operator$variant == "discrete") {
    return(
      charlier_evaluate(
        f = f,
        x = x,
        operator = operator
      )
    )
  }

  if (operator$family == "sheffer" &&
      identical(operator$subfamily, "meixner") &&
      operator$variant == "discrete") {
    return(
      meixner_evaluate(
        f = f,
        x = x,
        operator = operator
      )
    )
  }

  operator_name <- operator$family

  if (!is.null(operator$subfamily)) {
    operator_name <- paste0(
      operator_name,
      "/",
      operator$subfamily
    )
  }

  stop(
    paste0(
      "No evaluation engine is available for operator '",
      operator_name,
      "' with variant '",
      operator$variant,
      "'."
    ),
    call. = FALSE
  )
}
