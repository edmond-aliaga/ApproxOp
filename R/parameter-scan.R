#' Scan an Operator Parameter
#'
#' Evaluates the approximation performance of a parametric
#' approximation operator over a sequence of values of one parameter.
#'
#' For each parameter value, the operator is reconstructed and the
#' approximation error is evaluated on the supplied grid. The function
#' reports the maximum absolute error, mean absolute error (MAE), and
#' root mean squared error (RMSE).
#'
#' @param operator An object of class \code{"approx_operator"}.
#' @param parameter Character string giving the name of the parameter
#'   to be varied.
#' @param values Numeric vector containing the parameter values to scan.
#' @param f Function to be approximated.
#' @param grid Numeric vector of evaluation points.
#' @param n Optional positive integer giving the operator degree.
#'   If \code{NULL}, the value stored in \code{operator} is used.
#'
#' @return A data frame of class
#'   \code{c("approx_parameter_scan", "data.frame")} containing the
#'   parameter values and the corresponding error measures.
#'
#' @examples
#' \dontrun{
#' C <- approx_operator(
#'   family = "sheffer",
#'   subfamily = "charlier",
#'   n = 20,
#'   params = list(a = 2)
#' )
#'
#' parameter_scan(
#'   C,
#'   parameter = "a",
#'   values = c(1.5, 2, 3, 5),
#'   f = function(x) x^2,
#'   grid = seq(0, 1, length.out = 101)
#' )
#' }
#'
#' @export
parameter_scan <- function(
    operator,
    parameter,
    values,
    f,
    grid,
    n = NULL
) {

  if (!inherits(
    operator,
    "approx_operator"
  )) {
    stop(
      "`operator` must be an object of class \"approx_operator\".",
      call. = FALSE
    )
  }

  if (!is.character(parameter) ||
      length(parameter) != 1L ||
      is.na(parameter) ||
      !nzchar(parameter)) {
    stop(
      "`parameter` must be one non-empty character string.",
      call. = FALSE
    )
  }

  if (!is.numeric(values) ||
      length(values) == 0L ||
      anyNA(values) ||
      any(!is.finite(values))) {
    stop(
      "`values` must be a non-empty finite numeric vector.",
      call. = FALSE
    )
  }

  if (!is.function(f)) {
    stop(
      "`f` must be a function.",
      call. = FALSE
    )
  }

  if (!is.numeric(grid) ||
      length(grid) == 0L ||
      anyNA(grid) ||
      any(!is.finite(grid))) {
    stop(
      "`grid` must be a non-empty finite numeric vector.",
      call. = FALSE
    )
  }

  if (is.null(n)) {

    n_value <- operator$n

  } else {

    if (!is.numeric(n) ||
        length(n) != 1L ||
        is.na(n) ||
        !is.finite(n) ||
        n <= 0 ||
        n != floor(n)) {
      stop(
        "`n` must be one positive integer.",
        call. = FALSE
      )
    }

    n_value <- as.integer(n)
  }

  if (is.null(operator$parameters)) {

    base_params <- list()

  } else {

    base_params <- operator$parameters
  }

  if (!is.list(base_params)) {
    stop(
      "The operator parameters must be stored as a list.",
      call. = FALSE
    )
  }

  if (is.null(names(base_params)) ||
      !(parameter %in% names(base_params))) {
    stop(
      paste0(
        "Parameter `",
        parameter,
        "` is not present in the operator parameter list."
      ),
      call. = FALSE
    )
  }

  results <- vector(
    mode = "list",
    length = length(values)
  )

  for (i in seq_along(values)) {

    current_params <- base_params

    current_params[[parameter]] <-
      values[i]

    current_operator <- approx_operator(
      family = operator$family,
      subfamily = operator$subfamily,
      variant = operator$variant,
      n = n_value,
      params = current_params
    )

    current_error <- approx_error(
      current_operator,
      f = f,
      x = grid
    )

    results[[i]] <- data.frame(
      parameter = parameter,
      value = values[i],
      n = n_value,
      max_absolute_error =
        attr(
          current_error,
          "max_absolute_error"
        ),
      mae =
        attr(
          current_error,
          "mae"
        ),
      rmse =
        attr(
          current_error,
          "rmse"
        ),
      stringsAsFactors = FALSE
    )
  }

  result <- do.call(
    rbind,
    results
  )

  rownames(result) <- NULL

  class(result) <- c(
    "approx_parameter_scan",
    "data.frame"
  )

  attr(
    result,
    "family"
  ) <- operator$family

  attr(
    result,
    "subfamily"
  ) <- operator$subfamily

  attr(
    result,
    "variant"
  ) <- operator$variant

  attr(
    result,
    "parameter"
  ) <- parameter

  attr(
    result,
    "values"
  ) <- values

  attr(
    result,
    "n"
  ) <- n_value

  attr(
    result,
    "grid_size"
  ) <- length(grid)

  attr(
    result,
    "grid_range"
  ) <- range(grid)

  result
}
