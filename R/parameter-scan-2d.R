#' Scan Two Operator Parameters
#'
#' Evaluates the approximation performance of a parametric
#' approximation operator over all combinations of two parameter
#' sequences.
#'
#' For each pair of parameter values, the operator is reconstructed
#' and the approximation error is evaluated on the supplied grid.
#' The function reports the maximum absolute error, mean absolute
#' error (MAE), and root mean squared error (RMSE).
#'
#' Invalid parameter combinations do not interrupt the complete scan.
#' They are retained in the result with \code{valid = FALSE},
#' missing error measures, and an explanatory message.
#'
#' @param operator An object of class \code{"approx_operator"}.
#' @param parameter1 Character string giving the name of the first
#'   parameter to be varied.
#' @param values1 Numeric vector containing the values of the first
#'   parameter.
#' @param parameter2 Character string giving the name of the second
#'   parameter to be varied.
#' @param values2 Numeric vector containing the values of the second
#'   parameter.
#' @param f Function to be approximated.
#' @param grid Numeric vector of evaluation points.
#' @param n Optional positive integer giving the operator degree.
#'   If \code{NULL}, the value stored in \code{operator} is used.
#'
#' @return A data frame of class
#'   \code{c("approx_parameter_scan_2d", "data.frame")} containing
#'   all parameter combinations, validity information, and the
#'   corresponding error measures.
#'
#' @examples
#' \dontrun{
#' M <- approx_operator(
#'   family = "sheffer",
#'   subfamily = "meixner",
#'   n = 20,
#'   params = list(
#'     alpha = 0.5,
#'     beta = 2,
#'     theta = 1,
#'     c = 0.75
#'   )
#' )
#'
#' parameter_scan_2d(
#'   M,
#'   parameter1 = "alpha",
#'   values1 = c(0.5, 1, 2),
#'   parameter2 = "beta",
#'   values2 = c(1, 2, 4),
#'   f = function(x) x^2,
#'   grid = seq(0, 1, length.out = 101)
#' )
#' }
#'
#' @export
parameter_scan_2d <- function(
    operator,
    parameter1,
    values1,
    parameter2,
    values2,
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

  if (!is.character(parameter1) ||
      length(parameter1) != 1L ||
      is.na(parameter1) ||
      !nzchar(parameter1)) {
    stop(
      "`parameter1` must be one non-empty character string.",
      call. = FALSE
    )
  }

  if (!is.character(parameter2) ||
      length(parameter2) != 1L ||
      is.na(parameter2) ||
      !nzchar(parameter2)) {
    stop(
      "`parameter2` must be one non-empty character string.",
      call. = FALSE
    )
  }

  if (identical(
    parameter1,
    parameter2
  )) {
    stop(
      "`parameter1` and `parameter2` must be different.",
      call. = FALSE
    )
  }

  if (!is.numeric(values1) ||
      length(values1) == 0L ||
      anyNA(values1) ||
      any(!is.finite(values1))) {
    stop(
      "`values1` must be a non-empty finite numeric vector.",
      call. = FALSE
    )
  }

  if (!is.numeric(values2) ||
      length(values2) == 0L ||
      anyNA(values2) ||
      any(!is.finite(values2))) {
    stop(
      "`values2` must be a non-empty finite numeric vector.",
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

  if (!is.numeric(n_value) ||
      length(n_value) != 1L ||
      is.na(n_value) ||
      !is.finite(n_value) ||
      n_value <= 0 ||
      n_value != floor(n_value)) {
    stop(
      "The operator degree must be one positive integer.",
      call. = FALSE
    )
  }

  n_value <- as.integer(n_value)

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
      !(parameter1 %in% names(base_params))) {
    stop(
      paste0(
        "Parameter `",
        parameter1,
        "` is not present in the operator parameter list."
      ),
      call. = FALSE
    )
  }

  if (is.null(names(base_params)) ||
      !(parameter2 %in% names(base_params))) {
    stop(
      paste0(
        "Parameter `",
        parameter2,
        "` is not present in the operator parameter list."
      ),
      call. = FALSE
    )
  }

  combinations <- expand.grid(
    value1 = values1,
    value2 = values2,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  number_of_combinations <-
    nrow(combinations)

  results <- vector(
    mode = "list",
    length = number_of_combinations
  )

  for (i in seq_len(
    number_of_combinations
  )) {

    current_params <- base_params

    current_params[[parameter1]] <-
      combinations$value1[i]

    current_params[[parameter2]] <-
      combinations$value2[i]

    evaluation <- tryCatch(
      {

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

        list(
          valid = TRUE,
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
          message = NA_character_
        )
      },
      error = function(e) {

        list(
          valid = FALSE,
          max_absolute_error = NA_real_,
          mae = NA_real_,
          rmse = NA_real_,
          message = conditionMessage(e)
        )
      }
    )

    results[[i]] <- data.frame(
      parameter1 = parameter1,
      value1 = combinations$value1[i],
      parameter2 = parameter2,
      value2 = combinations$value2[i],
      n = n_value,
      valid = evaluation$valid,
      max_absolute_error =
        evaluation$max_absolute_error,
      mae =
        evaluation$mae,
      rmse =
        evaluation$rmse,
      message =
        evaluation$message,
      stringsAsFactors = FALSE
    )
  }

  result <- do.call(
    rbind,
    results
  )

  rownames(result) <- NULL

  class(result) <- c(
    "approx_parameter_scan_2d",
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
    "parameter1"
  ) <- parameter1

  attr(
    result,
    "parameter2"
  ) <- parameter2

  attr(
    result,
    "values1"
  ) <- values1

  attr(
    result,
    "values2"
  ) <- values2

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

  attr(
    result,
    "combination_count"
  ) <- number_of_combinations

  attr(
    result,
    "valid_count"
  ) <- sum(
    result$valid
  )

  attr(
    result,
    "invalid_count"
  ) <- sum(
    !result$valid
  )

  result
}
