#' Compare Approximation Operators
#'
#' Compares two or more approximation operators numerically for the
#' same function, evaluation grid, and one or more values of n.
#'
#' For each operator and each value of n, the function computes the
#' maximum absolute error, mean absolute error (MAE), and root mean
#' squared error (RMSE).
#'
#' The comparison is performed on a common evaluation grid. Therefore,
#' every point in \code{grid} must belong to the domain of every
#' operator included in the comparison.
#'
#' @param operators A list of objects of class
#'   \code{"approx_operator"}. Named lists are recommended because the
#'   names are used as operator labels in the returned table.
#' @param f Function to be approximated.
#' @param grid Numeric vector of evaluation points.
#' @param n Optional vector of positive integers. If \code{NULL}, each
#'   operator is evaluated using its own stored value of \code{n}.
#'
#' @return An object of class \code{"approx_operator_comparison"} and
#'   \code{"data.frame"}. The returned data frame contains the operator
#'   label, family, subfamily, variant, n, maximum absolute error, MAE,
#'   and RMSE.
#'
#' @export
compare_operators <- function(
    operators,
    f,
    grid,
    n = NULL
) {

  if (!is.list(operators) ||
      length(operators) < 2L) {
    stop(
      "`operators` must be a list containing at least two operators.",
      call. = FALSE
    )
  }

  valid_operators <- vapply(
    operators,
    function(op) {
      inherits(
        op,
        "approx_operator"
      )
    },
    logical(1)
  )

  if (!all(valid_operators)) {
    stop(
      "Every element of `operators` must be an object of class \"approx_operator\".",
      call. = FALSE
    )
  }

  for (op in operators) {
    validate_approx_operator(op)
  }

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

  if (!is.null(n)) {

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
  }

  for (i in seq_along(operators)) {

    op <- operators[[i]]

    if (any(
      grid < op$domain[1] |
      grid > op$domain[2]
    )) {

      stop(
        paste0(
          "All values in `grid` must belong to the domain of every operator. ",
          "The grid is not valid for operator ",
          i,
          "."
        ),
        call. = FALSE
      )
    }
  }

  operator_names <- names(operators)

  if (is.null(operator_names)) {
    operator_names <- rep(
      "",
      length(operators)
    )
  }

  missing_names <-
    is.na(operator_names) |
    operator_names == ""

  if (any(missing_names)) {

    generated_names <- vapply(
      seq_along(operators),
      function(i) {

        op <- operators[[i]]

        if (!is.null(op$subfamily)) {
          paste0(
            op$family,
            "_",
            op$subfamily
          )
        } else {
          op$family
        }
      },
      character(1)
    )

    operator_names[missing_names] <-
      generated_names[missing_names]
  }

  operator_names <- make.unique(
    operator_names,
    sep = "_"
  )

  results <- vector(
    "list",
    length(operators)
  )

  for (i in seq_along(operators)) {

    op <- operators[[i]]

    current_n <- n

    if (is.null(current_n)) {
      current_n <- op$n
    }

    conv <- convergence(
      operator = op,
      f = f,
      grid = grid,
      n = current_n
    )

    subfamily_value <-
      if (is.null(op$subfamily)) {
        NA_character_
      } else {
        op$subfamily
      }

    results[[i]] <- data.frame(
      operator = rep(
        operator_names[i],
        nrow(conv)
      ),
      family = rep(
        op$family,
        nrow(conv)
      ),
      subfamily = rep(
        subfamily_value,
        nrow(conv)
      ),
      variant = rep(
        op$variant,
        nrow(conv)
      ),
      n = conv$n,
      max_absolute_error =
        conv$max_absolute_error,
      mae =
        conv$mae,
      rmse =
        conv$rmse,
      stringsAsFactors = FALSE
    )
  }

  result <- do.call(
    rbind,
    results
  )

  rownames(result) <- NULL

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
    "operator_count"
  ) <- length(operators)

  attr(
    result,
    "operator_names"
  ) <- operator_names

  attr(
    result,
    "n_values"
  ) <- if (is.null(n)) {
    vapply(
      operators,
      function(op) op$n,
      numeric(1)
    )
  } else {
    n
  }

  class(result) <- c(
    "approx_operator_comparison",
    "data.frame"
  )

  result
}
