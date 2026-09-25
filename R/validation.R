# Internal validation for approximation operators

validate_approx_operator <- function(x) {

  if (!inherits(x, "approx_operator")) {
    stop(
      "`x` must be an object of class 'approx_operator'.",
      call. = FALSE
    )
  }

  if (!is.character(x$family) || length(x$family) != 1L ||
      is.na(x$family) || !nzchar(x$family)) {
    stop(
      "`family` must be a single non-empty character string.",
      call. = FALSE
    )
  }

  if (!is.null(x$subfamily)) {
    if (!is.character(x$subfamily) ||
        length(x$subfamily) != 1L ||
        is.na(x$subfamily) ||
        !nzchar(x$subfamily)) {
      stop(
        "`subfamily` must be NULL or a single non-empty character string.",
        call. = FALSE
      )
    }
  }

  if (!is.numeric(x$n) || length(x$n) != 1L ||
      is.na(x$n) || !is.finite(x$n) ||
      x$n <= 0 || x$n != floor(x$n)) {
    stop(
      "`n` must be a positive integer.",
      call. = FALSE
    )
  }

  if (!is.character(x$variant) || length(x$variant) != 1L ||
      is.na(x$variant) || !nzchar(x$variant)) {
    stop(
      "`variant` must be a single non-empty character string.",
      call. = FALSE
    )
  }

  if (!is.null(x$domain)) {
    if (!is.numeric(x$domain) || length(x$domain) != 2L ||
        anyNA(x$domain) || x$domain[1] >= x$domain[2]) {
      stop(
        "`domain` must be a numeric vector c(lower, upper) with lower < upper.",
        call. = FALSE
      )
    }
  }

  if (!is.list(x$parameters)) {
    stop(
      "`parameters` must be a list.",
      call. = FALSE
    )
  }

  if (!is.list(x$control)) {
    stop(
      "`control` must be a list.",
      call. = FALSE
    )
  }

  if (!is.list(x$metadata)) {
    stop(
      "`metadata` must be a list.",
      call. = FALSE
    )
  }

  invisible(x)
}
