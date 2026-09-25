#' Print an Approximation Operator
#'
#' Prints a concise summary of an approximation operator object.
#'
#' @param x An object of class \code{"approx_operator"}.
#' @param ... Additional arguments, currently unused.
#'
#' @return The operator object, invisibly.
#'
#' @export
print.approx_operator <- function(x, ...) {

  validate_approx_operator(x)

  cat("<ApproxOp approximation operator>\n")

  family_name <- x$family

  if (!is.null(x$metadata$display_name) &&
      is.character(x$metadata$display_name) &&
      length(x$metadata$display_name) == 1L) {
    family_name <- x$metadata$display_name
  }

  cat("  Family:     ", family_name, "\n", sep = "")

  if (!is.null(x$subfamily)) {
    cat(
      "  Subfamily:  ",
      tools::toTitleCase(x$subfamily),
      "\n",
      sep = ""
    )
  }

  cat("  Variant:    ", x$variant, "\n", sep = "")
  cat("  n:          ", x$n, "\n", sep = "")

  if (!is.null(x$domain)) {

    lower <- if (is.infinite(x$domain[1])) {
      if (x$domain[1] < 0) "-Inf" else "Inf"
    } else {
      as.character(x$domain[1])
    }

    upper <- if (is.infinite(x$domain[2])) {
      if (x$domain[2] < 0) "-Inf" else "Inf"
    } else {
      as.character(x$domain[2])
    }

    cat(
      "  Domain:     [",
      lower,
      ", ",
      upper,
      "]\n",
      sep = ""
    )
  }

  if (length(x$parameters) > 0L) {

    cat("  Parameters:\n")

    parameter_names <- names(x$parameters)

    if (is.null(parameter_names)) {
      parameter_names <- rep("", length(x$parameters))
    }

    for (i in seq_along(x$parameters)) {

      value <- x$parameters[[i]]
      name <- parameter_names[i]

      if (length(value) == 1L &&
          (is.numeric(value) ||
           is.character(value) ||
           is.logical(value))) {

        if (nzchar(name)) {
          cat(
            "    ",
            name,
            " = ",
            as.character(value),
            "\n",
            sep = ""
          )
        } else {
          cat(
            "    ",
            as.character(value),
            "\n",
            sep = ""
          )
        }
      }
    }
  }

  invisible(x)
}
