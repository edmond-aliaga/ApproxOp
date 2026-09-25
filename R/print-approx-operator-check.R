#' Print an Approximation Operator Check
#'
#' Prints a compact summary of the diagnostic results produced by
#' \code{check_operator()}.
#'
#' @param x An object of class \code{"approx_operator_check"}.
#' @param ... Additional arguments, currently unused.
#'
#' @return The object \code{x}, invisibly.
#'
#' @export
print.approx_operator_check <- function(x, ...) {

  if (!inherits(
    x,
    "approx_operator_check"
  )) {
    stop(
      "`x` must be an object of class \"approx_operator_check\".",
      call. = FALSE
    )
  }

  cat(
    "<ApproxOp operator diagnostic>\n"
  )

  cat(
    "  Family:     ",
    x$operator$family,
    "\n",
    sep = ""
  )

  if (!is.null(x$operator$subfamily)) {
    cat(
      "  Subfamily:  ",
      x$operator$subfamily,
      "\n",
      sep = ""
    )
  }

  cat(
    "  Variant:    ",
    x$operator$variant,
    "\n",
    sep = ""
  )

  cat(
    "  n:          ",
    x$operator$n,
    "\n",
    sep = ""
  )

  cat(
    "  Overall:    ",
    if (isTRUE(x$overall)) {
      "PASS"
    } else {
      "FAIL"
    },
    "\n\n",
    sep = ""
  )

  checks <- x$checks

  display <- data.frame(
    Check = checks$check,
    Status = ifelse(
      checks$status,
      "PASS",
      "FAIL"
    ),
    stringsAsFactors = FALSE
  )

  print(
    display,
    row.names = FALSE
  )

  failed <- !checks$status

  if (any(failed)) {

    cat(
      "\nFailed checks:\n"
    )

    failed_checks <- checks[
      failed,
      ,
      drop = FALSE
    ]

    for (i in seq_len(
      nrow(failed_checks)
    )) {

      message <- failed_checks$message[i]

      if (is.na(message) ||
          !nzchar(message)) {
        message <- "No additional diagnostic message is available."
      }

      cat(
        "  - ",
        failed_checks$check[i],
        ": ",
        message,
        "\n",
        sep = ""
      )
    }
  }

  cat(
    "\nDiagnostic points: ",
    paste(
      format(
        x$x,
        trim = TRUE
      ),
      collapse = ", "
    ),
    "\n",
    sep = ""
  )

  cat(
    "Tolerance: ",
    format(
      x$tolerance,
      scientific = TRUE
    ),
    "\n",
    sep = ""
  )

  if (isTRUE(
    x$check_positivity
  )) {
    cat(
      "Positivity diagnostics: enabled\n"
    )
  } else {
    cat(
      "Positivity diagnostics: disabled\n"
    )
  }

  invisible(x)
}
