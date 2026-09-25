#' Print an Approximation Error Table
#'
#' Prints an object of class \code{"approx_error_table"} in a
#' structured table with horizontal and vertical separators.
#'
#' @param x An object of class \code{"approx_error_table"}.
#' @param ... Additional arguments, currently unused.
#'
#' @return The object \code{x}, invisibly.
#'
#' @export
print.approx_error_table <- function(x, ...) {

  digits <- attr(x, "digits")

  if (is.null(digits)) {
    digits <- 6L
  }

  tab <- as.data.frame(x)

  formatted <- lapply(
    tab,
    function(column) {
      if (is.numeric(column)) {
        formatC(
          column,
          format = "f",
          digits = digits
        )
      } else {
        as.character(column)
      }
    }
  )

  formatted <- as.data.frame(
    formatted,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  widths <- vapply(
    seq_along(formatted),
    function(j) {
      max(
        nchar(names(formatted)[j]),
        nchar(formatted[[j]])
      )
    },
    numeric(1)
  )

  make_separator <- function() {
    paste0(
      "+",
      paste(
        vapply(
          widths,
          function(w) {
            paste(
              rep("-", w + 2L),
              collapse = ""
            )
          },
          character(1)
        ),
        collapse = "+"
      ),
      "+"
    )
  }

  make_row <- function(values) {

    cells <- vapply(
      seq_along(values),
      function(j) {
        sprintf(
          paste0(
            "%",
            widths[j],
            "s"
          ),
          values[j]
        )
      },
      character(1)
    )

    paste0(
      "| ",
      paste(
        cells,
        collapse = " | "
      ),
      " |"
    )
  }

  separator <- make_separator()

  cat(
    separator,
    "\n",
    sep = ""
  )

  cat(
    make_row(
      names(formatted)
    ),
    "\n",
    sep = ""
  )

  cat(
    separator,
    "\n",
    sep = ""
  )

  for (i in seq_len(nrow(formatted))) {

    cat(
      make_row(
        unlist(
          formatted[i, ],
          use.names = FALSE
        )
      ),
      "\n",
      sep = ""
    )

    cat(
      separator,
      "\n",
      sep = ""
    )
  }

  invisible(x)
}
