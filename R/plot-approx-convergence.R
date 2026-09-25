#' Plot Numerical Convergence
#'
#' Produces a graphical representation of the numerical convergence
#' measures computed by \code{convergence()}.
#'
#' The horizontal axis represents the parameter n, while the vertical
#' axis represents one of the available error measures: maximum
#' absolute error, mean absolute error (MAE), or root mean squared
#' error (RMSE).
#'
#' @param x An object of class \code{"approx_convergence"}.
#' @param metric Error measure to be plotted. One of
#'   \code{"max_absolute_error"}, \code{"mae"}, or \code{"rmse"}.
#' @param xlab Label for the horizontal axis.
#' @param ylab Optional label for the vertical axis.
#' @param main Optional plot title.
#' @param type Plot type passed to \code{graphics::plot()}.
#' @param lwd Line width.
#' @param pch Plotting symbol.
#' @param show_n Logical. If \code{TRUE}, the horizontal axis displays
#'   the actual values of n used in the convergence study.
#' @param grid_lines Logical value indicating whether light grid lines
#'   should be displayed.
#' @param ... Additional graphical arguments passed to
#'   \code{graphics::plot()}.
#'
#' @return The convergence object \code{x}, returned invisibly.
#'
#' @export
plot.approx_convergence <- function(
    x,
    metric = c(
      "max_absolute_error",
      "mae",
      "rmse"
    ),
    xlab = "n",
    ylab = NULL,
    main = NULL,
    type = "b",
    lwd = NULL,
    pch = NULL,
    show_n = TRUE,
    grid_lines = TRUE,
    ...
) {

  if (!inherits(
    x,
    "approx_convergence"
  )) {
    stop(
      "`x` must be an object of class \"approx_convergence\".",
      call. = FALSE
    )
  }

  metric <- match.arg(metric)

  settings <- approxop_graphics_settings()

  if (is.null(lwd)) {
    lwd <- settings$approximation_lwd
  }

  if (is.null(pch)) {
    pch <- approxop_point_types(1L)[1L]
  }

  if (!is.numeric(lwd) ||
      length(lwd) != 1L ||
      !is.finite(lwd) ||
      lwd <= 0) {
    stop(
      "`lwd` must be one positive finite numeric value.",
      call. = FALSE
    )
  }

  if (!is.numeric(pch) ||
      length(pch) != 1L ||
      !is.finite(pch)) {
    stop(
      "`pch` must be NULL or one finite numeric value.",
      call. = FALSE
    )
  }

  if (!is.character(type) ||
      length(type) != 1L ||
      is.na(type)) {
    stop(
      "`type` must be a single character value.",
      call. = FALSE
    )
  }

  if (!is.logical(show_n) ||
      length(show_n) != 1L ||
      is.na(show_n)) {
    stop(
      "`show_n` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (!is.logical(grid_lines) ||
      length(grid_lines) != 1L ||
      is.na(grid_lines)) {
    stop(
      "`grid_lines` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  y_values <- x[[metric]]

  if (anyNA(y_values) ||
      any(!is.finite(y_values))) {
    stop(
      "The selected error metric contains non-finite values.",
      call. = FALSE
    )
  }

  if (is.null(ylab)) {

    ylab <- switch(
      metric,
      max_absolute_error =
        "Maximum absolute error",
      mae =
        "Mean absolute error (MAE)",
      rmse =
        "Root mean squared error (RMSE)"
    )
  }

  if (is.null(main)) {

    main <- switch(
      metric,
      max_absolute_error =
        "Maximum absolute error versus n",
      mae =
        "Mean absolute error versus n",
      rmse =
        "Root mean squared error versus n"
    )
  }

  curve_color <- approxop_palette(1L)[1L]

  graphics::plot(
    x$n,
    y_values,
    type = "n",
    xlab = xlab,
    ylab = ylab,
    main = main,
    xaxt = if (show_n) "n" else "s",
    cex.axis = settings$axis_cex,
    cex.lab = settings$label_cex,
    cex.main = settings$title_cex,
    ...
  )

  if (grid_lines) {
    approxop_add_grid()
  }

  graphics::lines(
    x$n,
    y_values,
    type = type,
    col = curve_color,
    lwd = lwd,
    lty = 1,
    pch = pch,
    cex = settings$point_cex
  )

  if (show_n) {

    graphics::axis(
      side = 1,
      at = x$n,
      labels = x$n,
      cex.axis = settings$axis_cex
    )
  }

  approxop_add_box()

  invisible(x)
}
