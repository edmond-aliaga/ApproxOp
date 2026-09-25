#' Plot an Approximation Parameter Scan
#'
#' Produces a graphical representation of approximation error as a
#' function of a scanned operator parameter.
#'
#' The available error metrics are the maximum absolute error,
#' mean absolute error (MAE), and root mean squared error (RMSE).
#'
#' @param x An object of class \code{"approx_parameter_scan"}.
#' @param metric Error metric to be plotted. One of
#'   \code{"max_absolute_error"}, \code{"mae"}, or \code{"rmse"}.
#' @param type Plot type used to display the parameter scan.
#' @param lwd Optional line width. If \code{NULL}, the ApproxOp
#'   graphical default is used.
#' @param pch Optional plotting symbol. If \code{NULL}, the ApproxOp
#'   graphical default is used.
#' @param show_values Logical. If \code{TRUE}, the scanned parameter
#'   values are displayed explicitly on the horizontal axis.
#' @param grid_lines Logical. If \code{TRUE}, light grid lines are
#'   displayed.
#' @param xlab Optional label for the horizontal axis. If \code{NULL},
#'   the scanned parameter name is used.
#' @param ylab Optional label for the vertical axis.
#' @param main Optional plot title.
#' @param ... Additional graphical arguments passed to
#'   \code{graphics::plot()}.
#'
#' @return The object \code{x}, invisibly.
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
#' Scan <- parameter_scan(
#'   C,
#'   parameter = "a",
#'   values = c(1.5, 2, 3, 5, 10),
#'   f = function(x) x^2,
#'   grid = seq(0, 1, length.out = 101)
#' )
#'
#' plot(Scan)
#' plot(Scan, metric = "mae")
#' }
#'
#' @export
plot.approx_parameter_scan <- function(
    x,
    metric = c(
      "max_absolute_error",
      "mae",
      "rmse"
    ),
    type = "b",
    lwd = NULL,
    pch = NULL,
    show_values = TRUE,
    grid_lines = TRUE,
    xlab = NULL,
    ylab = NULL,
    main = NULL,
    ...
) {

  if (!inherits(
    x,
    "approx_parameter_scan"
  )) {
    stop(
      "`x` must be an object of class \"approx_parameter_scan\".",
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
      is.na(lwd) ||
      !is.finite(lwd) ||
      lwd <= 0) {
    stop(
      "`lwd` must be one positive finite numeric value.",
      call. = FALSE
    )
  }

  if (!is.numeric(pch) ||
      length(pch) != 1L ||
      is.na(pch) ||
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

  if (!is.logical(show_values) ||
      length(show_values) != 1L ||
      is.na(show_values)) {
    stop(
      "`show_values` must be TRUE or FALSE.",
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

  parameter_name <- attr(
    x,
    "parameter"
  )

  if (is.null(parameter_name) ||
      length(parameter_name) != 1L ||
      is.na(parameter_name) ||
      !nzchar(parameter_name)) {

    if ("parameter" %in% names(x) &&
        length(unique(x$parameter)) == 1L) {

      parameter_name <- unique(x$parameter)

    } else {

      parameter_name <- "parameter"
    }
  }

  if (is.null(xlab)) {
    xlab <- paste0(
      "Parameter ",
      parameter_name
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
        paste0(
          "Maximum absolute error versus ",
          parameter_name
        ),
      mae =
        paste0(
          "Mean absolute error versus ",
          parameter_name
        ),
      rmse =
        paste0(
          "Root mean squared error versus ",
          parameter_name
        )
    )
  }

  parameter_values <- x$value
  error_values <- x[[metric]]

  if (anyNA(parameter_values) ||
      any(!is.finite(parameter_values))) {
    stop(
      "The parameter values contain non-finite values.",
      call. = FALSE
    )
  }

  if (anyNA(error_values) ||
      any(!is.finite(error_values))) {
    stop(
      "The selected error metric contains non-finite values.",
      call. = FALSE
    )
  }

  order_index <- order(
    parameter_values
  )

  parameter_values <-
    parameter_values[
      order_index
    ]

  error_values <-
    error_values[
      order_index
    ]

  x_range <- range(
    parameter_values,
    finite = TRUE
  )

  y_range <- range(
    error_values,
    finite = TRUE
  )

  if (diff(x_range) == 0) {

    x_padding <- if (
      x_range[1] == 0
    ) {
      1
    } else {
      abs(x_range[1]) * 0.05
    }

    x_range <- x_range +
      c(
        -x_padding,
        x_padding
      )
  }

  if (diff(y_range) == 0) {

    y_padding <- if (
      y_range[1] == 0
    ) {
      1
    } else {
      abs(y_range[1]) * 0.05
    }

    y_range <- y_range +
      c(
        -y_padding,
        y_padding
      )
  }

  curve_color <- approxop_palette(1L)[1L]

  graphics::plot(
    parameter_values,
    error_values,
    type = "n",
    xaxt = if (show_values) "n" else "s",
    xlim = x_range,
    ylim = y_range,
    xlab = xlab,
    ylab = ylab,
    main = main,
    cex.axis = settings$axis_cex,
    cex.lab = settings$label_cex,
    cex.main = settings$title_cex,
    ...
  )

  if (grid_lines) {
    approxop_add_grid()
  }

  graphics::lines(
    parameter_values,
    error_values,
    type = type,
    col = curve_color,
    lwd = lwd,
    lty = 1,
    pch = pch,
    cex = settings$point_cex
  )

  if (show_values) {

    graphics::axis(
      side = 1,
      at = parameter_values,
      labels = parameter_values,
      cex.axis = settings$axis_cex
    )
  }

  approxop_add_box()

  invisible(x)
}
