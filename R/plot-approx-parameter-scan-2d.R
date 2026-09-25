#' Plot a Two-Dimensional Parameter Scan
#'
#' Produces a contour plot for the results obtained from
#' \code{parameter_scan_2d()}. The plot shows how a selected
#' approximation error metric changes with respect to two
#' operator parameters.
#'
#' Invalid parameter combinations are represented by missing
#' values and are therefore excluded from the contour surface.
#'
#' @param x An object of class \code{"approx_parameter_scan_2d"}.
#' @param metric Error metric to be plotted. One of
#'   \code{"max_absolute_error"}, \code{"mae"}, or \code{"rmse"}.
#' @param levels Number of contour levels, or a numeric vector
#'   specifying the contour levels.
#' @param draw_points Logical. If \code{TRUE}, valid parameter
#'   combinations are displayed as points.
#' @param show_invalid Logical. If \code{TRUE}, invalid parameter
#'   combinations are displayed using a different plotting symbol.
#' @param pch Optional plotting symbol for valid parameter
#'   combinations. If \code{NULL}, the ApproxOp graphical default
#'   is used.
#' @param invalid_pch Plotting symbol for invalid parameter
#'   combinations.
#' @param lwd Optional contour line width. If \code{NULL}, the
#'   ApproxOp graphical default is used.
#' @param grid_lines Logical. If \code{TRUE}, light grid lines are
#'   displayed.
#' @param xlab Optional label for the horizontal axis.
#' @param ylab Optional label for the vertical axis.
#' @param main Optional plot title.
#' @param ... Additional graphical arguments passed to
#'   \code{graphics::contour()}.
#'
#' @return The object \code{x}, invisibly.
#'
#' @export
plot.approx_parameter_scan_2d <- function(
    x,
    metric = c(
      "max_absolute_error",
      "mae",
      "rmse"
    ),
    levels = 10,
    draw_points = TRUE,
    show_invalid = TRUE,
    pch = NULL,
    invalid_pch = 4,
    lwd = NULL,
    grid_lines = TRUE,
    xlab = NULL,
    ylab = NULL,
    main = NULL,
    ...
) {

  if (!inherits(
    x,
    "approx_parameter_scan_2d"
  )) {
    stop(
      "`x` must be an object of class \"approx_parameter_scan_2d\".",
      call. = FALSE
    )
  }

  metric <- match.arg(metric)

  settings <- approxop_graphics_settings()

  if (is.null(pch)) {
    pch <- approxop_point_types(1L)[1L]
  }

  if (is.null(lwd)) {
    lwd <- settings$approximation_lwd
  }

  if (!is.logical(draw_points) ||
      length(draw_points) != 1L ||
      is.na(draw_points)) {
    stop(
      "`draw_points` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (!is.logical(show_invalid) ||
      length(show_invalid) != 1L ||
      is.na(show_invalid)) {
    stop(
      "`show_invalid` must be TRUE or FALSE.",
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

  if (!is.numeric(invalid_pch) ||
      length(invalid_pch) != 1L ||
      !is.finite(invalid_pch)) {
    stop(
      "`invalid_pch` must be one finite numeric value.",
      call. = FALSE
    )
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

  if (!is.logical(grid_lines) ||
      length(grid_lines) != 1L ||
      is.na(grid_lines)) {
    stop(
      "`grid_lines` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (!is.numeric(levels) ||
      length(levels) < 1L ||
      anyNA(levels) ||
      any(!is.finite(levels))) {
    stop(
      "`levels` must contain finite numeric values.",
      call. = FALSE
    )
  }

  if (length(levels) == 1L) {

    if (levels <= 0 ||
        levels != as.integer(levels)) {
      stop(
        "When `levels` has length one, it must be one positive integer.",
        call. = FALSE
      )
    }
  }

  required_columns <- c(
    "parameter1",
    "value1",
    "parameter2",
    "value2",
    "valid",
    metric
  )

  missing_columns <- setdiff(
    required_columns,
    names(x)
  )

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "The parameter scan object is missing required columns: ",
        paste(
          missing_columns,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }

  parameter1_names <- unique(
    x$parameter1
  )

  parameter2_names <- unique(
    x$parameter2
  )

  if (length(parameter1_names) != 1L ||
      length(parameter2_names) != 1L) {
    stop(
      "The parameter scan object must contain exactly two parameter names.",
      call. = FALSE
    )
  }

  parameter1 <- parameter1_names[1L]
  parameter2 <- parameter2_names[1L]

  values1 <- sort(
    unique(
      x$value1
    )
  )

  values2 <- sort(
    unique(
      x$value2
    )
  )

  if (length(values1) < 2L ||
      length(values2) < 2L) {
    stop(
      paste0(
        "At least two values are required for each parameter ",
        "to create a contour plot."
      ),
      call. = FALSE
    )
  }

  z <- matrix(
    NA_real_,
    nrow = length(values1),
    ncol = length(values2),
    dimnames = list(
      as.character(values1),
      as.character(values2)
    )
  )

  for (i in seq_len(nrow(x))) {

    if (isTRUE(x$valid[i]) &&
        is.finite(x[[metric]][i])) {

      row_index <- match(
        x$value1[i],
        values1
      )

      column_index <- match(
        x$value2[i],
        values2
      )

      z[
        row_index,
        column_index
      ] <- x[[metric]][i]
    }
  }

  finite_values <- z[
    is.finite(z)
  ]

  if (length(finite_values) == 0L) {
    stop(
      paste0(
        "No valid finite values are available for the selected ",
        "error metric."
      ),
      call. = FALSE
    )
  }

  if (length(unique(finite_values)) < 2L) {
    stop(
      paste0(
        "At least two distinct finite metric values are required ",
        "to create a contour plot."
      ),
      call. = FALSE
    )
  }

  if (is.null(xlab)) {
    xlab <- paste0(
      "Parameter ",
      parameter1
    )
  }

  if (is.null(ylab)) {
    ylab <- paste0(
      "Parameter ",
      parameter2
    )
  }

  if (is.null(main)) {

    metric_label <- switch(
      metric,
      max_absolute_error =
        "Maximum absolute error",
      mae =
        "Mean absolute error (MAE)",
      rmse =
        "Root mean squared error (RMSE)"
    )

    main <- paste0(
      metric_label,
      ": ",
      parameter1,
      " versus ",
      parameter2
    )
  }

  if (length(levels) == 1L) {

    contour_levels <- pretty(
      range(
        finite_values,
        finite = TRUE
      ),
      n = levels
    )

  } else {

    contour_levels <- sort(
      unique(levels)
    )
  }

  number_of_levels <- length(
    contour_levels
  )

  contour_colors <- approxop_palette(
    max(
      number_of_levels,
      1L
    )
  )

  graphics::plot(
    NA,
    NA,
    xlim = range(
      values1,
      finite = TRUE
    ),
    ylim = range(
      values2,
      finite = TRUE
    ),
    xlab = xlab,
    ylab = ylab,
    main = main,
    type = "n",
    cex.axis = settings$axis_cex,
    cex.lab = settings$label_cex,
    cex.main = settings$title_cex
  )

  if (grid_lines) {
    approxop_add_grid()
  }

  graphics::contour(
    x = values1,
    y = values2,
    z = z,
    levels = contour_levels,
    add = TRUE,
    drawlabels = TRUE,
    col = contour_colors,
    lwd = lwd,
    ...
  )

  if (draw_points) {

    valid_rows <-
      !is.na(x$valid) &
      x$valid &
      is.finite(x[[metric]])

    if (any(valid_rows)) {

      graphics::points(
        x$value1[valid_rows],
        x$value2[valid_rows],
        pch = pch,
        col = approxop_palette(1L)[1L],
        cex = settings$point_cex
      )
    }
  }

  if (show_invalid) {

    invalid_rows <-
      is.na(x$valid) |
      !x$valid |
      !is.finite(x[[metric]])

    if (any(invalid_rows)) {

      graphics::points(
        x$value1[invalid_rows],
        x$value2[invalid_rows],
        pch = invalid_pch,
        col = settings$invalid_color,
        cex = settings$point_cex
      )
    }
  }

  approxop_add_box()

  invisible(x)
}
