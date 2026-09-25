#' Plot an Approximation Operator
#'
#' Produces graphical representations of the approximation of a
#' function by an approximation operator.
#'
#' Two plot types are currently supported:
#'
#' \itemize{
#'   \item \code{"approximation"} displays the exact function together
#'   with the operator approximations for one or more values of n.
#'   \item \code{"error"} displays the absolute approximation error
#'   for one or more values of n.
#' }
#'
#' @param x An object of class \code{"approx_operator"}.
#' @param f Function to be approximated.
#' @param grid Numeric vector of evaluation points.
#' @param n Optional vector of positive integers. If \code{NULL},
#'   the value of \code{n} stored in the operator is used.
#' @param type Type of plot. One of \code{"approximation"} or
#'   \code{"error"}.
#' @param xlab Label for the horizontal axis.
#' @param ylab Optional label for the vertical axis.
#' @param main Optional plot title.
#' @param lwd Line width used for the approximation curves.
#' @param legend_position Position of the legend. The default
#'   \code{"best"} places the legend automatically in a low-density
#'   region of the graph. The value \code{"right"} places the legend
#'   outside the plotting region. Standard base R legend positions
#'   are also supported.
#' @param legend_title Optional title for the legend.
#' @param grid_lines Logical value indicating whether light grid lines
#'   should be displayed.
#' @param ... Additional graphical arguments passed to
#'   \code{graphics::plot()}.
#'
#' @return The approximation data used to construct the plot,
#'   returned invisibly.
#'
#' @export
plot.approx_operator <- function(
    x,
    f,
    grid,
    n = NULL,
    type = c("approximation", "error"),
    xlab = "x",
    ylab = NULL,
    main = NULL,
    lwd = NULL,
    legend_position = "best",
    legend_title = NULL,
    grid_lines = TRUE,
    ...
) {

  validate_approx_operator(x)

  if (missing(f) || !is.function(f)) {
    stop(
      "`f` must be a function.",
      call. = FALSE
    )
  }

  if (missing(grid)) {
    stop(
      "`grid` must be supplied.",
      call. = FALSE
    )
  }

  if (!is.character(legend_position) ||
      length(legend_position) != 1L ||
      is.na(legend_position)) {
    stop(
      "`legend_position` must be a single character value.",
      call. = FALSE
    )
  }

  if (!is.null(legend_title) &&
      (!is.character(legend_title) ||
       length(legend_title) != 1L ||
       is.na(legend_title))) {
    stop(
      "`legend_title` must be NULL or a single character value.",
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

  settings <- approxop_graphics_settings()

  if (is.null(lwd)) {
    lwd <- settings$approximation_lwd
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

  type <- match.arg(type)

  data <- approximation_data(
    operator = x,
    f = f,
    x = grid,
    n = n
  )

  n_values <- attr(
    data,
    "n_values"
  )

  number_of_curves <- length(n_values)

  curve_colors <-
    approxop_palette(
      number_of_curves
    )

  curve_line_types <-
    approxop_line_types(
      number_of_curves
    )

  old_par <- graphics::par(
    no.readonly = TRUE
  )

  on.exit(
    graphics::par(old_par),
    add = TRUE
  )

  outside_legend <- identical(
    legend_position,
    "right"
  )

  if (outside_legend) {

    current_mar <- graphics::par("mar")

    graphics::par(
      mar = c(
        current_mar[1],
        current_mar[2],
        current_mar[3],
        max(
          current_mar[4],
          8
        )
      ),
      xpd = NA
    )
  }

  if (type == "approximation") {

    if (is.null(ylab)) {
      ylab <- "Function value"
    }

    exact_data <- data[
      data$n == n_values[1L],
      ,
      drop = FALSE
    ]

    y_range <- range(
      c(
        data$exact,
        data$approximation
      ),
      finite = TRUE
    )

    graphics::plot(
      exact_data$x,
      exact_data$exact,
      type = "l",
      col = settings$exact_color,
      lwd = settings$exact_lwd,
      lty = 1,
      xlab = xlab,
      ylab = ylab,
      main = main,
      ylim = y_range,
      cex.axis = settings$axis_cex,
      cex.lab = settings$label_cex,
      cex.main = settings$title_cex,
      ...
    )

    if (grid_lines) {
      approxop_add_grid()
    }

    graphics::lines(
      exact_data$x,
      exact_data$exact,
      col = settings$exact_color,
      lwd = settings$exact_lwd,
      lty = 1
    )

    legend_x <- exact_data$x
    legend_y <- exact_data$exact

    for (j in seq_along(n_values)) {

      current <- data[
        data$n == n_values[j],
        ,
        drop = FALSE
      ]

      graphics::lines(
        current$x,
        current$approximation,
        col = curve_colors[j],
        lwd = lwd,
        lty = curve_line_types[j]
      )

      legend_x <- c(
        legend_x,
        current$x
      )

      legend_y <- c(
        legend_y,
        current$approximation
      )
    }

    approxop_add_box()

    legend_labels <- c(
      "f(x)",
      paste0(
        "n = ",
        n_values
      )
    )

    legend_colors <- c(
      settings$exact_color,
      curve_colors
    )

    legend_line_types <- c(
      1,
      curve_line_types
    )

    legend_line_widths <- c(
      settings$exact_lwd,
      rep(
        lwd,
        number_of_curves
      )
    )

  } else {

    if (is.null(ylab)) {
      ylab <- "Absolute error"
    }

    y_range <- range(
      data$absolute_error,
      finite = TRUE
    )

    first <- data[
      data$n == n_values[1L],
      ,
      drop = FALSE
    ]

    graphics::plot(
      first$x,
      first$absolute_error,
      type = "l",
      col = curve_colors[1L],
      lwd = lwd,
      lty = curve_line_types[1L],
      xlab = xlab,
      ylab = ylab,
      main = main,
      ylim = y_range,
      cex.axis = settings$axis_cex,
      cex.lab = settings$label_cex,
      cex.main = settings$title_cex,
      ...
    )

    if (grid_lines) {
      approxop_add_grid()
    }

    graphics::lines(
      first$x,
      first$absolute_error,
      col = curve_colors[1L],
      lwd = lwd,
      lty = curve_line_types[1L]
    )

    legend_x <- first$x
    legend_y <- first$absolute_error

    if (length(n_values) > 1L) {

      for (j in 2:length(n_values)) {

        current <- data[
          data$n == n_values[j],
          ,
          drop = FALSE
        ]

        graphics::lines(
          current$x,
          current$absolute_error,
          col = curve_colors[j],
          lwd = lwd,
          lty = curve_line_types[j]
        )

        legend_x <- c(
          legend_x,
          current$x
        )

        legend_y <- c(
          legend_y,
          current$absolute_error
        )
      }
    }

    approxop_add_box()

    legend_labels <- paste0(
      "n = ",
      n_values
    )

    legend_colors <- curve_colors

    legend_line_types <- curve_line_types

    legend_line_widths <- rep(
      lwd,
      number_of_curves
    )
  }

  if (outside_legend) {

    graphics::legend(
      "topright",
      inset = c(-0.32, 0),
      legend = legend_labels,
      title = legend_title,
      col = legend_colors,
      lty = legend_line_types,
      lwd = legend_line_widths,
      bty = "o",
      bg = "white",
      xpd = NA,
      cex = settings$legend_cex
    )

  } else {

    allowed_positions <- c(
      "best",
      "bottomright",
      "bottom",
      "bottomleft",
      "left",
      "topleft",
      "top",
      "topright",
      "right",
      "center"
    )

    if (!legend_position %in% allowed_positions) {
      stop(
        paste0(
          "`legend_position` must be \"best\", \"right\", ",
          "or a valid base R legend position."
        ),
        call. = FALSE
      )
    }

    approxop_add_legend(
      legend = legend_labels,
      col = legend_colors,
      lty = legend_line_types,
      lwd = legend_line_widths,
      x = legend_x,
      y = legend_y,
      position = legend_position,
      title = legend_title
    )
  }

  invisible(data)
}
