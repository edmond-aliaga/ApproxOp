#' Plot an Approximation Operator Comparison
#'
#' Produces a graphical comparison of approximation errors for
#' several approximation operators and values of n.
#'
#' The available error metrics are the maximum absolute error,
#' mean absolute error (MAE), and root mean squared error (RMSE).
#'
#' @param x An object of class \code{"approx_operator_comparison"}.
#' @param metric Error metric to be plotted. One of
#'   \code{"max_absolute_error"}, \code{"mae"}, or \code{"rmse"}.
#' @param type Plot type passed to \code{graphics::lines}.
#' @param lwd Line width.
#' @param pch Plotting symbol used for the first operator. If
#'   \code{NULL}, the standard ApproxOp plotting symbols are used.
#' @param show_n Logical. If \code{TRUE}, the actual values of
#'   \code{n} are displayed on the horizontal axis.
#' @param show_legend Logical. If \code{TRUE}, a legend is displayed.
#' @param legend_position Position of the legend. The default
#'   \code{"best"} places the legend automatically in a low-density
#'   region of the graph. The value \code{"right"} places the legend
#'   outside the plotting region. Standard base R legend positions
#'   are also supported.
#' @param legend_title Optional title for the legend.
#' @param grid_lines Logical value indicating whether light grid lines
#'   should be displayed.
#' @param xlab Label for the horizontal axis.
#' @param ylab Optional label for the vertical axis.
#' @param main Optional plot title.
#' @param ... Additional graphical arguments passed to
#'   \code{graphics::plot}.
#'
#' @return The object \code{x}, invisibly.
#'
#' @export
plot.approx_operator_comparison <- function(
    x,
    metric = c(
      "max_absolute_error",
      "mae",
      "rmse"
    ),
    type = "b",
    lwd = NULL,
    pch = NULL,
    show_n = TRUE,
    show_legend = TRUE,
    legend_position = "best",
    legend_title = "Operator",
    grid_lines = TRUE,
    xlab = "n",
    ylab = NULL,
    main = NULL,
    ...
) {

  if (!inherits(
    x,
    "approx_operator_comparison"
  )) {
    stop(
      "`x` must be an object of class \"approx_operator_comparison\".",
      call. = FALSE
    )
  }

  metric <- match.arg(metric)

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

  if (!is.null(pch) &&
      (!is.numeric(pch) ||
       length(pch) != 1L ||
       !is.finite(pch))) {
    stop(
      "`pch` must be NULL or one finite numeric value.",
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

  if (!is.logical(show_legend) ||
      length(show_legend) != 1L ||
      is.na(show_legend)) {
    stop(
      "`show_legend` must be TRUE or FALSE.",
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

  operator_names <- unique(
    x$operator
  )

  number_of_operators <-
    length(operator_names)

  operator_colors <-
    approxop_palette(
      number_of_operators
    )

  line_types <-
    approxop_line_types(
      number_of_operators
    )

  if (is.null(pch)) {

    point_types <-
      approxop_point_types(
        number_of_operators
      )

  } else {

    point_types <-
      pch +
      seq_len(number_of_operators) -
      1L
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
        "Comparison of maximum absolute errors",
      mae =
        "Comparison of mean absolute errors",
      rmse =
        "Comparison of root mean squared errors"
    )
  }

  all_n <- sort(
    unique(
      x$n
    )
  )

  y_values <- x[[metric]]

  if (anyNA(y_values) ||
      any(!is.finite(y_values))) {
    stop(
      "The selected error metric contains non-finite values.",
      call. = FALSE
    )
  }

  x_range <- range(
    x$n,
    finite = TRUE
  )

  y_range <- range(
    y_values,
    finite = TRUE
  )

  if (diff(x_range) == 0) {

    x_padding <- max(
      1,
      abs(x_range[1]) * 0.05
    )

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

  old_par <- graphics::par(
    no.readonly = TRUE
  )

  on.exit(
    graphics::par(old_par),
    add = TRUE
  )

  outside_legend <-
    show_legend &&
    identical(
      legend_position,
      "right"
    )

  if (outside_legend) {

    graphics::par(
      mar = c(
        old_par$mar[1],
        old_par$mar[2],
        old_par$mar[3],
        max(
          old_par$mar[4],
          8.5
        )
      ),
      xpd = NA
    )

  } else {

    graphics::par(
      xpd = FALSE
    )
  }

  graphics::plot(
    NA,
    NA,
    xlim = x_range,
    ylim = y_range,
    type = "n",
    xaxt = if (
      show_n
    ) {
      "n"
    } else {
      "s"
    },
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

  if (show_n) {

    graphics::axis(
      side = 1,
      at = all_n,
      labels = all_n,
      cex.axis = settings$axis_cex
    )
  }

  legend_x <- numeric(0)
  legend_y <- numeric(0)

  for (j in seq_along(
    operator_names
  )) {

    current_operator <-
      operator_names[j]

    current_data <-
      x[
        x$operator ==
          current_operator,
        ,
        drop = FALSE
      ]

    current_data <-
      current_data[
        order(
          current_data$n
        ),
        ,
        drop = FALSE
      ]

    graphics::lines(
      current_data$n,
      current_data[[metric]],
      type = type,
      col = operator_colors[j],
      lty = line_types[j],
      lwd = lwd,
      pch = point_types[j],
      cex = settings$point_cex
    )

    legend_x <- c(
      legend_x,
      current_data$n
    )

    legend_y <- c(
      legend_y,
      current_data[[metric]]
    )
  }

  approxop_add_box()

  if (show_legend) {

    if (outside_legend) {

      plot_coordinates <-
        graphics::par("usr")

      x_width <-
        plot_coordinates[2] -
        plot_coordinates[1]

      legend_x_position <-
        plot_coordinates[2] +
        0.05 * x_width

      legend_y_position <-
        plot_coordinates[4]

      graphics::legend(
        x = legend_x_position,
        y = legend_y_position,
        legend = operator_names,
        title = legend_title,
        col = operator_colors,
        lty = line_types,
        lwd = lwd,
        pch = point_types,
        bty = "o",
        bg = "white",
        cex = settings$legend_cex,
        xjust = 0,
        yjust = 1,
        xpd = NA
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

      if (!legend_position %in%
          allowed_positions) {
        stop(
          paste0(
            "`legend_position` must be \"best\", \"right\", ",
            "or a valid base R legend position."
          ),
          call. = FALSE
        )
      }

      approxop_add_legend(
        legend = operator_names,
        col = operator_colors,
        lty = line_types,
        lwd = rep(
          lwd,
          number_of_operators
        ),
        pch = point_types,
        x = legend_x,
        y = legend_y,
        position = legend_position,
        title = legend_title
      )
    }
  }

  invisible(x)
}
