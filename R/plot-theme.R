# Graphical style for ApproxOp
#
# Internal utilities used by the plotting methods of the package.
# These functions provide a common visual identity for ApproxOp
# graphics while keeping the plotting system based on base R.

approxop_palette <- function(n) {

  if (!is.numeric(n) ||
      length(n) != 1L ||
      !is.finite(n) ||
      n < 1 ||
      n != as.integer(n)) {
    stop(
      "`n` must be one positive integer.",
      call. = FALSE
    )
  }

  n <- as.integer(n)

  base_colors <- c(
    "#0072B2", # blue
    "#D55E00", # vermillion
    "#009E73", # green
    "#CC79A7", # reddish purple
    "#E69F00", # orange
    "#56B4E9", # sky blue
    "#000000", # black
    "#F0E442"  # yellow
  )

  if (n <= length(base_colors)) {
    return(base_colors[seq_len(n)])
  }

  grDevices::hcl.colors(
    n,
    palette = "Dark 3"
  )
}


approxop_line_types <- function(n) {

  if (!is.numeric(n) ||
      length(n) != 1L ||
      !is.finite(n) ||
      n < 1 ||
      n != as.integer(n)) {
    stop(
      "`n` must be one positive integer.",
      call. = FALSE
    )
  }

  n <- as.integer(n)

  base_types <- c(
    1, 2, 3, 4, 5, 6
  )

  rep(
    base_types,
    length.out = n
  )
}


approxop_point_types <- function(n) {

  if (!is.numeric(n) ||
      length(n) != 1L ||
      !is.finite(n) ||
      n < 1 ||
      n != as.integer(n)) {
    stop(
      "`n` must be one positive integer.",
      call. = FALSE
    )
  }

  n <- as.integer(n)

  base_points <- c(
    16, 17, 15, 18, 8, 3, 7, 4
  )

  rep(
    base_points,
    length.out = n
  )
}


approxop_graphics_settings <- function() {

  list(
    exact_color = "#202020",
    exact_lwd = 2.2,
    approximation_lwd = 1.8,
    error_lwd = 1.6,
    point_cex = 0.9,
    axis_cex = 0.95,
    label_cex = 1.05,
    title_cex = 1.10,
    legend_cex = 0.88,
    grid_lty = 3,
    grid_lwd = 0.6,
    grid_color = "grey88",
    border_color = "grey30"
  )
}


approxop_add_grid <- function(
    nx = NULL,
    ny = NULL
) {

  settings <- approxop_graphics_settings()

  graphics::grid(
    nx = nx,
    ny = ny,
    col = settings$grid_color,
    lty = settings$grid_lty,
    lwd = settings$grid_lwd
  )

  invisible(NULL)
}


approxop_add_box <- function() {

  settings <- approxop_graphics_settings()

  graphics::box(
    col = settings$border_color,
    lwd = 1
  )

  invisible(NULL)
}


approxop_legend_position <- function(
    x = NULL,
    y = NULL,
    preferred = "best"
) {

  allowed <- c(
    "best",
    "topright",
    "topleft",
    "bottomright",
    "bottomleft",
    "top",
    "bottom",
    "left",
    "right",
    "center"
  )

  if (!is.character(preferred) ||
      length(preferred) != 1L ||
      is.na(preferred) ||
      !(preferred %in% allowed)) {
    stop(
      paste0(
        "`preferred` must be one of: ",
        paste(
          allowed,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }

  if (preferred != "best") {
    return(preferred)
  }

  if (is.null(x) ||
      is.null(y) ||
      length(x) == 0L ||
      length(y) == 0L) {
    return("topright")
  }

  keep <- is.finite(x) &
    is.finite(y)

  x <- x[keep]
  y <- y[keep]

  if (length(x) == 0L ||
      length(y) == 0L) {
    return("topright")
  }

  x_mid <- mean(
    range(x)
  )

  y_mid <- mean(
    range(y)
  )

  quadrants <- c(
    topright = 0,
    topleft = 0,
    bottomright = 0,
    bottomleft = 0
  )

  quadrants["topright"] <-
    sum(
      x >= x_mid &
        y >= y_mid
    )

  quadrants["topleft"] <-
    sum(
      x < x_mid &
        y >= y_mid
    )

  quadrants["bottomright"] <-
    sum(
      x >= x_mid &
        y < y_mid
    )

  quadrants["bottomleft"] <-
    sum(
      x < x_mid &
        y < y_mid
    )

  names(
    quadrants
  )[which.min(quadrants)]
}


approxop_add_legend <- function(
    legend,
    col,
    lty,
    lwd,
    pch = NA,
    x = NULL,
    y = NULL,
    position = "best",
    title = NULL,
    bty = "o",
    cex = NULL
) {

  settings <- approxop_graphics_settings()

  if (is.null(cex)) {
    cex <- settings$legend_cex
  }

  legend_position <-
    approxop_legend_position(
      x = x,
      y = y,
      preferred = position
    )

  graphics::legend(
    legend_position,
    legend = legend,
    col = col,
    lty = lty,
    lwd = lwd,
    pch = pch,
    title = title,
    bty = bty,
    cex = cex,
    bg = "white",
    inset = 0.015
  )

  invisible(
    legend_position
  )
}
