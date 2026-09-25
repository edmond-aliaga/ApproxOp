test_that("approxop_palette returns the requested number of colors", {

  colors <- ApproxOp:::approxop_palette(4)

  expect_length(
    colors,
    4L
  )

  expect_type(
    colors,
    "character"
  )

  expect_true(
    all(nzchar(colors))
  )
})


test_that("approxop_palette supports more than eight colors", {

  colors <- ApproxOp:::approxop_palette(12)

  expect_length(
    colors,
    12L
  )

  expect_type(
    colors,
    "character"
  )
})


test_that("approxop_palette validates n", {

  expect_error(
    ApproxOp:::approxop_palette(0),
    "`n` must be one positive integer"
  )

  expect_error(
    ApproxOp:::approxop_palette(-1),
    "`n` must be one positive integer"
  )

  expect_error(
    ApproxOp:::approxop_palette(2.5),
    "`n` must be one positive integer"
  )

  expect_error(
    ApproxOp:::approxop_palette(NA_real_),
    "`n` must be one positive integer"
  )
})


test_that("approxop_line_types returns the requested number of line types", {

  line_types <- ApproxOp:::approxop_line_types(8)

  expect_length(
    line_types,
    8L
  )

  expect_true(
    all(is.finite(line_types))
  )
})


test_that("approxop_line_types validates n", {

  expect_error(
    ApproxOp:::approxop_line_types(0),
    "`n` must be one positive integer"
  )

  expect_error(
    ApproxOp:::approxop_line_types(1.5),
    "`n` must be one positive integer"
  )
})


test_that("approxop_point_types returns the requested number of point types", {

  point_types <- ApproxOp:::approxop_point_types(10)

  expect_length(
    point_types,
    10L
  )

  expect_true(
    all(is.finite(point_types))
  )
})


test_that("approxop_point_types validates n", {

  expect_error(
    ApproxOp:::approxop_point_types(0),
    "`n` must be one positive integer"
  )

  expect_error(
    ApproxOp:::approxop_point_types(2.5),
    "`n` must be one positive integer"
  )
})


test_that("approxop_graphics_settings returns required settings", {

  settings <-
    ApproxOp:::approxop_graphics_settings()

  expect_type(
    settings,
    "list"
  )

  expected_names <- c(
    "exact_color",
    "exact_lwd",
    "approximation_lwd",
    "error_lwd",
    "point_cex",
    "axis_cex",
    "label_cex",
    "title_cex",
    "legend_cex",
    "grid_lty",
    "grid_lwd",
    "grid_color",
    "border_color"
  )

  expect_true(
    all(
      expected_names %in%
        names(settings)
    )
  )

  expect_true(
    settings$exact_lwd > 0
  )

  expect_true(
    settings$approximation_lwd > 0
  )

  expect_true(
    settings$error_lwd > 0
  )
})


test_that("approxop_legend_position accepts explicit positions", {

  positions <- c(
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

  for (position in positions) {

    result <-
      ApproxOp:::approxop_legend_position(
        preferred = position
      )

    expect_identical(
      result,
      position
    )
  }
})


test_that("approxop_legend_position validates preferred position", {

  expect_error(
    ApproxOp:::approxop_legend_position(
      preferred = "outside"
    ),
    "`preferred` must be one of"
  )

  expect_error(
    ApproxOp:::approxop_legend_position(
      preferred = NA_character_
    ),
    "`preferred` must be one of"
  )
})


test_that("approxop_legend_position defaults safely when data are absent", {

  result <-
    ApproxOp:::approxop_legend_position(
      preferred = "best"
    )

  expect_identical(
    result,
    "topright"
  )
})


test_that("approxop_legend_position chooses a low-density quadrant", {

  x <- c(
    0.1,
    0.2,
    0.3,
    0.4
  )

  y <- c(
    0.1,
    0.2,
    0.3,
    0.4
  )

  result <-
    ApproxOp:::approxop_legend_position(
      x = x,
      y = y,
      preferred = "best"
    )

  expect_true(
    result %in% c(
      "topright",
      "topleft",
      "bottomright",
      "bottomleft"
    )
  )
})


test_that("approxop_legend_position ignores non-finite observations", {

  x <- c(
    0,
    1,
    NA,
    Inf
  )

  y <- c(
    0,
    1,
    2,
    3
  )

  result <-
    ApproxOp:::approxop_legend_position(
      x = x,
      y = y,
      preferred = "best"
    )

  expect_true(
    result %in% c(
      "topright",
      "topleft",
      "bottomright",
      "bottomleft"
    )
  )
})


test_that("approxop_add_grid works on an active graphics device", {

  grDevices::pdf(NULL)
  on.exit(
    grDevices::dev.off(),
    add = TRUE
  )

  graphics::plot(
    1:3,
    1:3,
    type = "n"
  )

  expect_no_error(
    ApproxOp:::approxop_add_grid()
  )
})


test_that("approxop_add_box works on an active graphics device", {

  grDevices::pdf(NULL)
  on.exit(
    grDevices::dev.off(),
    add = TRUE
  )

  graphics::plot(
    1:3,
    1:3,
    type = "n"
  )

  expect_no_error(
    ApproxOp:::approxop_add_box()
  )
})


test_that("approxop_add_legend works with automatic placement", {

  grDevices::pdf(NULL)
  on.exit(
    grDevices::dev.off(),
    add = TRUE
  )

  x <- seq(
    0,
    1,
    length.out = 20
  )

  y <- x^2

  graphics::plot(
    x,
    y,
    type = "l"
  )

  position <-
    ApproxOp:::approxop_add_legend(
      legend = "Curve",
      col = "#0072B2",
      lty = 1,
      lwd = 2,
      x = x,
      y = y,
      position = "best"
    )

  expect_true(
    position %in% c(
      "topright",
      "topleft",
      "bottomright",
      "bottomleft"
    )
  )
})


test_that("approxop_add_legend accepts an explicit position", {

  grDevices::pdf(NULL)
  on.exit(
    grDevices::dev.off(),
    add = TRUE
  )

  graphics::plot(
    1:3,
    1:3,
    type = "l"
  )

  position <-
    ApproxOp:::approxop_add_legend(
      legend = "Curve",
      col = "#0072B2",
      lty = 1,
      lwd = 2,
      position = "bottomright"
    )

  expect_identical(
    position,
    "bottomright"
  )
})
