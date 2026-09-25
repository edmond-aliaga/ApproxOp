test_that("plot.approx_operator works for approximation plots", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  result <- plot(
    B,
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 20),
    n = c(5, 10, 20),
    type = "approximation"
  )

  expect_s3_class(
    result,
    "approximation_data"
  )

  expect_equal(
    attr(result, "n_values"),
    c(5L, 10L, 20L)
  )

  expect_equal(
    nrow(result),
    60
  )
})


test_that("plot.approx_operator works for error plots", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  result <- plot(
    B,
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 20),
    n = c(5, 10, 20),
    type = "error"
  )

  expect_s3_class(
    result,
    "approximation_data"
  )

  expect_equal(
    attr(result, "n_values"),
    c(5L, 10L, 20L)
  )

  expect_equal(
    nrow(result),
    60
  )
})


test_that("plot.approx_operator returns correct Bernstein data", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  grid <- c(
    0.2,
    0.5,
    0.8
  )

  result <- plot(
    B,
    f = function(x) x^2,
    grid = grid,
    n = c(10, 20),
    type = "approximation"
  )

  data10 <- result[
    result$n == 10,
    ,
    drop = FALSE
  ]

  data20 <- result[
    result$n == 20,
    ,
    drop = FALSE
  ]

  expect_equal(
    data10$exact,
    grid^2,
    tolerance = 1e-12
  )

  expect_equal(
    data10$approximation,
    grid^2 +
      grid * (1 - grid) / 10,
    tolerance = 1e-12
  )

  expect_equal(
    data20$approximation,
    grid^2 +
      grid * (1 - grid) / 20,
    tolerance = 1e-12
  )
})


test_that("error plot contains correct Bernstein absolute errors", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  grid <- c(
    0,
    0.25,
    0.5,
    0.75,
    1
  )

  result <- plot(
    B,
    f = function(x) x^2,
    grid = grid,
    n = c(5, 10, 20),
    type = "error"
  )

  for (ni in c(5, 10, 20)) {

    current <- result[
      result$n == ni,
      ,
      drop = FALSE
    ]

    expected <-
      grid * (1 - grid) / ni

    expect_equal(
      current$absolute_error,
      expected,
      tolerance = 1e-12
    )
  }
})


test_that("Bernstein error plot has the correct midpoint errors", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  result <- plot(
    B,
    f = function(x) x^2,
    grid = 0.5,
    n = c(5, 10, 20),
    type = "error"
  )

  expect_equal(
    result$absolute_error,
    c(
      0.05,
      0.025,
      0.0125
    ),
    tolerance = 1e-12
  )
})


test_that("plot.approx_operator uses operator n when n is NULL", {

  B <- approx_operator(
    family = "bernstein",
    n = 25
  )

  result <- plot(
    B,
    f = function(x) x^2,
    grid = c(0.2, 0.5, 0.8)
  )

  expect_equal(
    unique(result$n),
    25L
  )

  expect_equal(
    attr(result, "n_values"),
    25L
  )
})


test_that("plot.approx_operator works with a Sheffer subfamily", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 10,
    params = list(
      a = 2
    )
  )

  result <- plot(
    C,
    f = function(x) x,
    grid = seq(
      0,
      1,
      length.out = 10
    ),
    n = c(10, 20),
    type = "approximation"
  )

  expect_s3_class(
    result,
    "approximation_data"
  )

  expect_equal(
    nrow(result),
    20
  )

  expect_true(
    all(
      is.finite(
        result$approximation
      )
    )
  )
})


test_that("plot.approx_operator rejects invalid plot types", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  expect_error(
    plot(
      B,
      f = function(x) x^2,
      grid = seq(0, 1, length.out = 10),
      type = "unknown"
    )
  )
})


test_that("plot.approx_operator rejects invalid line widths", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  expect_error(
    plot(
      B,
      f = function(x) x^2,
      grid = seq(0, 1, length.out = 10),
      lwd = 0
    ),
    "`lwd` must be one positive finite numeric value"
  )

  expect_error(
    plot(
      B,
      f = function(x) x^2,
      grid = seq(0, 1, length.out = 10),
      lwd = -1
    ),
    "`lwd` must be one positive finite numeric value"
  )

  expect_error(
    plot(
      B,
      f = function(x) x^2,
      grid = seq(0, 1, length.out = 10),
      lwd = Inf
    ),
    "`lwd` must be one positive finite numeric value"
  )
})


test_that("plot.approx_operator rejects invalid legend positions", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  expect_error(
    plot(
      B,
      f = function(x) x^2,
      grid = seq(0, 1, length.out = 10),
      legend_position = "somewhere"
    ),
    "valid base R legend position"
  )
})


test_that("plot.approx_operator accepts standard legend positions", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  result <- plot(
    B,
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 10),
    legend_position = "topleft"
  )

  expect_s3_class(
    result,
    "approximation_data"
  )
})


test_that("plot.approx_operator accepts a legend title", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  result <- plot(
    B,
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 10),
    n = c(5, 10),
    legend_title = "Degree"
  )

  expect_s3_class(
    result,
    "approximation_data"
  )
})


test_that("plot.approx_operator rejects invalid legend titles", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  expect_error(
    plot(
      B,
      f = function(x) x^2,
      grid = seq(0, 1, length.out = 10),
      legend_title = c("A", "B")
    ),
    "`legend_title` must be NULL or a single character value"
  )
})


test_that("plot.approx_operator requires a function", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  expect_error(
    plot(
      B,
      f = 5,
      grid = seq(0, 1, length.out = 10)
    ),
    "`f` must be a function"
  )
})


test_that("plot.approx_operator requires a grid", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  expect_error(
    plot(
      B,
      f = function(x) x^2
    ),
    "`grid` must be supplied"
  )
})
