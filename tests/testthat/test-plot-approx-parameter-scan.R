test_that("plot.approx_parameter_scan works with default metric", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 20,
    params = list(
      a = 2
    )
  )

  Scan <- parameter_scan(
    C,
    parameter = "a",
    values = c(
      1.5,
      2,
      3,
      5,
      10
    ),
    f = function(x) x^2,
    grid = seq(
      0,
      1,
      length.out = 101
    )
  )

  result <- plot(Scan)

  expect_s3_class(
    result,
    "approx_parameter_scan"
  )

  expect_equal(
    result,
    Scan
  )
})


test_that("plot.approx_parameter_scan works for all metrics", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 20,
    params = list(
      a = 2
    )
  )

  Scan <- parameter_scan(
    C,
    parameter = "a",
    values = c(
      1.5,
      2,
      3,
      5,
      10
    ),
    f = function(x) x^2,
    grid = seq(
      0,
      1,
      length.out = 51
    )
  )

  metrics <- c(
    "max_absolute_error",
    "mae",
    "rmse"
  )

  for (current_metric in metrics) {

    result <- plot(
      Scan,
      metric = current_metric
    )

    expect_s3_class(
      result,
      "approx_parameter_scan"
    )
  }
})


test_that("plot.approx_parameter_scan preserves numerical data", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 20,
    params = list(
      a = 2
    )
  )

  Scan <- parameter_scan(
    C,
    parameter = "a",
    values = c(
      1.5,
      2,
      3,
      5,
      10
    ),
    f = function(x) x^2,
    grid = seq(
      0,
      1,
      length.out = 51
    )
  )

  result <- plot(Scan)

  expect_equal(
    result$value,
    Scan$value
  )

  expect_equal(
    result$max_absolute_error,
    Scan$max_absolute_error
  )

  expect_equal(
    result$mae,
    Scan$mae
  )

  expect_equal(
    result$rmse,
    Scan$rmse
  )
})


test_that("plot.approx_parameter_scan works with show_values TRUE and FALSE", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 20,
    params = list(
      a = 2
    )
  )

  Scan <- parameter_scan(
    C,
    parameter = "a",
    values = c(
      1.5,
      2,
      3,
      5,
      10
    ),
    f = function(x) x^2,
    grid = seq(
      0,
      1,
      length.out = 51
    )
  )

  result_true <- plot(
    Scan,
    show_values = TRUE
  )

  result_false <- plot(
    Scan,
    show_values = FALSE
  )

  expect_s3_class(
    result_true,
    "approx_parameter_scan"
  )

  expect_s3_class(
    result_false,
    "approx_parameter_scan"
  )
})


test_that("plot.approx_parameter_scan accepts custom graphical arguments", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 20,
    params = list(
      a = 2
    )
  )

  Scan <- parameter_scan(
    C,
    parameter = "a",
    values = c(
      1.5,
      2,
      3,
      5
    ),
    f = function(x) x^2,
    grid = seq(
      0,
      1,
      length.out = 51
    )
  )

  result <- plot(
    Scan,
    metric = "mae",
    type = "o",
    lwd = 2,
    pch = 15,
    xlab = "Charlier parameter a",
    ylab = "Approximation error",
    main = "Charlier parameter scan"
  )

  expect_s3_class(
    result,
    "approx_parameter_scan"
  )
})


test_that("plot.approx_parameter_scan rejects invalid objects", {

  expect_error(
    plot.approx_parameter_scan(
      data.frame(
        value = c(
          1,
          2,
          3
        )
      )
    ),
    "`x` must be an object of class"
  )
})


test_that("plot.approx_parameter_scan rejects invalid metrics", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 20,
    params = list(
      a = 2
    )
  )

  Scan <- parameter_scan(
    C,
    parameter = "a",
    values = c(
      2,
      3,
      5
    ),
    f = function(x) x^2,
    grid = seq(
      0,
      1,
      length.out = 31
    )
  )

  expect_error(
    plot(
      Scan,
      metric = "unknown"
    )
  )
})


test_that("plot.approx_parameter_scan rejects invalid line widths", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 20,
    params = list(
      a = 2
    )
  )

  Scan <- parameter_scan(
    C,
    parameter = "a",
    values = c(
      2,
      3,
      5
    ),
    f = function(x) x^2,
    grid = seq(
      0,
      1,
      length.out = 31
    )
  )

  expect_error(
    plot(
      Scan,
      lwd = 0
    ),
    "`lwd` must be one positive finite numeric value"
  )

  expect_error(
    plot(
      Scan,
      lwd = -1
    ),
    "`lwd` must be one positive finite numeric value"
  )

  expect_error(
    plot(
      Scan,
      lwd = Inf
    ),
    "`lwd` must be one positive finite numeric value"
  )
})


test_that("plot.approx_parameter_scan rejects invalid plotting symbols", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 20,
    params = list(
      a = 2
    )
  )

  Scan <- parameter_scan(
    C,
    parameter = "a",
    values = c(
      2,
      3,
      5
    ),
    f = function(x) x^2,
    grid = seq(
      0,
      1,
      length.out = 31
    )
  )

  expect_error(
    plot(
      Scan,
      pch = NA_real_
    ),
    "`pch` must be NULL or one finite numeric value"
  )

  expect_error(
    plot(
      Scan,
      pch = Inf
    ),
    "`pch` must be NULL or one finite numeric value"
  )

  expect_error(
    plot(
      Scan,
      pch = c(
        16,
        17
      )
    ),
    "`pch` must be NULL or one finite numeric value"
  )
})


test_that("plot.approx_parameter_scan rejects invalid show_values values", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 20,
    params = list(
      a = 2
    )
  )

  Scan <- parameter_scan(
    C,
    parameter = "a",
    values = c(
      2,
      3,
      5
    ),
    f = function(x) x^2,
    grid = seq(
      0,
      1,
      length.out = 31
    )
  )

  expect_error(
    plot(
      Scan,
      show_values = 1
    ),
    "`show_values` must be TRUE or FALSE"
  )

  expect_error(
    plot(
      Scan,
      show_values = NA
    ),
    "`show_values` must be TRUE or FALSE"
  )

  expect_error(
    plot(
      Scan,
      show_values = c(
        TRUE,
        FALSE
      )
    ),
    "`show_values` must be TRUE or FALSE"
  )
})


test_that("plot.approx_parameter_scan works with unsorted parameter values", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 20,
    params = list(
      a = 2
    )
  )

  Scan <- parameter_scan(
    C,
    parameter = "a",
    values = c(
      10,
      2,
      5,
      1.5,
      3
    ),
    f = function(x) x^2,
    grid = seq(
      0,
      1,
      length.out = 51
    )
  )

  original_values <- Scan$value

  result <- plot(Scan)

  expect_s3_class(
    result,
    "approx_parameter_scan"
  )

  expect_equal(
    result$value,
    original_values
  )
})


test_that("plot.approx_parameter_scan works with a single parameter value", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 20,
    params = list(
      a = 2
    )
  )

  Scan <- parameter_scan(
    C,
    parameter = "a",
    values = 2,
    f = function(x) x^2,
    grid = seq(
      0,
      1,
      length.out = 31
    )
  )

  result <- plot(Scan)

  expect_s3_class(
    result,
    "approx_parameter_scan"
  )

  expect_equal(
    nrow(result),
    1L
  )
})


test_that("plot.approx_parameter_scan preserves metadata", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 20,
    params = list(
      a = 2
    )
  )

  Scan <- parameter_scan(
    C,
    parameter = "a",
    values = c(
      1.5,
      2,
      3,
      5
    ),
    f = function(x) x^2,
    grid = seq(
      0,
      1,
      length.out = 51
    )
  )

  result <- plot(Scan)

  expect_equal(
    attr(
      result,
      "family"
    ),
    "sheffer"
  )

  expect_equal(
    attr(
      result,
      "subfamily"
    ),
    "charlier"
  )

  expect_equal(
    attr(
      result,
      "variant"
    ),
    "discrete"
  )

  expect_equal(
    attr(
      result,
      "parameter"
    ),
    "a"
  )

  expect_equal(
    attr(
      result,
      "n"
    ),
    20L
  )

  expect_equal(
    attr(
      result,
      "grid_size"
    ),
    51L
  )

  expect_equal(
    attr(
      result,
      "grid_range"
    ),
    c(
      0,
      1
    )
  )
})
