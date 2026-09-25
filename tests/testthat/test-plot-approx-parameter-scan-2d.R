test_that("plot.approx_parameter_scan_2d returns object invisibly", {

  M <- approx_operator(
    family = "sheffer",
    subfamily = "meixner",
    n = 20,
    params = list(
      alpha = 0.5,
      beta = 2,
      theta = 1,
      c = 0.75
    )
  )

  result <- parameter_scan_2d(
    M,
    parameter1 = "alpha",
    values1 = c(0.5, 1),
    parameter2 = "beta",
    values2 = c(2, 3),
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 21)
  )

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  plotted <- plot(result)

  expect_identical(
    plotted,
    result
  )
})


test_that("plot.approx_parameter_scan_2d accepts all error metrics", {

  M <- approx_operator(
    family = "sheffer",
    subfamily = "meixner",
    n = 20,
    params = list(
      alpha = 0.5,
      beta = 2,
      theta = 1,
      c = 0.75
    )
  )

  result <- parameter_scan_2d(
    M,
    parameter1 = "alpha",
    values1 = c(0.5, 1),
    parameter2 = "beta",
    values2 = c(2, 3),
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 21)
  )

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_no_error(
    plot(
      result,
      metric = "max_absolute_error"
    )
  )

  expect_no_error(
    plot(
      result,
      metric = "mae"
    )
  )

  expect_no_error(
    plot(
      result,
      metric = "rmse"
    )
  )
})


test_that("plot.approx_parameter_scan_2d handles invalid combinations", {

  M <- approx_operator(
    family = "sheffer",
    subfamily = "meixner",
    n = 20,
    params = list(
      alpha = 0.5,
      beta = 2,
      theta = 1,
      c = 0.75
    )
  )

  result <- parameter_scan_2d(
    M,
    parameter1 = "alpha",
    values1 = c(0.5, 2, 3),
    parameter2 = "beta",
    values2 = c(1, 2),
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 21)
  )

  expect_true(
    any(!result$valid)
  )

  expect_true(
    any(result$valid)
  )

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_no_error(
    plot(result)
  )

  expect_no_error(
    plot(
      result,
      show_invalid = FALSE
    )
  )
})


test_that("plot.approx_parameter_scan_2d validates metric", {

  M <- approx_operator(
    family = "sheffer",
    subfamily = "meixner",
    n = 20,
    params = list(
      alpha = 0.5,
      beta = 2,
      theta = 1,
      c = 0.75
    )
  )

  result <- parameter_scan_2d(
    M,
    parameter1 = "alpha",
    values1 = c(0.5, 1),
    parameter2 = "beta",
    values2 = c(2, 3),
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 21)
  )

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_error(
    plot(
      result,
      metric = "wrong_metric"
    )
  )
})


test_that("plot.approx_parameter_scan_2d validates show_invalid", {

  M <- approx_operator(
    family = "sheffer",
    subfamily = "meixner",
    n = 20,
    params = list(
      alpha = 0.5,
      beta = 2,
      theta = 1,
      c = 0.75
    )
  )

  result <- parameter_scan_2d(
    M,
    parameter1 = "alpha",
    values1 = c(0.5, 1),
    parameter2 = "beta",
    values2 = c(2, 3),
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 21)
  )

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_error(
    plot(
      result,
      show_invalid = NA
    ),
    "`show_invalid` must be TRUE or FALSE"
  )

  expect_error(
    plot(
      result,
      show_invalid = "yes"
    ),
    "`show_invalid` must be TRUE or FALSE"
  )
})


test_that("plot.approx_parameter_scan_2d validates plotting symbols", {

  M <- approx_operator(
    family = "sheffer",
    subfamily = "meixner",
    n = 20,
    params = list(
      alpha = 0.5,
      beta = 2,
      theta = 1,
      c = 0.75
    )
  )

  result <- parameter_scan_2d(
    M,
    parameter1 = "alpha",
    values1 = c(0.5, 1),
    parameter2 = "beta",
    values2 = c(2, 3),
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 21)
  )

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_error(
    plot(
      result,
      pch = NA
    )
  )

  expect_error(
    plot(
      result,
      invalid_pch = NA
    )
  )
})


test_that("plot.approx_parameter_scan_2d accepts custom labels and title", {

  M <- approx_operator(
    family = "sheffer",
    subfamily = "meixner",
    n = 20,
    params = list(
      alpha = 0.5,
      beta = 2,
      theta = 1,
      c = 0.75
    )
  )

  result <- parameter_scan_2d(
    M,
    parameter1 = "alpha",
    values1 = c(0.5, 1),
    parameter2 = "beta",
    values2 = c(2, 3),
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 21)
  )

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_no_error(
    plot(
      result,
      xlab = "Alpha",
      ylab = "Beta",
      main = "Two-dimensional parameter scan"
    )
  )
})
