test_that("parameter_scan_2d returns the expected structure", {

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
    grid = seq(
      0,
      1,
      length.out = 21
    )
  )

  expect_s3_class(
    result,
    "approx_parameter_scan_2d"
  )

  expect_s3_class(
    result,
    "data.frame"
  )

  expect_equal(
    nrow(result),
    4L
  )

  expect_equal(
    names(result),
    c(
      "parameter1",
      "value1",
      "parameter2",
      "value2",
      "n",
      "valid",
      "max_absolute_error",
      "mae",
      "rmse",
      "message"
    )
  )

  expect_true(
    all(result$valid)
  )

  expect_true(
    all(is.na(result$message))
  )

  expect_true(
    all(is.finite(
      result$max_absolute_error
    ))
  )

  expect_true(
    all(is.finite(
      result$mae
    ))
  )

  expect_true(
    all(is.finite(
      result$rmse
    ))
  )
})


test_that("parameter_scan_2d creates the Cartesian parameter grid", {

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
    values1 = c(
      0.5,
      1,
      1.5
    ),
    parameter2 = "beta",
    values2 = c(
      2,
      3
    ),
    f = function(x) x^2,
    grid = seq(
      0,
      1,
      length.out = 21
    )
  )

  expect_equal(
    nrow(result),
    6L
  )

  expect_equal(
    result$value1,
    c(
      0.5,
      1,
      1.5,
      0.5,
      1,
      1.5
    )
  )

  expect_equal(
    result$value2,
    c(
      2,
      2,
      2,
      3,
      3,
      3
    )
  )

  expect_true(
    all(result$parameter1 == "alpha")
  )

  expect_true(
    all(result$parameter2 == "beta")
  )
})


test_that("parameter_scan_2d preserves fixed operator parameters", {

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
    values1 = 0.5,
    parameter2 = "beta",
    values2 = 2,
    f = function(x) x^2,
    grid = seq(
      0,
      1,
      length.out = 21
    )
  )

  direct_error <- approx_error(
    M,
    f = function(x) x^2,
    x = seq(
      0,
      1,
      length.out = 21
    )
  )

  expect_equal(
    result$max_absolute_error,
    attr(
      direct_error,
      "max_absolute_error"
    ),
    tolerance = 1e-12
  )

  expect_equal(
    result$mae,
    attr(
      direct_error,
      "mae"
    ),
    tolerance = 1e-12
  )

  expect_equal(
    result$rmse,
    attr(
      direct_error,
      "rmse"
    ),
    tolerance = 1e-12
  )
})


test_that("parameter_scan_2d handles invalid parameter combinations", {

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
    values1 = c(
      0.5,
      2,
      3
    ),
    parameter2 = "beta",
    values2 = c(
      1,
      2
    ),
    f = function(x) x^2,
    grid = seq(
      0,
      1,
      length.out = 21
    )
  )

  expect_equal(
    nrow(result),
    6L
  )

  expect_equal(
    sum(result$valid),
    3L
  )

  expect_equal(
    sum(!result$valid),
    3L
  )

  invalid_rows <- !result$valid

  expect_true(
    all(is.na(
      result$max_absolute_error[
        invalid_rows
      ]
    ))
  )

  expect_true(
    all(is.na(
      result$mae[
        invalid_rows
      ]
    ))
  )

  expect_true(
    all(is.na(
      result$rmse[
        invalid_rows
      ]
    ))
  )

  expect_true(
    all(!is.na(
      result$message[
        invalid_rows
      ]
    ))
  )

  expect_match(
    result$message[
      which(invalid_rows)[1]
    ],
    "beta >= alpha",
    fixed = TRUE
  )
})


test_that("parameter_scan_2d stores metadata correctly", {

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

  grid <- seq(
    0,
    1,
    length.out = 51
  )

  result <- parameter_scan_2d(
    M,
    parameter1 = "alpha",
    values1 = c(
      0.5,
      1
    ),
    parameter2 = "beta",
    values2 = c(
      2,
      3
    ),
    f = function(x) x^2,
    grid = grid
  )

  expect_equal(
    attr(result, "family"),
    "sheffer"
  )

  expect_equal(
    attr(result, "subfamily"),
    "meixner"
  )

  expect_equal(
    attr(result, "variant"),
    "discrete"
  )

  expect_equal(
    attr(result, "parameter1"),
    "alpha"
  )

  expect_equal(
    attr(result, "parameter2"),
    "beta"
  )

  expect_equal(
    attr(result, "values1"),
    c(
      0.5,
      1
    )
  )

  expect_equal(
    attr(result, "values2"),
    c(
      2,
      3
    )
  )

  expect_equal(
    attr(result, "n"),
    20L
  )

  expect_equal(
    attr(result, "grid_size"),
    51L
  )

  expect_equal(
    attr(result, "grid_range"),
    c(
      0,
      1
    )
  )

  expect_equal(
    attr(
      result,
      "combination_count"
    ),
    4L
  )

  expect_equal(
    attr(
      result,
      "valid_count"
    ),
    4L
  )

  expect_equal(
    attr(
      result,
      "invalid_count"
    ),
    0L
  )
})


test_that("parameter_scan_2d stores valid and invalid counts", {

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
    values1 = c(
      0.5,
      2,
      3
    ),
    parameter2 = "beta",
    values2 = c(
      1,
      2
    ),
    f = function(x) x^2,
    grid = seq(
      0,
      1,
      length.out = 11
    )
  )

  expect_equal(
    attr(
      result,
      "combination_count"
    ),
    6L
  )

  expect_equal(
    attr(
      result,
      "valid_count"
    ),
    3L
  )

  expect_equal(
    attr(
      result,
      "invalid_count"
    ),
    3L
  )
})


test_that("parameter_scan_2d allows an explicit n value", {

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
    values1 = 0.5,
    parameter2 = "beta",
    values2 = 2,
    f = function(x) x^2,
    grid = seq(
      0,
      1,
      length.out = 11
    ),
    n = 30
  )

  expect_equal(
    result$n,
    30L
  )

  expect_equal(
    attr(result, "n"),
    30L
  )
})


test_that("parameter_scan_2d validates the operator", {

  expect_error(
    parameter_scan_2d(
      list(),
      parameter1 = "alpha",
      values1 = 1,
      parameter2 = "beta",
      values2 = 2,
      f = function(x) x^2,
      grid = c(0, 1)
    ),
    "`operator` must be an object of class"
  )
})


test_that("parameter_scan_2d requires different parameter names", {

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

  expect_error(
    parameter_scan_2d(
      M,
      parameter1 = "alpha",
      values1 = 1,
      parameter2 = "alpha",
      values2 = 2,
      f = function(x) x^2,
      grid = c(0, 1)
    ),
    "must be different"
  )
})


test_that("parameter_scan_2d validates parameter value vectors", {

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

  expect_error(
    parameter_scan_2d(
      M,
      parameter1 = "alpha",
      values1 = numeric(0),
      parameter2 = "beta",
      values2 = 2,
      f = function(x) x^2,
      grid = c(0, 1)
    ),
    "`values1` must be a non-empty finite numeric vector"
  )

  expect_error(
    parameter_scan_2d(
      M,
      parameter1 = "alpha",
      values1 = 1,
      parameter2 = "beta",
      values2 = c(
        2,
        NA_real_
      ),
      f = function(x) x^2,
      grid = c(0, 1)
    ),
    "`values2` must be a non-empty finite numeric vector"
  )
})


test_that("parameter_scan_2d validates f and grid", {

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

  expect_error(
    parameter_scan_2d(
      M,
      parameter1 = "alpha",
      values1 = 1,
      parameter2 = "beta",
      values2 = 2,
      f = 1,
      grid = c(0, 1)
    ),
    "`f` must be a function"
  )

  expect_error(
    parameter_scan_2d(
      M,
      parameter1 = "alpha",
      values1 = 1,
      parameter2 = "beta",
      values2 = 2,
      f = function(x) x^2,
      grid = numeric(0)
    ),
    "`grid` must be a non-empty finite numeric vector"
  )
})


test_that("parameter_scan_2d validates n", {

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

  expect_error(
    parameter_scan_2d(
      M,
      parameter1 = "alpha",
      values1 = 1,
      parameter2 = "beta",
      values2 = 2,
      f = function(x) x^2,
      grid = c(0, 1),
      n = 0
    ),
    "`n` must be one positive integer"
  )

  expect_error(
    parameter_scan_2d(
      M,
      parameter1 = "alpha",
      values1 = 1,
      parameter2 = "beta",
      values2 = 2,
      f = function(x) x^2,
      grid = c(0, 1),
      n = 2.5
    ),
    "`n` must be one positive integer"
  )
})


test_that("parameter_scan_2d checks parameter existence", {

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

  expect_error(
    parameter_scan_2d(
      M,
      parameter1 = "unknown_parameter",
      values1 = 1,
      parameter2 = "beta",
      values2 = 2,
      f = function(x) x^2,
      grid = c(0, 1)
    ),
    "is not present in the operator parameter list"
  )

  expect_error(
    parameter_scan_2d(
      M,
      parameter1 = "alpha",
      values1 = 1,
      parameter2 = "unknown_parameter",
      values2 = 2,
      f = function(x) x^2,
      grid = c(0, 1)
    ),
    "is not present in the operator parameter list"
  )
})
