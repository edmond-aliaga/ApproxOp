test_that("parameter_scan returns the expected object structure", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 20,
    params = list(
      a = 2
    )
  )

  result <- parameter_scan(
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

  expect_s3_class(
    result,
    "approx_parameter_scan"
  )

  expect_s3_class(
    result,
    "data.frame"
  )

  expect_equal(
    nrow(result),
    5L
  )

  expect_equal(
    names(result),
    c(
      "parameter",
      "value",
      "n",
      "max_absolute_error",
      "mae",
      "rmse"
    )
  )
})


test_that("parameter_scan preserves parameter values and degree", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 20,
    params = list(
      a = 2
    )
  )

  values <- c(
    1.5,
    2,
    3,
    5,
    10
  )

  result <- parameter_scan(
    C,
    parameter = "a",
    values = values,
    f = function(x) x^2,
    grid = seq(
      0,
      1,
      length.out = 101
    )
  )

  expect_equal(
    result$parameter,
    rep(
      "a",
      length(values)
    )
  )

  expect_equal(
    result$value,
    values
  )

  expect_equal(
    result$n,
    rep(
      20,
      length(values)
    )
  )
})


test_that("parameter_scan gives correct Charlier maximum errors", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 20,
    params = list(
      a = 2
    )
  )

  result <- parameter_scan(
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

  expected <- c(
    0.255,
    0.205,
    0.180,
    0.1675,
    0.160555555555556
  )

  expect_equal(
    result$max_absolute_error,
    expected,
    tolerance = 1e-10
  )
})


test_that("parameter_scan error metrics are finite and nonnegative", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 20,
    params = list(
      a = 2
    )
  )

  result <- parameter_scan(
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

  expect_true(
    all(
      is.finite(
        result$max_absolute_error
      )
    )
  )

  expect_true(
    all(
      is.finite(
        result$mae
      )
    )
  )

  expect_true(
    all(
      is.finite(
        result$rmse
      )
    )
  )

  expect_true(
    all(
      result$max_absolute_error >= 0
    )
  )

  expect_true(
    all(
      result$mae >= 0
    )
  )

  expect_true(
    all(
      result$rmse >= 0
    )
  )
})


test_that("parameter_scan can override the operator degree", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 20,
    params = list(
      a = 2
    )
  )

  result <- parameter_scan(
    C,
    parameter = "a",
    values = c(
      2,
      3
    ),
    f = function(x) x^2,
    grid = seq(
      0,
      1,
      length.out = 51
    ),
    n = 40
  )

  expect_equal(
    result$n,
    c(
      40,
      40
    )
  )

  expect_equal(
    attr(
      result,
      "n"
    ),
    40L
  )
})


test_that("parameter_scan does not modify the original operator", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 20,
    params = list(
      a = 2
    )
  )

  original_a <-
    C$parameters$a

  parameter_scan(
    C,
    parameter = "a",
    values = c(
      1.5,
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

  expect_equal(
    C$parameters$a,
    original_a
  )

  expect_equal(
    C$n,
    20L
  )
})


test_that("parameter_scan stores metadata correctly", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 20,
    params = list(
      a = 2
    )
  )

  values <- c(
    1.5,
    2,
    3
  )

  result <- parameter_scan(
    C,
    parameter = "a",
    values = values,
    f = function(x) x^2,
    grid = seq(
      0,
      1,
      length.out = 101
    )
  )

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
      "values"
    ),
    values
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
    101L
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


test_that("parameter_scan rejects an invalid operator", {

  expect_error(
    parameter_scan(
      operator = list(),
      parameter = "a",
      values = c(
        2,
        3
      ),
      f = function(x) x^2,
      grid = seq(
        0,
        1,
        length.out = 11
      )
    ),
    "`operator` must be an object of class"
  )
})


test_that("parameter_scan rejects invalid parameter names", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 20,
    params = list(
      a = 2
    )
  )

  expect_error(
    parameter_scan(
      C,
      parameter = "",
      values = c(
        2,
        3
      ),
      f = function(x) x^2,
      grid = seq(
        0,
        1,
        length.out = 11
      )
    ),
    "`parameter` must be one non-empty character string"
  )

  expect_error(
    parameter_scan(
      C,
      parameter = c(
        "a",
        "b"
      ),
      values = c(
        2,
        3
      ),
      f = function(x) x^2,
      grid = seq(
        0,
        1,
        length.out = 11
      )
    ),
    "`parameter` must be one non-empty character string"
  )
})


test_that("parameter_scan rejects parameters not present in operator", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 20,
    params = list(
      a = 2
    )
  )

  expect_error(
    parameter_scan(
      C,
      parameter = "alpha",
      values = c(
        1,
        2
      ),
      f = function(x) x^2,
      grid = seq(
        0,
        1,
        length.out = 11
      )
    ),
    "Parameter `alpha` is not present"
  )
})


test_that("parameter_scan rejects invalid values", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 20,
    params = list(
      a = 2
    )
  )

  expect_error(
    parameter_scan(
      C,
      parameter = "a",
      values = numeric(0),
      f = function(x) x^2,
      grid = seq(
        0,
        1,
        length.out = 11
      )
    ),
    "`values` must be a non-empty finite numeric vector"
  )

  expect_error(
    parameter_scan(
      C,
      parameter = "a",
      values = c(
        2,
        NA
      ),
      f = function(x) x^2,
      grid = seq(
        0,
        1,
        length.out = 11
      )
    ),
    "`values` must be a non-empty finite numeric vector"
  )

  expect_error(
    parameter_scan(
      C,
      parameter = "a",
      values = c(
        2,
        Inf
      ),
      f = function(x) x^2,
      grid = seq(
        0,
        1,
        length.out = 11
      )
    ),
    "`values` must be a non-empty finite numeric vector"
  )
})


test_that("parameter_scan rejects an invalid function", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 20,
    params = list(
      a = 2
    )
  )

  expect_error(
    parameter_scan(
      C,
      parameter = "a",
      values = c(
        2,
        3
      ),
      f = 2,
      grid = seq(
        0,
        1,
        length.out = 11
      )
    ),
    "`f` must be a function"
  )
})


test_that("parameter_scan rejects an invalid grid", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 20,
    params = list(
      a = 2
    )
  )

  expect_error(
    parameter_scan(
      C,
      parameter = "a",
      values = c(
        2,
        3
      ),
      f = function(x) x^2,
      grid = numeric(0)
    ),
    "`grid` must be a non-empty finite numeric vector"
  )

  expect_error(
    parameter_scan(
      C,
      parameter = "a",
      values = c(
        2,
        3
      ),
      f = function(x) x^2,
      grid = c(
        0,
        NA,
        1
      )
    ),
    "`grid` must be a non-empty finite numeric vector"
  )
})


test_that("parameter_scan rejects invalid n values", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 20,
    params = list(
      a = 2
    )
  )

  expect_error(
    parameter_scan(
      C,
      parameter = "a",
      values = c(
        2,
        3
      ),
      f = function(x) x^2,
      grid = seq(
        0,
        1,
        length.out = 11
      ),
      n = 0
    ),
    "`n` must be one positive integer"
  )

  expect_error(
    parameter_scan(
      C,
      parameter = "a",
      values = c(
        2,
        3
      ),
      f = function(x) x^2,
      grid = seq(
        0,
        1,
        length.out = 11
      ),
      n = 10.5
    ),
    "`n` must be one positive integer"
  )

  expect_error(
    parameter_scan(
      C,
      parameter = "a",
      values = c(
        2,
        3
      ),
      f = function(x) x^2,
      grid = seq(
        0,
        1,
        length.out = 11
      ),
      n = Inf
    ),
    "`n` must be one positive integer"
  )
})
