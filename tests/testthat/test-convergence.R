test_that("convergence returns the correct object structure", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  C <- convergence(
    B,
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 101),
    n = c(5, 10, 20)
  )

  expect_s3_class(
    C,
    "approx_convergence"
  )

  expect_s3_class(
    C,
    "data.frame"
  )

  expect_equal(
    names(C),
    c(
      "n",
      "max_absolute_error",
      "mae",
      "rmse"
    )
  )

  expect_equal(
    nrow(C),
    3
  )
})


test_that("convergence gives exact Bernstein maximum errors", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  n_values <- c(
    5,
    10,
    20,
    50,
    100
  )

  C <- convergence(
    B,
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 501),
    n = n_values
  )

  expect_equal(
    C$n,
    as.integer(n_values)
  )

  expect_equal(
    C$max_absolute_error,
    1 / (4 * n_values),
    tolerance = 1e-12
  )
})


test_that("convergence gives correct Bernstein MAE", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  grid <- seq(
    0,
    1,
    length.out = 501
  )

  n_values <- c(
    5,
    10,
    20
  )

  C <- convergence(
    B,
    f = function(x) x^2,
    grid = grid,
    n = n_values
  )

  expected_mae <- vapply(
    n_values,
    function(ni) {
      mean(
        grid * (1 - grid) / ni
      )
    },
    numeric(1)
  )

  expect_equal(
    C$mae,
    expected_mae,
    tolerance = 1e-12
  )
})


test_that("convergence gives correct Bernstein RMSE", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  grid <- seq(
    0,
    1,
    length.out = 501
  )

  n_values <- c(
    5,
    10,
    20
  )

  C <- convergence(
    B,
    f = function(x) x^2,
    grid = grid,
    n = n_values
  )

  expected_rmse <- vapply(
    n_values,
    function(ni) {

      error <-
        grid * (1 - grid) / ni

      sqrt(
        mean(error^2)
      )
    },
    numeric(1)
  )

  expect_equal(
    C$rmse,
    expected_rmse,
    tolerance = 1e-12
  )
})


test_that("convergence errors decrease for Bernstein", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  C <- convergence(
    B,
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 501),
    n = c(5, 10, 20, 50, 100)
  )

  expect_true(
    all(
      diff(
        C$max_absolute_error
      ) < 0
    )
  )

  expect_true(
    all(
      diff(C$mae) < 0
    )
  )

  expect_true(
    all(
      diff(C$rmse) < 0
    )
  )
})


test_that("convergence stores grid information", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  grid <- seq(
    0,
    1,
    length.out = 101
  )

  C <- convergence(
    B,
    f = function(x) x^2,
    grid = grid,
    n = c(10, 20)
  )

  expect_equal(
    attr(C, "grid_size"),
    101L
  )

  expect_equal(
    attr(C, "grid_range"),
    c(0, 1)
  )
})


test_that("convergence stores operator metadata", {

  C_op <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 10,
    params = list(
      a = 2
    )
  )

  C <- convergence(
    C_op,
    f = function(x) x,
    grid = seq(0, 1, length.out = 20),
    n = c(10, 20)
  )

  expect_equal(
    attr(C, "operator_family"),
    "sheffer"
  )

  expect_equal(
    attr(C, "operator_subfamily"),
    "charlier"
  )

  expect_equal(
    attr(C, "operator_variant"),
    "discrete"
  )
})


test_that("convergence works with a Sheffer subfamily", {

  C_op <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 10,
    params = list(
      a = 2
    )
  )

  C <- convergence(
    C_op,
    f = function(x) x,
    grid = seq(0, 1, length.out = 20),
    n = c(10, 20, 50)
  )

  expect_s3_class(
    C,
    "approx_convergence"
  )

  expect_equal(
    nrow(C),
    3
  )

  expect_true(
    all(
      is.finite(
        C$max_absolute_error
      )
    )
  )

  expect_true(
    all(C$mae >= 0)
  )

  expect_true(
    all(C$rmse >= 0)
  )
})


test_that("convergence rejects invalid n values", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  expect_error(
    convergence(
      B,
      f = function(x) x^2,
      grid = seq(0, 1, length.out = 20),
      n = 0
    ),
    "`n` must contain positive integers"
  )

  expect_error(
    convergence(
      B,
      f = function(x) x^2,
      grid = seq(0, 1, length.out = 20),
      n = 10.5
    ),
    "`n` must contain positive integers"
  )

  expect_error(
    convergence(
      B,
      f = function(x) x^2,
      grid = seq(0, 1, length.out = 20),
      n = NA_real_
    ),
    "`n` must contain positive integers"
  )
})


test_that("convergence rejects invalid grids", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  expect_error(
    convergence(
      B,
      f = function(x) x^2,
      grid = numeric(0),
      n = 10
    ),
    "`grid` must be a non-empty vector"
  )

  expect_error(
    convergence(
      B,
      f = function(x) x^2,
      grid = NA_real_,
      n = 10
    ),
    "`grid` must be a non-empty vector"
  )

  expect_error(
    convergence(
      B,
      f = function(x) x^2,
      grid = Inf,
      n = 10
    ),
    "`grid` must be a non-empty vector"
  )
})


test_that("convergence rejects points outside the operator domain", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  expect_error(
    convergence(
      B,
      f = function(x) x^2,
      grid = c(0, 0.5, 1.1),
      n = 10
    ),
    "operator domain"
  )
})


test_that("convergence rejects invalid functions", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  expect_error(
    convergence(
      B,
      f = 5,
      grid = seq(0, 1, length.out = 20),
      n = 10
    ),
    "`f` must be a function"
  )
})
