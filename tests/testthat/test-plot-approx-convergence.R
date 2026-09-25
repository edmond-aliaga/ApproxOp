test_that("plot.approx_convergence works with the default metric", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  C <- convergence(
    B,
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 101),
    n = c(5, 10, 20, 50, 100)
  )

  result <- plot(C)

  expect_s3_class(
    result,
    "approx_convergence"
  )

  expect_equal(
    result,
    C
  )
})


test_that("plot.approx_convergence works for maximum absolute error", {

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

  result <- plot(
    C,
    metric = "max_absolute_error"
  )

  expect_s3_class(
    result,
    "approx_convergence"
  )

  expect_equal(
    result$max_absolute_error,
    C$max_absolute_error
  )
})


test_that("plot.approx_convergence works for MAE", {

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

  result <- plot(
    C,
    metric = "mae"
  )

  expect_s3_class(
    result,
    "approx_convergence"
  )

  expect_equal(
    result$mae,
    C$mae
  )
})


test_that("plot.approx_convergence works for RMSE", {

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

  result <- plot(
    C,
    metric = "rmse"
  )

  expect_s3_class(
    result,
    "approx_convergence"
  )

  expect_equal(
    result$rmse,
    C$rmse
  )
})


test_that("plot.approx_convergence works with show_n TRUE", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  C <- convergence(
    B,
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 101),
    n = c(5, 10, 20, 50, 100)
  )

  result <- plot(
    C,
    show_n = TRUE
  )

  expect_s3_class(
    result,
    "approx_convergence"
  )

  expect_equal(
    result$n,
    c(
      5L,
      10L,
      20L,
      50L,
      100L
    )
  )
})


test_that("plot.approx_convergence works with show_n FALSE", {

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

  result <- plot(
    C,
    show_n = FALSE
  )

  expect_s3_class(
    result,
    "approx_convergence"
  )
})


test_that("plot.approx_convergence accepts custom graphical arguments", {

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

  result <- plot(
    C,
    metric = "mae",
    xlab = "Degree n",
    ylab = "Error",
    main = "Convergence",
    type = "o",
    lwd = 3,
    pch = 16
  )

  expect_s3_class(
    result,
    "approx_convergence"
  )
})


test_that("plot.approx_convergence rejects invalid metrics", {

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

  expect_error(
    plot(
      C,
      metric = "unknown"
    )
  )
})


test_that("plot.approx_convergence rejects invalid line widths", {

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

  expect_error(
    plot(
      C,
      lwd = 0
    ),
    "`lwd` must be one positive finite numeric value"
  )

  expect_error(
    plot(
      C,
      lwd = -1
    ),
    "`lwd` must be one positive finite numeric value"
  )

  expect_error(
    plot(
      C,
      lwd = Inf
    ),
    "`lwd` must be one positive finite numeric value"
  )
})


test_that("plot.approx_convergence rejects invalid plotting symbols", {

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

  expect_error(
    plot(
      C,
      pch = NA_real_
    ),
    "`pch` must be NULL or one finite numeric value"
  )

  expect_error(
    plot(
      C,
      pch = Inf
    ),
    "`pch` must be NULL or one finite numeric value"
  )

  expect_error(
    plot(
      C,
      pch = c(16, 17)
    ),
    "`pch` must be NULL or one finite numeric value"
  )
})


test_that("plot.approx_convergence rejects invalid plot types", {

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

  expect_error(
    plot(
      C,
      type = c("b", "l")
    ),
    "`type` must be a single character value"
  )

  expect_error(
    plot(
      C,
      type = NA_character_
    ),
    "`type` must be a single character value"
  )
})


test_that("plot.approx_convergence rejects invalid show_n values", {

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

  expect_error(
    plot(
      C,
      show_n = 1
    ),
    "`show_n` must be TRUE or FALSE"
  )

  expect_error(
    plot(
      C,
      show_n = NA
    ),
    "`show_n` must be TRUE or FALSE"
  )

  expect_error(
    plot(
      C,
      show_n = c(TRUE, FALSE)
    ),
    "`show_n` must be TRUE or FALSE"
  )
})


test_that("plot.approx_convergence preserves convergence metadata", {

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

  result <- plot(C)

  expect_equal(
    attr(result, "operator_family"),
    "bernstein"
  )

  expect_null(
    attr(result, "operator_subfamily")
  )

  expect_equal(
    attr(result, "operator_variant"),
    "discrete"
  )

  expect_equal(
    attr(result, "grid_size"),
    101L
  )

  expect_equal(
    attr(result, "grid_range"),
    c(0, 1)
  )
})


test_that("plot.approx_convergence works for a Sheffer subfamily", {

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

  result <- plot(
    C,
    metric = "max_absolute_error"
  )

  expect_s3_class(
    result,
    "approx_convergence"
  )

  expect_equal(
    attr(result, "operator_family"),
    "sheffer"
  )

  expect_equal(
    attr(result, "operator_subfamily"),
    "charlier"
  )
})
