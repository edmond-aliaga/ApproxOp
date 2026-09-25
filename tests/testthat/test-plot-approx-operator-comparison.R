test_that("plot.approx_operator_comparison works with default metric", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  S <- approx_operator(
    family = "szasz",
    n = 10
  )

  Comp <- compare_operators(
    operators = list(
      Bernstein = B,
      Szasz_Mirakyan = S
    ),
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 51),
    n = c(10, 20, 50)
  )

  result <- plot(Comp)

  expect_s3_class(
    result,
    "approx_operator_comparison"
  )

  expect_equal(
    result,
    Comp
  )
})


test_that("plot.approx_operator_comparison works for all metrics", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  S <- approx_operator(
    family = "szasz",
    n = 10
  )

  Comp <- compare_operators(
    operators = list(
      Bernstein = B,
      Szasz_Mirakyan = S
    ),
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 51),
    n = c(10, 20, 50)
  )

  metrics <- c(
    "max_absolute_error",
    "mae",
    "rmse"
  )

  for (current_metric in metrics) {

    result <- plot(
      Comp,
      metric = current_metric
    )

    expect_s3_class(
      result,
      "approx_operator_comparison"
    )
  }
})


test_that("plot.approx_operator_comparison preserves numerical data", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  S <- approx_operator(
    family = "szasz",
    n = 10
  )

  Comp <- compare_operators(
    operators = list(
      Bernstein = B,
      Szasz_Mirakyan = S
    ),
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 51),
    n = c(10, 20, 50)
  )

  result <- plot(Comp)

  expect_equal(
    result$n,
    Comp$n
  )

  expect_equal(
    result$max_absolute_error,
    Comp$max_absolute_error
  )

  expect_equal(
    result$mae,
    Comp$mae
  )

  expect_equal(
    result$rmse,
    Comp$rmse
  )
})


test_that("plot.approx_operator_comparison works with show_n TRUE and FALSE", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  S <- approx_operator(
    family = "szasz",
    n = 10
  )

  Comp <- compare_operators(
    operators = list(
      Bernstein = B,
      Szasz_Mirakyan = S
    ),
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 51),
    n = c(10, 20, 50)
  )

  result_true <- plot(
    Comp,
    show_n = TRUE
  )

  result_false <- plot(
    Comp,
    show_n = FALSE
  )

  expect_s3_class(
    result_true,
    "approx_operator_comparison"
  )

  expect_s3_class(
    result_false,
    "approx_operator_comparison"
  )
})


test_that("plot.approx_operator_comparison works with and without legend", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  S <- approx_operator(
    family = "szasz",
    n = 10
  )

  Comp <- compare_operators(
    operators = list(
      Bernstein = B,
      Szasz_Mirakyan = S
    ),
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 51),
    n = c(10, 20, 50)
  )

  result_with_legend <- plot(
    Comp,
    show_legend = TRUE
  )

  result_without_legend <- plot(
    Comp,
    show_legend = FALSE
  )

  expect_s3_class(
    result_with_legend,
    "approx_operator_comparison"
  )

  expect_s3_class(
    result_without_legend,
    "approx_operator_comparison"
  )
})


test_that("plot.approx_operator_comparison accepts custom graphical arguments", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  S <- approx_operator(
    family = "szasz",
    n = 10
  )

  Comp <- compare_operators(
    operators = list(
      Bernstein = B,
      Szasz_Mirakyan = S
    ),
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 51),
    n = c(10, 20, 50)
  )

  result <- plot(
    Comp,
    metric = "mae",
    type = "o",
    lwd = 2,
    pch = 15,
    xlab = "Degree n",
    ylab = "Approximation error",
    main = "Operator comparison"
  )

  expect_s3_class(
    result,
    "approx_operator_comparison"
  )
})


test_that("plot.approx_operator_comparison works for three operators", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  S <- approx_operator(
    family = "szasz",
    n = 10
  )

  V <- approx_operator(
    family = "baskakov",
    n = 10
  )

  Comp <- compare_operators(
    operators = list(
      Bernstein = B,
      Szasz_Mirakyan = S,
      Baskakov = V
    ),
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 51),
    n = c(10, 20, 50)
  )

  result <- plot(
    Comp,
    metric = "max_absolute_error"
  )

  expect_s3_class(
    result,
    "approx_operator_comparison"
  )

  expect_equal(
    unique(result$operator),
    c(
      "Bernstein",
      "Szasz_Mirakyan",
      "Baskakov"
    )
  )
})


test_that("plot.approx_operator_comparison works for Sheffer subfamilies", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 10,
    params = list(
      a = 2
    )
  )

  Comp <- compare_operators(
    operators = list(
      Bernstein = B,
      Charlier = C
    ),
    f = function(x) x,
    grid = seq(0, 1, length.out = 31),
    n = c(10, 20)
  )

  result <- plot(
    Comp,
    metric = "rmse"
  )

  expect_s3_class(
    result,
    "approx_operator_comparison"
  )

  expect_true(
    "Charlier" %in% result$operator
  )
})


test_that("plot.approx_operator_comparison rejects invalid metrics", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  S <- approx_operator(
    family = "szasz",
    n = 10
  )

  Comp <- compare_operators(
    operators = list(B, S),
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 21),
    n = c(10, 20)
  )

  expect_error(
    plot(
      Comp,
      metric = "unknown"
    )
  )
})


test_that("plot.approx_operator_comparison rejects invalid line widths", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  S <- approx_operator(
    family = "szasz",
    n = 10
  )

  Comp <- compare_operators(
    operators = list(B, S),
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 21),
    n = c(10, 20)
  )

  expect_error(
    plot(
      Comp,
      lwd = 0
    ),
    "`lwd` must be one positive finite numeric value"
  )

  expect_error(
    plot(
      Comp,
      lwd = -1
    ),
    "`lwd` must be one positive finite numeric value"
  )

  expect_error(
    plot(
      Comp,
      lwd = Inf
    ),
    "`lwd` must be one positive finite numeric value"
  )
})


test_that("plot.approx_operator_comparison rejects invalid plotting symbols", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  S <- approx_operator(
    family = "szasz",
    n = 10
  )

  Comp <- compare_operators(
    operators = list(B, S),
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 21),
    n = c(10, 20)
  )

  expect_error(
    plot(
      Comp,
      pch = NA_real_
    ),
    "`pch` must be NULL or one finite numeric value"
  )

  expect_error(
    plot(
      Comp,
      pch = Inf
    ),
    "`pch` must be NULL or one finite numeric value"
  )

  expect_error(
    plot(
      Comp,
      pch = c(16, 17)
    ),
    "`pch` must be NULL or one finite numeric value"
  )
})


test_that("plot.approx_operator_comparison rejects invalid show_n values", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  S <- approx_operator(
    family = "szasz",
    n = 10
  )

  Comp <- compare_operators(
    operators = list(B, S),
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 21),
    n = c(10, 20)
  )

  expect_error(
    plot(
      Comp,
      show_n = 1
    ),
    "`show_n` must be TRUE or FALSE"
  )

  expect_error(
    plot(
      Comp,
      show_n = NA
    ),
    "`show_n` must be TRUE or FALSE"
  )

  expect_error(
    plot(
      Comp,
      show_n = c(TRUE, FALSE)
    ),
    "`show_n` must be TRUE or FALSE"
  )
})


test_that("plot.approx_operator_comparison rejects invalid show_legend values", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  S <- approx_operator(
    family = "szasz",
    n = 10
  )

  Comp <- compare_operators(
    operators = list(B, S),
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 21),
    n = c(10, 20)
  )

  expect_error(
    plot(
      Comp,
      show_legend = 1
    ),
    "`show_legend` must be TRUE or FALSE"
  )

  expect_error(
    plot(
      Comp,
      show_legend = NA
    ),
    "`show_legend` must be TRUE or FALSE"
  )

  expect_error(
    plot(
      Comp,
      show_legend = c(TRUE, FALSE)
    ),
    "`show_legend` must be TRUE or FALSE"
  )
})


test_that("plot.approx_operator_comparison preserves comparison metadata", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  S <- approx_operator(
    family = "szasz",
    n = 10
  )

  Comp <- compare_operators(
    operators = list(
      Bernstein = B,
      Szasz_Mirakyan = S
    ),
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 51),
    n = c(10, 20, 50)
  )

  result <- plot(Comp)

  expect_equal(
    attr(result, "grid_size"),
    51L
  )

  expect_equal(
    attr(result, "grid_range"),
    c(0, 1)
  )

  expect_equal(
    attr(result, "operator_count"),
    2L
  )

  expect_equal(
    attr(result, "operator_names"),
    c(
      "Bernstein",
      "Szasz_Mirakyan"
    )
  )

  expect_equal(
    attr(result, "n_values"),
    c(10L, 20L, 50L)
  )
})
