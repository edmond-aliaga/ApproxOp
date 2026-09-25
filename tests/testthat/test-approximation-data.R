test_that("approximation_data returns the correct object structure", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  D <- approximation_data(
    B,
    f = function(x) x^2,
    x = c(0.2, 0.5, 0.8),
    n = c(10, 20, 50)
  )

  expect_s3_class(D, "approximation_data")
  expect_s3_class(D, "data.frame")

  expect_equal(
    names(D),
    c(
      "x",
      "n",
      "exact",
      "approximation",
      "signed_error",
      "absolute_error",
      "relative_error"
    )
  )

  expect_equal(nrow(D), 9)
})


test_that("approximation_data gives correct Bernstein values", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  x <- c(0.2, 0.5, 0.8)
  n_values <- c(10, 20, 50)

  D <- approximation_data(
    B,
    f = function(x) x^2,
    x = x,
    n = n_values
  )

  for (ni in n_values) {

    current <- D[D$n == ni, ]

    expected_exact <- x^2

    expected_approximation <-
      x^2 + x * (1 - x) / ni

    expected_error <-
      x * (1 - x) / ni

    expect_equal(
      current$exact,
      expected_exact,
      tolerance = 1e-12
    )

    expect_equal(
      current$approximation,
      expected_approximation,
      tolerance = 1e-12
    )

    expect_equal(
      current$signed_error,
      expected_error,
      tolerance = 1e-12
    )

    expect_equal(
      current$absolute_error,
      expected_error,
      tolerance = 1e-12
    )
  }
})


test_that("approximation_data computes relative errors correctly", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  D <- approximation_data(
    B,
    f = function(x) x^2,
    x = c(0.2, 0.5, 0.8),
    n = 10
  )

  expected <- c(
    0.016 / 0.04,
    0.025 / 0.25,
    0.016 / 0.64
  )

  expect_equal(
    D$relative_error,
    expected,
    tolerance = 1e-12
  )
})


test_that("approximation_data returns NA relative error when exact value is zero", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  D <- approximation_data(
    B,
    f = function(x) x,
    x = c(0, 0.5),
    n = 10
  )

  expect_true(
    is.na(D$relative_error[1])
  )

  expect_false(
    is.na(D$relative_error[2])
  )
})


test_that("approximation_data uses operator n when n is NULL", {

  B <- approx_operator(
    family = "bernstein",
    n = 20
  )

  D <- approximation_data(
    B,
    f = function(x) x^2,
    x = c(0.2, 0.5)
  )

  expect_equal(
    unique(D$n),
    20L
  )

  expect_equal(
    attr(D, "n_values"),
    20L
  )
})


test_that("approximation_data stores operator metadata", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 10,
    params = list(
      a = 2
    )
  )

  D <- approximation_data(
    C,
    f = function(x) x,
    x = c(0.25, 0.5),
    n = c(10, 20)
  )

  expect_equal(
    attr(D, "operator_family"),
    "sheffer"
  )

  expect_equal(
    attr(D, "operator_subfamily"),
    "charlier"
  )

  expect_equal(
    attr(D, "operator_variant"),
    "discrete"
  )

  expect_equal(
    attr(D, "n_values"),
    c(10L, 20L)
  )
})


test_that("approximation_data rejects invalid n values", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  expect_error(
    approximation_data(
      B,
      f = function(x) x^2,
      x = 0.5,
      n = 0
    ),
    "`n` must contain positive integers"
  )

  expect_error(
    approximation_data(
      B,
      f = function(x) x^2,
      x = 0.5,
      n = 10.5
    ),
    "`n` must contain positive integers"
  )

  expect_error(
    approximation_data(
      B,
      f = function(x) x^2,
      x = 0.5,
      n = NA_real_
    ),
    "`n` must contain positive integers"
  )
})


test_that("approximation_data rejects invalid evaluation points", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  expect_error(
    approximation_data(
      B,
      f = function(x) x^2,
      x = numeric(0)
    ),
    "`x` must be a non-empty vector"
  )

  expect_error(
    approximation_data(
      B,
      f = function(x) x^2,
      x = NA_real_
    ),
    "`x` must be a non-empty vector"
  )

  expect_error(
    approximation_data(
      B,
      f = function(x) x^2,
      x = Inf
    ),
    "`x` must be a non-empty vector"
  )
})


test_that("approximation_data rejects points outside operator domain", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  expect_error(
    approximation_data(
      B,
      f = function(x) x^2,
      x = c(0.5, 1.1)
    ),
    "operator domain"
  )
})


test_that("approximation_data rejects invalid functions", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  expect_error(
    approximation_data(
      B,
      f = 5,
      x = 0.5
    ),
    "`f` must be a function"
  )

  expect_error(
    approximation_data(
      B,
      f = function(x) c(x, x),
      x = 0.5
    ),
    "`f` must return one finite numeric value"
  )

  expect_error(
    approximation_data(
      B,
      f = function(x) Inf,
      x = 0.5
    ),
    "`f` must return one finite numeric value"
  )
})


test_that("approximation_data works with a Sheffer subfamily", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 10,
    params = list(
      a = 2
    )
  )

  D <- approximation_data(
    C,
    f = function(x) x,
    x = c(0.25, 0.5, 1),
    n = c(10, 20)
  )

  expect_s3_class(
    D,
    "approximation_data"
  )

  expect_equal(
    nrow(D),
    6
  )

  expect_true(
    all(is.finite(D$approximation))
  )

  expect_true(
    all(D$absolute_error >= 0)
  )
})
