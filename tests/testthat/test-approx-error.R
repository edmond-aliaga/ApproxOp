test_that("approx_error returns the correct object structure", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  E <- approx_error(
    B,
    f = function(x) x^2,
    x = c(0.2, 0.5, 0.8)
  )

  expect_s3_class(E, "approx_error")
  expect_s3_class(E, "data.frame")

  expect_equal(
    names(E),
    c(
      "x",
      "exact",
      "approximation",
      "signed_error",
      "absolute_error",
      "relative_error"
    )
  )

  expect_equal(nrow(E), 3)
})


test_that("approx_error gives correct Bernstein errors", {

  n <- 10

  B <- approx_operator(
    family = "bernstein",
    n = n
  )

  x <- c(0.2, 0.5, 0.8)

  E <- approx_error(
    B,
    f = function(x) x^2,
    x = x
  )

  exact <- x^2

  expected_approximation <-
    x^2 + x * (1 - x) / n

  expected_error <-
    x * (1 - x) / n

  expect_equal(
    E$exact,
    exact,
    tolerance = 1e-12
  )

  expect_equal(
    E$approximation,
    expected_approximation,
    tolerance = 1e-12
  )

  expect_equal(
    E$signed_error,
    expected_error,
    tolerance = 1e-12
  )

  expect_equal(
    E$absolute_error,
    expected_error,
    tolerance = 1e-12
  )
})


test_that("approx_error computes relative errors correctly", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  E <- approx_error(
    B,
    f = function(x) x^2,
    x = c(0.2, 0.5, 0.8)
  )

  expected <- c(
    0.4,
    0.1,
    0.025
  )

  expect_equal(
    E$relative_error,
    expected,
    tolerance = 1e-12
  )
})


test_that("approx_error returns NA relative error when exact value is zero", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  E <- approx_error(
    B,
    f = function(x) x^2,
    x = c(0, 0.5)
  )

  expect_true(
    is.na(E$relative_error[1])
  )

  expect_false(
    is.na(E$relative_error[2])
  )
})


test_that("approx_error stores correct summary measures", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  E <- approx_error(
    B,
    f = function(x) x^2,
    x = c(0.2, 0.5, 0.8)
  )

  errors <- c(
    0.016,
    0.025,
    0.016
  )

  expect_equal(
    attr(E, "max_absolute_error"),
    max(errors),
    tolerance = 1e-12
  )

  expect_equal(
    attr(E, "mae"),
    mean(errors),
    tolerance = 1e-12
  )

  expect_equal(
    attr(E, "rmse"),
    sqrt(mean(errors^2)),
    tolerance = 1e-12
  )
})


test_that("approx_error stores operator metadata", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  E <- approx_error(
    B,
    f = function(x) x^2,
    x = 0.5
  )

  expect_equal(
    attr(E, "operator_family"),
    "bernstein"
  )

  expect_null(
    attr(E, "operator_subfamily")
  )

  expect_equal(
    attr(E, "operator_variant"),
    "discrete"
  )

  expect_equal(
    attr(E, "n"),
    10
  )
})


test_that("approx_error works with a Sheffer subfamily", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 10,
    params = list(
      a = 2
    )
  )

  E <- approx_error(
    C,
    f = function(x) x,
    x = 0.5
  )

  expect_equal(
    E$exact,
    0.5,
    tolerance = 1e-10
  )

  expect_equal(
    E$approximation,
    0.6,
    tolerance = 1e-10
  )

  expect_equal(
    E$signed_error,
    0.1,
    tolerance = 1e-10
  )

  expect_equal(
    E$absolute_error,
    0.1,
    tolerance = 1e-10
  )

  expect_equal(
    attr(E, "operator_family"),
    "sheffer"
  )

  expect_equal(
    attr(E, "operator_subfamily"),
    "charlier"
  )
})


test_that("approx_error rejects invalid f", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  expect_error(
    approx_error(
      B,
      f = 1,
      x = 0.5
    ),
    "`f` must be a function"
  )
})


test_that("approx_error rejects invalid evaluation points", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  expect_error(
    approx_error(
      B,
      f = function(x) x^2,
      x = NA_real_
    ),
    "`x` must be a non-empty vector of finite numeric values"
  )

  expect_error(
    approx_error(
      B,
      f = function(x) x^2,
      x = Inf
    ),
    "`x` must be a non-empty vector of finite numeric values"
  )
})


test_that("approx_error rejects points outside the operator domain", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  expect_error(
    approx_error(
      B,
      f = function(x) x^2,
      x = -0.1
    ),
    "operator domain"
  )

  expect_error(
    approx_error(
      B,
      f = function(x) x^2,
      x = 1.1
    ),
    "operator domain"
  )
})


test_that("approx_error requires finite scalar function values", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  expect_error(
    approx_error(
      B,
      f = function(x) c(x, x^2),
      x = 0.5
    ),
    "one finite numeric value"
  )

  expect_error(
    approx_error(
      B,
      f = function(x) Inf,
      x = 0.5
    ),
    "one finite numeric value"
  )
})
