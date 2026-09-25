test_that("error_table returns the correct object structure", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  T <- error_table(
    B,
    f = function(x) x^2,
    x = c(0.2, 0.5, 0.8),
    n = c(10, 20, 50, 100)
  )

  expect_s3_class(T, "approx_error_table")
  expect_s3_class(T, "data.frame")

  expect_equal(
    names(T),
    c(
      "x",
      "exact",
      "error_n10",
      "error_n20",
      "error_n50",
      "error_n100"
    )
  )

  expect_equal(nrow(T), 3)
})


test_that("error_table gives correct Bernstein errors", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  x <- c(0.2, 0.5, 0.8)

  T <- error_table(
    B,
    f = function(x) x^2,
    x = x,
    n = c(10, 20, 50, 100)
  )

  expect_equal(
    T$exact,
    x^2,
    tolerance = 1e-12
  )

  expect_equal(
    T$error_n10,
    x * (1 - x) / 10,
    tolerance = 1e-12
  )

  expect_equal(
    T$error_n20,
    x * (1 - x) / 20,
    tolerance = 1e-12
  )

  expect_equal(
    T$error_n50,
    x * (1 - x) / 50,
    tolerance = 1e-12
  )

  expect_equal(
    T$error_n100,
    x * (1 - x) / 100,
    tolerance = 1e-12
  )
})


test_that("error_table uses operator n when n is NULL", {

  B <- approx_operator(
    family = "bernstein",
    n = 20
  )

  T <- error_table(
    B,
    f = function(x) x^2,
    x = 0.5
  )

  expect_true(
    "error_n20" %in% names(T)
  )

  expect_equal(
    T$error_n20,
    0.5 * 0.5 / 20,
    tolerance = 1e-12
  )

  expect_equal(
    attr(T, "n_values"),
    20L
  )
})


test_that("error_table stores correct summary measures", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  x <- c(0.2, 0.5, 0.8)

  T <- error_table(
    B,
    f = function(x) x^2,
    x = x,
    n = c(10, 20)
  )

  S <- attr(
    T,
    "summary"
  )

  expect_s3_class(
    S,
    "data.frame"
  )

  expect_equal(
    names(S),
    c(
      "n",
      "max_absolute_error",
      "mae",
      "rmse"
    )
  )

  error10 <- x * (1 - x) / 10
  error20 <- x * (1 - x) / 20

  expect_equal(
    S$max_absolute_error,
    c(
      max(error10),
      max(error20)
    ),
    tolerance = 1e-12
  )

  expect_equal(
    S$mae,
    c(
      mean(error10),
      mean(error20)
    ),
    tolerance = 1e-12
  )

  expect_equal(
    S$rmse,
    c(
      sqrt(mean(error10^2)),
      sqrt(mean(error20^2))
    ),
    tolerance = 1e-12
  )
})


test_that("error_table stores operator metadata", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 10,
    params = list(
      a = 2
    )
  )

  T <- error_table(
    C,
    f = function(x) x,
    x = c(0.25, 0.5),
    n = c(10, 20)
  )

  expect_equal(
    attr(T, "operator_family"),
    "sheffer"
  )

  expect_equal(
    attr(T, "operator_subfamily"),
    "charlier"
  )

  expect_equal(
    attr(T, "operator_variant"),
    "discrete"
  )

  expect_equal(
    attr(T, "n_values"),
    c(10L, 20L)
  )
})


test_that("error_table stores digits correctly", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  T <- error_table(
    B,
    f = function(x) x^2,
    x = 0.5,
    digits = 4
  )

  expect_equal(
    attr(T, "digits"),
    4L
  )
})


test_that("error_table rejects invalid n values", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  expect_error(
    error_table(
      B,
      f = function(x) x^2,
      x = 0.5,
      n = 0
    ),
    "`n` must contain positive integers"
  )

  expect_error(
    error_table(
      B,
      f = function(x) x^2,
      x = 0.5,
      n = 10.5
    ),
    "`n` must contain positive integers"
  )

  expect_error(
    error_table(
      B,
      f = function(x) x^2,
      x = 0.5,
      n = NA_real_
    ),
    "`n` must contain positive integers"
  )
})


test_that("error_table rejects invalid digits", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  expect_error(
    error_table(
      B,
      f = function(x) x^2,
      x = 0.5,
      digits = -1
    ),
    "`digits` must be a non-negative integer"
  )

  expect_error(
    error_table(
      B,
      f = function(x) x^2,
      x = 0.5,
      digits = 2.5
    ),
    "`digits` must be a non-negative integer"
  )
})


test_that("error_table rejects points outside the operator domain", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  expect_error(
    error_table(
      B,
      f = function(x) x^2,
      x = c(0.5, 1.1)
    ),
    "operator domain"
  )
})


test_that("print.approx_error_table uses table separators", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  T <- error_table(
    B,
    f = function(x) x^2,
    x = c(0.2, 0.5),
    n = c(10, 20),
    digits = 6
  )

  output <- capture.output(
    print(T)
  )

  expect_true(
    any(
      grepl(
        "^\\+[-+]+\\+$",
        output
      )
    )
  )

  expect_true(
    any(
      grepl(
        "\\|",
        output
      )
    )
  )

  expect_true(
    any(
      grepl(
        "error_n10",
        output,
        fixed = TRUE
      )
    )
  )

  expect_true(
    any(
      grepl(
        "error_n20",
        output,
        fixed = TRUE
      )
    )
  )
})


test_that("print.approx_error_table respects digits", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  T <- error_table(
    B,
    f = function(x) x^2,
    x = 0.5,
    digits = 4
  )

  output <- capture.output(
    print(T)
  )

  expect_true(
    any(
      grepl(
        "0.5000",
        output,
        fixed = TRUE
      )
    )
  )

  expect_true(
    any(
      grepl(
        "0.0250",
        output,
        fixed = TRUE
      )
    )
  )
})
