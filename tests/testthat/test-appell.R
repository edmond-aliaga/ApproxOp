test_that("Appell operator can be constructed", {

  Aop <- approx_operator(
    family = "sheffer",
    subfamily = "appell",
    n = 10,
    params = list(
      A = function(t) 1,
      coefficients = function(x, k) {

        if (x == 0) {
          return(if (k == 0) 1 else 0)
        }

        exp(
          k * log(x) -
            lgamma(k + 1)
        )
      }
    )
  )

  expect_s3_class(Aop, "approx_operator")
  expect_equal(Aop$family, "sheffer")
  expect_equal(Aop$subfamily, "appell")
  expect_equal(Aop$n, 10)
  expect_equal(Aop$variant, "discrete")
  expect_equal(Aop$domain, c(0, Inf))
})


test_that("Appell reproduces the constant function", {

  Aop <- approx_operator(
    family = "sheffer",
    subfamily = "appell",
    n = 10,
    params = list(
      A = function(t) 1,
      coefficients = function(x, k) {

        if (x == 0) {
          return(if (k == 0) 1 else 0)
        }

        exp(
          k * log(x) -
            lgamma(k + 1)
        )
      }
    )
  )

  value <- approximate(
    Aop,
    f = function(t) 1,
    x = 0.5
  )

  expect_equal(
    value,
    1,
    tolerance = 1e-10
  )
})


test_that("Appell with A(t)=1 reproduces Szasz-Mirakyan moments", {

  Aop <- approx_operator(
    family = "sheffer",
    subfamily = "appell",
    n = 10,
    params = list(
      A = function(t) 1,
      coefficients = function(x, k) {

        if (x == 0) {
          return(if (k == 0) 1 else 0)
        }

        exp(
          k * log(x) -
            lgamma(k + 1)
        )
      }
    )
  )

  x <- 0.5

  m0 <- approximate(
    Aop,
    f = function(t) 1,
    x = x
  )

  m1 <- approximate(
    Aop,
    f = function(t) t,
    x = x
  )

  m2 <- approximate(
    Aop,
    f = function(t) t^2,
    x = x
  )

  expect_equal(
    m0,
    1,
    tolerance = 1e-10
  )

  expect_equal(
    m1,
    x,
    tolerance = 1e-10
  )

  expect_equal(
    m2,
    x^2 + x / 10,
    tolerance = 1e-10
  )
})


test_that("Appell with A(t)=1 agrees with Szasz-Mirakyan", {

  Aop <- approx_operator(
    family = "sheffer",
    subfamily = "appell",
    n = 10,
    params = list(
      A = function(t) 1,
      coefficients = function(x, k) {

        if (x == 0) {
          return(if (k == 0) 1 else 0)
        }

        exp(
          k * log(x) -
            lgamma(k + 1)
        )
      }
    )
  )

  S <- approx_operator(
    family = "szasz",
    n = 10
  )

  x <- c(
    0,
    0.25,
    0.5,
    1
  )

  f <- function(t) {
    exp(-t) * sin(t)
  }

  appell_value <- approximate(
    Aop,
    f = f,
    x = x
  )

  szasz_value <- approximate(
    S,
    f = f,
    x = x
  )

  expect_equal(
    appell_value,
    szasz_value,
    tolerance = 1e-9
  )
})


test_that("Appell moments function works correctly", {

  Aop <- approx_operator(
    family = "sheffer",
    subfamily = "appell",
    n = 10,
    params = list(
      A = function(t) 1,
      coefficients = function(x, k) {

        if (x == 0) {
          return(if (k == 0) 1 else 0)
        }

        exp(
          k * log(x) -
            lgamma(k + 1)
        )
      }
    )
  )

  M <- moments(
    Aop,
    x = 0.5,
    order = 4
  )

  expect_equal(
    M[1],
    1,
    tolerance = 1e-10
  )

  expect_equal(
    M[2],
    0.5,
    tolerance = 1e-10
  )

  expect_equal(
    M[3],
    0.30,
    tolerance = 1e-10
  )
})


test_that("Appell rejects an invalid A(1)", {

  Aop <- approx_operator(
    family = "sheffer",
    subfamily = "appell",
    n = 10,
    params = list(
      A = function(t) 0,
      coefficients = function(x, k) {

        if (x == 0) {
          return(if (k == 0) 1 else 0)
        }

        exp(
          k * log(x) -
            lgamma(k + 1)
        )
      }
    )
  )

  expect_error(
    approximate(
      Aop,
      f = function(t) t,
      x = 0.5
    ),
    "`A\\(1\\)` must be one finite non-zero numeric value"
  )
})


test_that("Appell requires a coefficient function", {

  Aop <- approx_operator(
    family = "sheffer",
    subfamily = "appell",
    n = 10,
    params = list(
      A = function(t) 1
    )
  )

  expect_error(
    approximate(
      Aop,
      f = function(t) t,
      x = 0.5
    ),
    "`parameters\\$coefficients` must be supplied"
  )
})
