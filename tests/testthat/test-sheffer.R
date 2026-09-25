test_that("General Sheffer operator can be constructed", {

  S <- approx_operator(
    family = "sheffer",
    n = 10,
    params = list(
      A = function(t) 1,
      H = function(t) t,
      Hprime1 = 1,
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

  expect_s3_class(S, "approx_operator")
  expect_equal(S$family, "sheffer")
  expect_null(S$subfamily)
  expect_equal(S$n, 10)
  expect_equal(S$variant, "discrete")
  expect_equal(S$domain, c(0, Inf))
})


test_that("General Sheffer reproduces the constant function", {

  S <- approx_operator(
    family = "sheffer",
    n = 10,
    params = list(
      A = function(t) 1,
      H = function(t) t,
      Hprime1 = 1,
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
    S,
    f = function(t) 1,
    x = 0.5
  )

  expect_equal(
    value,
    1,
    tolerance = 1e-10
  )
})


test_that("General Sheffer reproduces Szasz-Mirakyan moments", {

  S <- approx_operator(
    family = "sheffer",
    n = 10,
    params = list(
      A = function(t) 1,
      H = function(t) t,
      Hprime1 = 1,
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
    S,
    f = function(t) 1,
    x = x
  )

  m1 <- approximate(
    S,
    f = function(t) t,
    x = x
  )

  m2 <- approximate(
    S,
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


test_that("General Sheffer agrees with Szasz-Mirakyan", {

  G <- approx_operator(
    family = "sheffer",
    n = 10,
    params = list(
      A = function(t) 1,
      H = function(t) t,
      Hprime1 = 1,
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

  x <- c(0, 0.25, 0.5, 1)

  f <- function(t) {
    exp(-t) * sin(t)
  }

  generic_value <- approximate(
    G,
    f = f,
    x = x
  )

  szasz_value <- approximate(
    S,
    f = f,
    x = x
  )

  expect_equal(
    generic_value,
    szasz_value,
    tolerance = 1e-9
  )
})


test_that("General Sheffer requires Hprime1 equal to one", {

  S <- approx_operator(
    family = "sheffer",
    n = 10,
    params = list(
      A = function(t) 1,
      H = function(t) t,
      Hprime1 = 0.9,
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
      S,
      f = function(t) t,
      x = 0.5
    ),
    "requires `Hprime1 = 1`"
  )
})


test_that("General Sheffer requires Hprime1 to be supplied", {

  S <- approx_operator(
    family = "sheffer",
    n = 10,
    params = list(
      A = function(t) 1,
      H = function(t) t,
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
      S,
      f = function(t) t,
      x = 0.5
    ),
    "`parameters\\$Hprime1` must be supplied"
  )
})
