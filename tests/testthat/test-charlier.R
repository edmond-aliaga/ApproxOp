test_that("Charlier operator can be constructed", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 10,
    params = list(
      a = 2
    )
  )

  expect_s3_class(C, "approx_operator")
  expect_equal(C$family, "sheffer")
  expect_equal(C$subfamily, "charlier")
  expect_equal(C$n, 10)
  expect_equal(C$variant, "discrete")
  expect_equal(C$domain, c(0, Inf))
})


test_that("Charlier reproduces the constant function", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 10,
    params = list(
      a = 2
    )
  )

  value <- approximate(
    C,
    f = function(t) 1,
    x = 0.5
  )

  expect_equal(
    value,
    1,
    tolerance = 1e-10
  )
})


test_that("Charlier first moment is correct", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 10,
    params = list(
      a = 2
    )
  )

  x <- 0.5

  value <- approximate(
    C,
    f = function(t) t,
    x = x
  )

  expected <- x + 1 / 10

  expect_equal(
    value,
    expected,
    tolerance = 1e-10
  )
})


test_that("Charlier second moment is correct", {

  n <- 10
  a <- 2
  x <- 0.5

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = n,
    params = list(
      a = a
    )
  )

  value <- approximate(
    C,
    f = function(t) t^2,
    x = x
  )

  expected <-
    x^2 +
    (x / n) *
    (
      3 +
        1 / (a - 1)
    ) +
    2 / n^2

  expect_equal(
    value,
    expected,
    tolerance = 1e-10
  )
})


test_that("Charlier moments function works correctly", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 10,
    params = list(
      a = 2
    )
  )

  M <- moments(
    C,
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
    0.6,
    tolerance = 1e-10
  )

  expect_equal(
    M[3],
    0.47,
    tolerance = 1e-10
  )
})


test_that("Charlier works for vector evaluation points", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 10,
    params = list(
      a = 2
    )
  )

  x <- c(
    0,
    0.25,
    0.5,
    1
  )

  value <- approximate(
    C,
    f = function(t) 1,
    x = x
  )

  expect_equal(
    value,
    rep(1, length(x)),
    tolerance = 1e-10
  )
})


test_that("Charlier first moment works for vector inputs", {

  n <- 10

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = n,
    params = list(
      a = 2
    )
  )

  x <- c(
    0,
    0.25,
    0.5,
    1
  )

  value <- approximate(
    C,
    f = function(t) t,
    x = x
  )

  expected <- x + 1 / n

  expect_equal(
    value,
    expected,
    tolerance = 1e-10
  )
})


test_that("Charlier requires parameter a", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 10
  )

  expect_error(
    approximate(
      C,
      f = function(t) t,
      x = 0.5
    ),
    "`parameters\\$a` must be supplied"
  )
})


test_that("Charlier requires a greater than one", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 10,
    params = list(
      a = 1
    )
  )

  expect_error(
    approximate(
      C,
      f = function(t) t,
      x = 0.5
    ),
    "must satisfy `a > 1`"
  )
})


test_that("Charlier rejects parameter a below one", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 10,
    params = list(
      a = 0.5
    )
  )

  expect_error(
    approximate(
      C,
      f = function(t) t,
      x = 0.5
    ),
    "must satisfy `a > 1`"
  )
})


test_that("Charlier rejects points outside its domain", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 10,
    params = list(
      a = 2
    )
  )

  expect_error(
    approximate(
      C,
      f = function(t) t,
      x = -0.1
    ),
    "operator domain"
  )
})


test_that("Charlier gives correct moments at x equal to zero", {

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 10,
    params = list(
      a = 2
    )
  )

  m0 <- approximate(
    C,
    f = function(t) 1,
    x = 0
  )

  m1 <- approximate(
    C,
    f = function(t) t,
    x = 0
  )

  m2 <- approximate(
    C,
    f = function(t) t^2,
    x = 0
  )

  expect_equal(
    m0,
    1,
    tolerance = 1e-10
  )

  expect_equal(
    m1,
    0.1,
    tolerance = 1e-10
  )

  expect_equal(
    m2,
    0.02,
    tolerance = 1e-10
  )
})


test_that("Charlier second moment works for another parameter value", {

  n <- 20
  a <- 3
  x <- 0.75

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = n,
    params = list(
      a = a
    )
  )

  value <- approximate(
    C,
    f = function(t) t^2,
    x = x
  )

  expected <-
    x^2 +
    (x / n) *
    (
      3 +
        1 / (a - 1)
    ) +
    2 / n^2

  expect_equal(
    value,
    expected,
    tolerance = 1e-10
  )
})
