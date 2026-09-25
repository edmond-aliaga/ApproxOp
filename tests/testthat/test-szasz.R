test_that("Szász-Mirakyan operator is constructed correctly", {

  S <- approx_operator(
    family = "szasz",
    n = 10
  )

  expect_s3_class(S, "approx_operator")
  expect_equal(S$family, "szasz")
  expect_equal(S$n, 10)
  expect_equal(S$variant, "discrete")
  expect_equal(S$domain, c(0, Inf))
})


test_that("Szász-Mirakyan reproduces the constant function", {

  S <- approx_operator(
    family = "szasz",
    n = 10
  )

  x <- c(0, 0.25, 0.5, 1, 2)

  result <- approximate(
    operator = S,
    f = function(t) 1,
    x = x
  )

  expect_equal(
    unname(result),
    rep(1, length(x)),
    tolerance = 1e-10
  )
})


test_that("Szász-Mirakyan reproduces the linear function", {

  S <- approx_operator(
    family = "szasz",
    n = 10
  )

  x <- c(0, 0.25, 0.5, 1, 2)

  result <- approximate(
    operator = S,
    f = function(t) t,
    x = x
  )

  expect_equal(
    unname(result),
    x,
    tolerance = 1e-10
  )
})


test_that("Szász-Mirakyan second moment is correct", {

  n <- 10

  S <- approx_operator(
    family = "szasz",
    n = n
  )

  x <- c(0, 0.25, 0.5, 1, 2)

  result <- approximate(
    operator = S,
    f = function(t) t^2,
    x = x
  )

  expected <- x^2 + x / n

  expect_equal(
    unname(result),
    expected,
    tolerance = 1e-10
  )
})


test_that("Szász-Mirakyan ordinary moments are computed through order 8", {

  S <- approx_operator(
    family = "szasz",
    n = 10
  )

  M <- moments(
    operator = S,
    order = 8,
    x = 0.5,
    central = FALSE
  )

  expect_equal(
    dim(M),
    c(1L, 9L)
  )

  expect_equal(
    colnames(M),
    paste0("m", 0:8)
  )

  expect_equal(
    unname(M[1, 1]),
    1,
    tolerance = 1e-10
  )

  expect_equal(
    unname(M[1, 2]),
    0.5,
    tolerance = 1e-10
  )

  expect_equal(
    unname(M[1, 3]),
    0.30,
    tolerance = 1e-10
  )
})


test_that("Szász-Mirakyan central moments are correct", {

  n <- 10
  x <- 0.5

  S <- approx_operator(
    family = "szasz",
    n = n
  )

  M <- moments(
    operator = S,
    order = 8,
    x = x,
    central = TRUE
  )

  expect_equal(
    dim(M),
    c(1L, 9L)
  )

  expect_equal(
    colnames(M),
    paste0("mu", 0:8)
  )

  expect_equal(
    unname(M[1, 1]),
    1,
    tolerance = 1e-10
  )

  expect_equal(
    unname(M[1, 2]),
    0,
    tolerance = 1e-10
  )

  # mu_2(x) = x / n
  expect_equal(
    unname(M[1, 3]),
    x / n,
    tolerance = 1e-8
  )

  # mu_3(x) = x / n^2
  expect_equal(
    unname(M[1, 4]),
    x / n^2,
    tolerance = 1e-8
  )

  # mu_4(x) = 3*x^2/n^2 + x/n^3
  expect_equal(
    unname(M[1, 5]),
    3 * x^2 / n^2 + x / n^3,
    tolerance = 1e-8
  )
})


test_that("Szász-Mirakyan moments verify through order 8", {

  S <- approx_operator(
    family = "szasz",
    n = 10
  )

  ordinary_check <- verify_moments(
    operator = S,
    order = 8,
    x = 0.5,
    central = FALSE
  )

  central_check <- verify_moments(
    operator = S,
    order = 8,
    x = 0.5,
    central = TRUE
  )

  expect_true(
    all(ordinary_check$status)
  )

  expect_true(
    all(central_check$status)
  )
})


test_that("Szász-Mirakyan rejects points outside its domain", {

  S <- approx_operator(
    family = "szasz",
    n = 10
  )

  expect_error(
    approximate(
      operator = S,
      f = function(t) t^2,
      x = -0.1
    ),
    "domain"
  )
})
