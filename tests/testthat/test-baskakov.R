test_that("Baskakov operator is constructed correctly", {

  B <- approx_operator(
    family = "baskakov",
    n = 10
  )

  expect_s3_class(B, "approx_operator")
  expect_equal(B$family, "baskakov")
  expect_equal(B$n, 10)
  expect_equal(B$variant, "discrete")
  expect_equal(B$domain, c(0, Inf))
})


test_that("Baskakov reproduces the constant function", {

  B <- approx_operator(
    family = "baskakov",
    n = 10
  )

  x <- c(0, 0.25, 0.5, 1, 2)

  result <- approximate(
    operator = B,
    f = function(t) 1,
    x = x
  )

  expect_equal(
    unname(result),
    rep(1, length(x)),
    tolerance = 1e-8
  )
})


test_that("Baskakov reproduces the linear function", {

  B <- approx_operator(
    family = "baskakov",
    n = 10
  )

  x <- c(0, 0.25, 0.5, 1, 2)

  result <- approximate(
    operator = B,
    f = function(t) t,
    x = x
  )

  expect_equal(
    unname(result),
    x,
    tolerance = 1e-8
  )
})


test_that("Baskakov second moment is correct", {

  n <- 10

  B <- approx_operator(
    family = "baskakov",
    n = n
  )

  x <- c(0, 0.25, 0.5, 1, 2)

  result <- approximate(
    operator = B,
    f = function(t) t^2,
    x = x
  )

  expected <- x^2 + x * (1 + x) / n

  expect_equal(
    unname(result),
    expected,
    tolerance = 1e-8
  )
})


test_that("Baskakov ordinary moments are computed through order 8", {

  B <- approx_operator(
    family = "baskakov",
    n = 10
  )

  M <- moments(
    operator = B,
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
    tolerance = 1e-8
  )

  expect_equal(
    unname(M[1, 2]),
    0.5,
    tolerance = 1e-8
  )

  expect_equal(
    unname(M[1, 3]),
    0.325,
    tolerance = 1e-8
  )
})


test_that("Baskakov central moments are correct", {

  n <- 10
  x <- 0.5

  B <- approx_operator(
    family = "baskakov",
    n = n
  )

  M <- moments(
    operator = B,
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
    tolerance = 1e-8
  )

  expect_equal(
    unname(M[1, 2]),
    0,
    tolerance = 1e-8
  )

  # mu_2(x) = x(1 + x) / n
  expect_equal(
    unname(M[1, 3]),
    x * (1 + x) / n,
    tolerance = 1e-8
  )

  # mu_3(x) = x(1 + x)(1 + 2x) / n^2
  expect_equal(
    unname(M[1, 4]),
    x * (1 + x) * (1 + 2 * x) / n^2,
    tolerance = 1e-8
  )

  # For n = 10 and x = 0.5, mu_4 = 0.021
  expect_equal(
    unname(M[1, 5]),
    0.021,
    tolerance = 1e-8
  )
})


test_that("Baskakov moments verify through order 8", {

  B <- approx_operator(
    family = "baskakov",
    n = 10
  )

  ordinary_check <- verify_moments(
    operator = B,
    order = 8,
    x = 0.5,
    central = FALSE
  )

  central_check <- verify_moments(
    operator = B,
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


test_that("Baskakov rejects points outside its domain", {

  B <- approx_operator(
    family = "baskakov",
    n = 10
  )

  expect_error(
    approximate(
      operator = B,
      f = function(t) t^2,
      x = -0.1
    ),
    "domain"
  )
})
