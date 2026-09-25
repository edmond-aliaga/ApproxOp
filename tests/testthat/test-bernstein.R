test_that("Bernstein operator is constructed correctly", {

  op <- approx_operator(
    family = "bernstein",
    n = 10
  )

  expect_s3_class(op, "approx_operator")
  expect_equal(op$family, "bernstein")
  expect_equal(op$n, 10)
  expect_equal(op$variant, "discrete")
  expect_equal(op$domain, c(0, 1))
})


test_that("Bernstein reproduces constants and linear functions", {

  op <- approx_operator(
    family = "bernstein",
    n = 10
  )

  x <- c(0, 0.25, 0.5, 0.75, 1)

  f0 <- function(x) 1
  f1 <- function(x) x

  expect_equal(
    approximate(op, f0, x),
    rep(1, length(x)),
    tolerance = 1e-12
  )

  expect_equal(
    approximate(op, f1, x),
    x,
    tolerance = 1e-12
  )
})


test_that("Bernstein second moment is correct", {

  op <- approx_operator(
    family = "bernstein",
    n = 10
  )

  x <- c(0.1, 0.25, 0.5, 0.75, 0.9)

  numerical <- approximate(
    op,
    function(t) t^2,
    x
  )

  theoretical <- x^2 + x * (1 - x) / op$n

  expect_equal(
    numerical,
    theoretical,
    tolerance = 1e-12
  )
})


test_that("Bernstein moments are available through order 8", {

  op <- approx_operator(
    family = "bernstein",
    n = 10
  )

  m <- moments(
    operator = op,
    order = 8,
    x = 0.5
  )

  mu <- moments(
    operator = op,
    order = 8,
    x = 0.5,
    central = TRUE
  )

  expect_equal(ncol(m), 9)
  expect_equal(ncol(mu), 9)

  expect_equal(
    unname(m[1, 1]),
    1,
    tolerance = 1e-12
  )

  expect_equal(
    unname(m[1, 2]),
    0.5,
    tolerance = 1e-12
  )

  expect_equal(
    unname(mu[1, 1]),
    1,
    tolerance = 1e-12
  )

  expect_equal(
    unname(mu[1, 2]),
    0,
    tolerance = 1e-12
  )

  expect_equal(
    unname(mu[1, 3]),
    0.5 * (1 - 0.5) / op$n,
    tolerance = 1e-12
  )
})


test_that("Bernstein moment verification succeeds through order 8", {

  op <- approx_operator(
    family = "bernstein",
    n = 10
  )

  verification <- verify_moments(
    operator = op,
    order = 8,
    x = 0.5
  )

  verification_central <- verify_moments(
    operator = op,
    order = 8,
    x = 0.5,
    central = TRUE
  )

  expect_true(all(verification$status))
  expect_true(all(verification_central$status))
})


test_that("Bernstein rejects invalid n", {

  expect_error(
    approx_operator(
      family = "bernstein",
      n = -5
    ),
    "positive integer"
  )

  expect_error(
    approx_operator(
      family = "bernstein",
      n = 2.5
    ),
    "positive integer"
  )
})
