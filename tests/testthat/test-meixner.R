test_that("Sheffer-Meixner operator can be constructed", {

  M <- approx_operator(
    family = "sheffer",
    subfamily = "meixner",
    n = 10,
    params = list(
      alpha = 0,
      beta = 0,
      theta = 1,
      c = 0.75
    )
  )

  expect_s3_class(M, "approx_operator")
  expect_equal(M$family, "sheffer")
  expect_equal(M$subfamily, "meixner")
  expect_equal(M$n, 10)
  expect_equal(M$variant, "discrete")
  expect_equal(M$domain, c(0, Inf))

  expect_equal(M$parameters$alpha, 0)
  expect_equal(M$parameters$beta, 0)
  expect_equal(M$parameters$theta, 1)
  expect_equal(M$parameters$c, 0.75)
})


test_that("Sheffer-Meixner operator reproduces the constant function", {

  M <- approx_operator(
    family = "sheffer",
    subfamily = "meixner",
    n = 10,
    params = list(
      alpha = 0,
      beta = 0,
      theta = 1,
      c = 0.75
    )
  )

  value <- approximate(
    M,
    f = function(t) 1,
    x = 0.5
  )

  expect_equal(value, 1, tolerance = 1e-8)
})


test_that("Sheffer-Meixner first moment is correct", {

  M <- approx_operator(
    family = "sheffer",
    subfamily = "meixner",
    n = 10,
    params = list(
      alpha = 0,
      beta = 0,
      theta = 1,
      c = 0.75
    )
  )

  value <- approximate(
    M,
    f = function(t) t,
    x = 0.5
  )

  expect_equal(value, 0.6, tolerance = 1e-8)
})


test_that("Sheffer-Meixner second moment is correct", {

  M <- approx_operator(
    family = "sheffer",
    subfamily = "meixner",
    n = 10,
    params = list(
      alpha = 0,
      beta = 0,
      theta = 1,
      c = 0.75
    )
  )

  value <- approximate(
    M,
    f = function(t) t^2,
    x = 0.5
  )

  expect_equal(value, 0.58, tolerance = 1e-8)
})


test_that("Sheffer-Meixner ordinary moments through order 8 are finite", {

  M <- approx_operator(
    family = "sheffer",
    subfamily = "meixner",
    n = 10,
    params = list(
      alpha = 0,
      beta = 0,
      theta = 1,
      c = 0.75
    )
  )

  mm <- moments(
    M,
    order = 8,
    x = 0.5
  )

  expect_length(mm, 9)
  expect_true(all(is.finite(mm)))

  expect_equal(mm[1], 1, tolerance = 1e-8)
  expect_equal(mm[2], 0.6, tolerance = 1e-8)
  expect_equal(mm[3], 0.58, tolerance = 1e-8)
})


test_that("Sheffer-Meixner central moments have expected low-order values", {

  M <- approx_operator(
    family = "sheffer",
    subfamily = "meixner",
    n = 10,
    params = list(
      alpha = 0,
      beta = 0,
      theta = 1,
      c = 0.75
    )
  )

  cm <- moments(
    M,
    order = 8,
    x = 0.5,
    central = TRUE
  )

  expect_length(cm, 9)
  expect_true(all(is.finite(cm)))

  expect_equal(cm[1], 1, tolerance = 1e-8)
  expect_equal(cm[2], 0.1, tolerance = 1e-8)
  expect_equal(cm[3], 0.23, tolerance = 1e-8)
  expect_equal(cm[5], 0.3095, tolerance = 1e-7)
})


test_that("Sheffer-Meixner ordinary moments verify through order 8", {

  M <- approx_operator(
    family = "sheffer",
    subfamily = "meixner",
    n = 10,
    params = list(
      alpha = 0,
      beta = 0,
      theta = 1,
      c = 0.75
    )
  )

  check <- verify_moments(
    M,
    order = 8,
    x = 0.5
  )

  expect_true(all(check$status))
})


test_that("Sheffer-Meixner central moments verify through order 8", {

  M <- approx_operator(
    family = "sheffer",
    subfamily = "meixner",
    n = 10,
    params = list(
      alpha = 0,
      beta = 0,
      theta = 1,
      c = 0.75
    )
  )

  check <- verify_moments(
    M,
    order = 8,
    x = 0.5,
    central = TRUE
  )

  expect_true(all(check$status))
})


test_that("Sheffer-Meixner operator works for vector x", {

  M <- approx_operator(
    family = "sheffer",
    subfamily = "meixner",
    n = 10,
    params = list(
      alpha = 0,
      beta = 0,
      theta = 1,
      c = 0.75
    )
  )

  x <- c(0, 0.25, 0.5, 1)

  value <- approximate(
    M,
    f = function(t) 1,
    x = x
  )

  expect_length(value, length(x))

  expect_equal(
    value,
    rep(1, length(x)),
    tolerance = 1e-8
  )
})


test_that("Sheffer-Meixner parameter restrictions are enforced", {

  M_bad_theta <- approx_operator(
    family = "sheffer",
    subfamily = "meixner",
    n = 10,
    params = list(
      alpha = 0,
      beta = 0,
      theta = 0,
      c = 0.75
    )
  )

  expect_error(
    approximate(
      M_bad_theta,
      f = function(t) t,
      x = 0.5
    ),
    "theta"
  )

  M_bad_c <- approx_operator(
    family = "sheffer",
    subfamily = "meixner",
    n = 10,
    params = list(
      alpha = 0,
      beta = 0,
      theta = 1,
      c = 0.5
    )
  )

  expect_error(
    approximate(
      M_bad_c,
      f = function(t) t,
      x = 0.5
    ),
    "0.5 < c < 1"
  )

  M_bad_stancu <- approx_operator(
    family = "sheffer",
    subfamily = "meixner",
    n = 10,
    params = list(
      alpha = 2,
      beta = 1,
      theta = 1,
      c = 0.75
    )
  )

  expect_error(
    approximate(
      M_bad_stancu,
      f = function(t) t,
      x = 0.5
    ),
    "beta >= alpha"
  )
})


test_that("Sheffer-Meixner requires theta and c", {

  M_no_theta <- approx_operator(
    family = "sheffer",
    subfamily = "meixner",
    n = 10,
    params = list(
      c = 0.75
    )
  )

  expect_error(
    approximate(
      M_no_theta,
      f = function(t) t,
      x = 0.5
    ),
    "theta"
  )

  M_no_c <- approx_operator(
    family = "sheffer",
    subfamily = "meixner",
    n = 10,
    params = list(
      theta = 1
    )
  )

  expect_error(
    approximate(
      M_no_c,
      f = function(t) t,
      x = 0.5
    ),
    "c"
  )
})


test_that("Sheffer-Meixner Stancu moments are correct", {

  M <- approx_operator(
    family = "sheffer",
    subfamily = "meixner",
    n = 20,
    params = list(
      alpha = 1,
      beta = 2,
      theta = 2,
      c = 0.8
    )
  )

  x <- 0.5

  m0 <- approximate(
    M,
    f = function(t) 1,
    x = x
  )

  m1 <- approximate(
    M,
    f = function(t) t,
    x = x
  )

  m2 <- approximate(
    M,
    f = function(t) t^2,
    x = x
  )

  expected_m0 <- 1

  expected_m1 <-
    (20 * x + 1 + 2) / (20 + 2)

  expected_m2 <-
    (
      20^2 * x^2 +
        20 * x *
        (
          2 * 1 +
            2 * 2 +
            (2 * 0.8) / (2 * 0.8 - 1) +
            1
        ) +
        1^2 +
        2^2 +
        2 * 1 * 2 +
        2 * 2
    ) /
    (20 + 2)^2

  expect_equal(
    m0,
    expected_m0,
    tolerance = 1e-8
  )

  expect_equal(
    m1,
    expected_m1,
    tolerance = 1e-8
  )

  expect_equal(
    m2,
    expected_m2,
    tolerance = 1e-8
  )
})
