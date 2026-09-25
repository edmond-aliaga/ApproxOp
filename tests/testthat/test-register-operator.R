test_that("register_operator registers and evaluates a custom operator", {

  family_name <- "test_identity_operator"

  register_operator(
    family = family_name,
    domain = c(0, 1),
    evaluation_engine = function(f, x, operator) {
      f(x)
    },
    display_name = "Test identity operator",
    overwrite = TRUE
  )

  op <- approx_operator(
    family = family_name,
    n = 10
  )

  expect_s3_class(
    op,
    "approx_operator"
  )

  expect_equal(
    op$family,
    family_name
  )

  expect_equal(
    op$variant,
    "discrete"
  )

  expect_equal(
    op$domain,
    c(0, 1)
  )

  expect_true(
    is.function(op$evaluation_engine)
  )

  x <- c(
    0,
    0.25,
    0.5,
    0.75,
    1
  )

  result <- approximate(
    op,
    f = function(x) x^2,
    x = x
  )

  expect_equal(
    result,
    x^2
  )
})


test_that("registered operators use the universal moment fallback", {

  family_name <- "test_moment_fallback"

  register_operator(
    family = family_name,
    domain = c(0, 1),
    evaluation_engine = function(f, x, operator) {
      f(x)
    },
    overwrite = TRUE
  )

  op <- approx_operator(
    family = family_name,
    n = 10
  )

  x <- c(
    0.25,
    0.5
  )

  result <- moments(
    op,
    order = 4,
    x = x
  )

  expected <- cbind(
    m0 = rep(1, length(x)),
    m1 = x,
    m2 = x^2,
    m3 = x^3,
    m4 = x^4
  )

  expect_equal(
    result,
    expected
  )
})


test_that("registered operators support central moments", {

  family_name <- "test_central_moments"

  register_operator(
    family = family_name,
    domain = c(0, 1),
    evaluation_engine = function(f, x, operator) {
      f(x)
    },
    overwrite = TRUE
  )

  op <- approx_operator(
    family = family_name,
    n = 10
  )

  result <- moments(
    op,
    order = 4,
    x = c(0.25, 0.5),
    central = TRUE
  )

  expect_equal(
    result[, "mu0"],
    c(1, 1)
  )

  expect_equal(
    result[, "mu1"],
    c(0, 0),
    tolerance = 1e-12
  )

  expect_equal(
    result[, "mu2"],
    c(0, 0),
    tolerance = 1e-12
  )

  expect_equal(
    result[, "mu3"],
    c(0, 0),
    tolerance = 1e-12
  )

  expect_equal(
    result[, "mu4"],
    c(0, 0),
    tolerance = 1e-12
  )
})


test_that("register_operator supports a custom moment engine", {

  family_name <- "test_custom_moment_engine"

  register_operator(
    family = family_name,
    domain = c(0, 1),
    evaluation_engine = function(f, x, operator) {
      f(x)
    },
    moment_engine = function(operator, order, x) {

      result <- vapply(
        0:order,
        function(r) {
          x^r
        },
        numeric(length(x))
      )

      matrix(
        result,
        nrow = length(x),
        ncol = order + 1L
      )
    },
    overwrite = TRUE
  )

  op <- approx_operator(
    family = family_name,
    n = 10
  )

  expect_true(
    is.function(op$moment_engine)
  )

  x <- c(
    0.25,
    0.5
  )

  result <- moments(
    op,
    order = 4,
    x = x
  )

  expected <- cbind(
    m0 = rep(1, length(x)),
    m1 = x,
    m2 = x^2,
    m3 = x^3,
    m4 = x^4
  )

  expect_equal(
    result,
    expected
  )
})


test_that("register_operator supports parameter validation", {

  family_name <- "test_parameter_validator"

  register_operator(
    family = family_name,
    domain = c(0, 1),
    evaluation_engine = function(f, x, operator) {
      f(x)
    },
    parameter_validator = function(params) {

      if (is.null(params$a) ||
          !is.numeric(params$a) ||
          length(params$a) != 1L ||
          !is.finite(params$a) ||
          params$a <= 0) {
        stop(
          "`a` must be one positive finite numeric value.",
          call. = FALSE
        )
      }

      invisible(TRUE)
    },
    overwrite = TRUE
  )

  expect_error(
    approx_operator(
      family = family_name,
      n = 10
    ),
    "`a` must be one positive finite numeric value."
  )

  expect_error(
    approx_operator(
      family = family_name,
      n = 10,
      params = list(
        a = -1
      )
    ),
    "`a` must be one positive finite numeric value."
  )

  op <- approx_operator(
    family = family_name,
    n = 10,
    params = list(
      a = 2
    )
  )

  expect_equal(
    op$parameters$a,
    2
  )
})


test_that("register_operator validates its main arguments", {

  expect_error(
    register_operator(
      family = "",
      domain = c(0, 1),
      evaluation_engine = function(f, x, operator) {
        f(x)
      }
    ),
    "`family` must be a single non-empty character string."
  )

  expect_error(
    register_operator(
      family = "test_invalid_domain",
      domain = c(1, 0),
      evaluation_engine = function(f, x, operator) {
        f(x)
      }
    ),
    "`domain` must be a numeric vector"
  )

  expect_error(
    register_operator(
      family = "test_invalid_engine",
      domain = c(0, 1),
      evaluation_engine = 1
    ),
    "`evaluation_engine` must be a function."
  )

  expect_error(
    register_operator(
      family = "test_invalid_moment_engine",
      domain = c(0, 1),
      evaluation_engine = function(f, x, operator) {
        f(x)
      },
      moment_engine = 1
    ),
    "`moment_engine` must be NULL or a function."
  )

  expect_error(
    register_operator(
      family = "test_invalid_validator",
      domain = c(0, 1),
      evaluation_engine = function(f, x, operator) {
        f(x)
      },
      parameter_validator = 1
    ),
    "`parameter_validator` must be NULL or a function."
  )
})


test_that("registered evaluation engines must return valid output", {

  family_name <- "test_invalid_evaluation_output"

  register_operator(
    family = family_name,
    domain = c(0, 1),
    evaluation_engine = function(f, x, operator) {
      1
    },
    overwrite = TRUE
  )

  op <- approx_operator(
    family = family_name,
    n = 10
  )

  expect_error(
    approximate(
      op,
      f = function(x) x^2,
      x = c(0.25, 0.5)
    ),
    paste0(
      "The registered evaluation engine must return ",
      "a numeric vector of length equal to `length\\(x\\)`"
    )
  )
})


test_that("register_operator restores registries after failed registration", {

  family_name <- "test_atomic_registration"

  register_operator(
    family = family_name,
    domain = c(0, 1),
    evaluation_engine = function(f, x, operator) {
      f(x)
    },
    display_name = "Original atomic test operator",
    overwrite = TRUE
  )

  family_before <- get_family_spec(
    family_name
  )

  key <- operator_registry_key(
    family = family_name,
    subfamily = NULL,
    variant = "discrete"
  )

  operator_before <- get(
    key,
    envir = .operator_implementation_registry,
    inherits = FALSE
  )

  expect_error(
    register_operator(
      family = family_name,
      domain = c(0, 1),
      evaluation_engine = function(f, x, operator) {
        2 * f(x)
      },
      display_name = "Modified atomic test operator",
      metadata = list(
        temporary_value = TRUE
      ),
      overwrite = FALSE
    )
  )

  family_after <- get_family_spec(
    family_name
  )

  operator_after <- get(
    key,
    envir = .operator_implementation_registry,
    inherits = FALSE
  )

  expect_equal(
    family_after,
    family_before
  )

  expect_identical(
    operator_after$evaluation_engine,
    operator_before$evaluation_engine
  )

  expect_equal(
    operator_after$metadata,
    operator_before$metadata
  )

  expect_false(
    "temporary_value" %in%
      names(family_after$metadata)
  )

  op <- approx_operator(
    family = family_name,
    n = 10
  )

  result <- approximate(
    op,
    f = function(x) x^2,
    x = c(0.25, 0.5)
  )

  expect_equal(
    result,
    c(0.0625, 0.25)
  )
})
