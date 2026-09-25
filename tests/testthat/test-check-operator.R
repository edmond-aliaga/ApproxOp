test_that("check_operator works for a valid registered operator", {

  family_name <- "check_demo_valid"

  register_operator(
    family = family_name,
    domain = c(0, 1),
    evaluation_engine = function(f, x, operator) {
      f(x)
    },
    display_name = "Check demo valid operator"
  )

  op <- approx_operator(
    family = family_name,
    n = 10
  )

  result <- check_operator(
    op,
    x = c(0, 0.25, 0.5, 0.75, 1)
  )

  expect_s3_class(
    result,
    "approx_operator_check"
  )

  expect_true(result$overall)

  expect_true(
    all(result$checks$status)
  )

  expect_equal(
    result$x,
    c(0, 0.25, 0.5, 0.75, 1)
  )

  expect_equal(
    result$tolerance,
    1e-10
  )

  expect_true(
    result$check_positivity
  )
})


test_that("check_operator detects failure to reproduce constants", {

  family_name <- "check_demo_bad_constant"

  register_operator(
    family = family_name,
    domain = c(0, 1),
    evaluation_engine = function(f, x, operator) {
      2 * f(x)
    },
    display_name = "Check demo bad constant operator"
  )

  op <- approx_operator(
    family = family_name,
    n = 10
  )

  result <- check_operator(
    op,
    x = c(0, 0.5, 1)
  )

  expect_false(
    result$overall
  )

  constant_row <- result$checks[
    result$checks$check ==
      "constant_reproduction",
    ,
    drop = FALSE
  ]

  expect_equal(
    nrow(constant_row),
    1L
  )

  expect_false(
    constant_row$status
  )
})


test_that("check_operator detects non-finite output", {

  family_name <- "check_demo_nonfinite"

  register_operator(
    family = family_name,
    domain = c(0, 1),
    evaluation_engine = function(f, x, operator) {
      rep(Inf, length(x))
    },
    display_name = "Check demo non-finite operator"
  )

  op <- approx_operator(
    family = family_name,
    n = 10
  )

  result <- check_operator(
    op,
    x = c(0, 0.5, 1)
  )

  expect_false(
    result$overall
  )

  finite_row <- result$checks[
    result$checks$check ==
      "finite_output",
    ,
    drop = FALSE
  ]

  expect_equal(
    nrow(finite_row),
    1L
  )

  expect_false(
    finite_row$status
  )
})


test_that("check_operator can disable positivity diagnostics", {

  family_name <- "check_demo_no_positivity"

  register_operator(
    family = family_name,
    domain = c(0, 1),
    evaluation_engine = function(f, x, operator) {
      f(x)
    },
    display_name = "Check demo without positivity"
  )

  op <- approx_operator(
    family = family_name,
    n = 10
  )

  result <- check_operator(
    op,
    x = c(0, 0.5, 1),
    check_positivity = FALSE
  )

  expect_true(
    result$overall
  )

  expect_false(
    result$check_positivity
  )

  expect_false(
    any(
      grepl(
        "^positivity_",
        result$checks$check
      )
    )
  )
})


test_that("check_operator validates tolerance", {

  family_name <- "check_demo_tolerance"

  register_operator(
    family = family_name,
    domain = c(0, 1),
    evaluation_engine = function(f, x, operator) {
      f(x)
    },
    display_name = "Check demo tolerance operator"
  )

  op <- approx_operator(
    family = family_name,
    n = 10
  )

  expect_error(
    check_operator(
      op,
      x = c(0, 0.5, 1),
      tolerance = 0
    ),
    "`tolerance`"
  )
})


test_that("print method for operator checks returns object invisibly", {

  family_name <- "check_demo_print"

  register_operator(
    family = family_name,
    domain = c(0, 1),
    evaluation_engine = function(f, x, operator) {
      f(x)
    },
    display_name = "Check demo print operator"
  )

  op <- approx_operator(
    family = family_name,
    n = 10
  )

  result <- check_operator(
    op,
    x = c(0, 0.5, 1)
  )

  printed <- capture.output(
    returned <- print(result)
  )

  expect_true(
    length(printed) > 0L
  )

  expect_match(
    printed[1L],
    "ApproxOp operator diagnostic"
  )

  expect_identical(
    returned,
    result
  )
})
