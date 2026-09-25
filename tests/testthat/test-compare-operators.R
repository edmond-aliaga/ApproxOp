test_that("compare_operators returns the expected structure", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  S <- approx_operator(
    family = "szasz",
    n = 10
  )

  result <- compare_operators(
    operators = list(
      Bernstein = B,
      Szasz_Mirakyan = S
    ),
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 101),
    n = c(10, 20, 50)
  )

  expect_s3_class(
    result,
    "approx_operator_comparison"
  )

  expect_s3_class(
    result,
    "data.frame"
  )

  expect_equal(
    nrow(result),
    6L
  )

  expect_equal(
    ncol(result),
    8L
  )

  expect_named(
    result,
    c(
      "operator",
      "family",
      "subfamily",
      "variant",
      "n",
      "max_absolute_error",
      "mae",
      "rmse"
    )
  )
})


test_that("compare_operators computes correct Bernstein errors", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  S <- approx_operator(
    family = "szasz",
    n = 10
  )

  result <- compare_operators(
    operators = list(
      Bernstein = B,
      Szasz_Mirakyan = S
    ),
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 101),
    n = c(10, 20, 50)
  )

  bernstein_result <-
    result[
      result$operator == "Bernstein",
      ,
      drop = FALSE
    ]

  expect_equal(
    bernstein_result$n,
    c(10L, 20L, 50L)
  )

  expect_equal(
    bernstein_result$max_absolute_error,
    c(
      0.025,
      0.0125,
      0.005
    ),
    tolerance = 1e-12
  )
})


test_that("compare_operators preserves operator information", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  C <- approx_operator(
    family = "sheffer",
    subfamily = "charlier",
    n = 10,
    params = list(
      a = 2
    )
  )

  result <- compare_operators(
    operators = list(
      Bernstein = B,
      Charlier = C
    ),
    f = function(x) x,
    grid = seq(0, 1, length.out = 21),
    n = c(10, 20)
  )

  expect_true(
    all(
      result$family[
        result$operator == "Bernstein"
      ] == "bernstein"
    )
  )

  expect_true(
    all(
      is.na(
        result$subfamily[
          result$operator == "Bernstein"
        ]
      )
    )
  )

  expect_true(
    all(
      result$family[
        result$operator == "Charlier"
      ] == "sheffer"
    )
  )

  expect_true(
    all(
      result$subfamily[
        result$operator == "Charlier"
      ] == "charlier"
    )
  )

  expect_true(
    all(
      result$variant == "discrete"
    )
  )
})


test_that("compare_operators uses stored n values when n is NULL", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  S <- approx_operator(
    family = "szasz",
    n = 25
  )

  result <- compare_operators(
    operators = list(
      Bernstein = B,
      Szasz_Mirakyan = S
    ),
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 51)
  )

  expect_equal(
    result$n,
    c(10L, 25L)
  )

  expect_equal(
    unname(attr(result, "n_values")),
    c(10, 25)
  )

  expect_equal(
    names(attr(result, "n_values")),
    c(
      "Bernstein",
      "Szasz_Mirakyan"
    )
  )
})


test_that("compare_operators generates labels for unnamed operators", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  S <- approx_operator(
    family = "szasz",
    n = 10
  )

  result <- compare_operators(
    operators = list(
      B,
      S
    ),
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 21),
    n = 10
  )

  expect_equal(
    result$operator,
    c(
      "bernstein",
      "szasz"
    )
  )
})


test_that("compare_operators makes duplicate labels unique", {

  B1 <- approx_operator(
    family = "bernstein",
    n = 10
  )

  B2 <- approx_operator(
    family = "bernstein",
    n = 20
  )

  result <- compare_operators(
    operators = list(
      B1,
      B2
    ),
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 21),
    n = 10
  )

  expect_equal(
    result$operator,
    c(
      "bernstein",
      "bernstein_1"
    )
  )
})


test_that("compare_operators stores comparison metadata", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  S <- approx_operator(
    family = "szasz",
    n = 10
  )

  result <- compare_operators(
    operators = list(
      Bernstein = B,
      Szasz_Mirakyan = S
    ),
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 101),
    n = c(10, 20, 50)
  )

  expect_equal(
    attr(result, "grid_size"),
    101L
  )

  expect_equal(
    attr(result, "grid_range"),
    c(0, 1)
  )

  expect_equal(
    attr(result, "operator_count"),
    2L
  )

  expect_equal(
    attr(result, "operator_names"),
    c(
      "Bernstein",
      "Szasz_Mirakyan"
    )
  )

  expect_equal(
    attr(result, "n_values"),
    c(10L, 20L, 50L)
  )
})


test_that("compare_operators errors decrease for standard test case", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  S <- approx_operator(
    family = "szasz",
    n = 10
  )

  V <- approx_operator(
    family = "baskakov",
    n = 10
  )

  result <- compare_operators(
    operators = list(
      Bernstein = B,
      Szasz_Mirakyan = S,
      Baskakov = V
    ),
    f = function(x) x^2,
    grid = seq(0, 1, length.out = 101),
    n = c(10, 20, 50)
  )

  for (name in unique(result$operator)) {

    current <-
      result[
        result$operator == name,
        ,
        drop = FALSE
      ]

    expect_true(
      all(
        diff(
          current$max_absolute_error
        ) < 0
      )
    )

    expect_true(
      all(
        diff(
          current$mae
        ) < 0
      )
    )

    expect_true(
      all(
        diff(
          current$rmse
        ) < 0
      )
    )
  }
})


test_that("compare_operators rejects fewer than two operators", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  expect_error(
    compare_operators(
      operators = list(B),
      f = function(x) x^2,
      grid = seq(0, 1, length.out = 21),
      n = 10
    ),
    "`operators` must be a list containing at least two operators"
  )
})


test_that("compare_operators rejects non-operator elements", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  expect_error(
    compare_operators(
      operators = list(
        B,
        "not an operator"
      ),
      f = function(x) x^2,
      grid = seq(0, 1, length.out = 21),
      n = 10
    ),
    "Every element of `operators` must be an object of class"
  )
})


test_that("compare_operators rejects invalid functions", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  S <- approx_operator(
    family = "szasz",
    n = 10
  )

  expect_error(
    compare_operators(
      operators = list(B, S),
      f = 5,
      grid = seq(0, 1, length.out = 21),
      n = 10
    ),
    "`f` must be a function"
  )
})


test_that("compare_operators rejects invalid grids", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  S <- approx_operator(
    family = "szasz",
    n = 10
  )

  expect_error(
    compare_operators(
      operators = list(B, S),
      f = function(x) x^2,
      grid = numeric(0),
      n = 10
    ),
    "`grid` must be a non-empty vector of finite numeric values"
  )

  expect_error(
    compare_operators(
      operators = list(B, S),
      f = function(x) x^2,
      grid = c(0, NA, 1),
      n = 10
    ),
    "`grid` must be a non-empty vector of finite numeric values"
  )

  expect_error(
    compare_operators(
      operators = list(B, S),
      f = function(x) x^2,
      grid = c(0, Inf),
      n = 10
    ),
    "`grid` must be a non-empty vector of finite numeric values"
  )
})


test_that("compare_operators checks the common domain", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  S <- approx_operator(
    family = "szasz",
    n = 10
  )

  expect_error(
    compare_operators(
      operators = list(
        Bernstein = B,
        Szasz_Mirakyan = S
      ),
      f = function(x) x^2,
      grid = c(
        0,
        0.5,
        1,
        2
      ),
      n = 10
    ),
    "The grid is not valid for operator"
  )
})


test_that("compare_operators rejects invalid n values", {

  B <- approx_operator(
    family = "bernstein",
    n = 10
  )

  S <- approx_operator(
    family = "szasz",
    n = 10
  )

  expect_error(
    compare_operators(
      operators = list(B, S),
      f = function(x) x^2,
      grid = seq(0, 1, length.out = 21),
      n = 0
    ),
    "`n` must contain positive integers"
  )

  expect_error(
    compare_operators(
      operators = list(B, S),
      f = function(x) x^2,
      grid = seq(0, 1, length.out = 21),
      n = 2.5
    ),
    "`n` must contain positive integers"
  )

  expect_error(
    compare_operators(
      operators = list(B, S),
      f = function(x) x^2,
      grid = seq(0, 1, length.out = 21),
      n = NA_real_
    ),
    "`n` must contain positive integers"
  )
})
