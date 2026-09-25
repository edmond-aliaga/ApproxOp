# Bernstein operator -------------------------------------------------------

bernstein_weights <- function(n, x) {

  if (length(x) != 1L || !is.numeric(x) || !is.finite(x)) {
    stop("`x` must be a single finite numeric value.", call. = FALSE)
  }

  if (x < 0 || x > 1) {
    stop("For the Bernstein operator, `x` must belong to [0, 1].",
         call. = FALSE)
  }

  k <- 0:n

  stats::dbinom(
    k,
    size = n,
    prob = x
  )
}


bernstein_evaluate <- function(f, n, x) {

  if (!is.function(f)) {
    stop("`f` must be a function.", call. = FALSE)
  }

  if (length(x) > 1L) {
    return(vapply(
      x,
      function(xi) bernstein_evaluate(f, n, xi),
      numeric(1)
    ))
  }

  k <- 0:n
  w <- bernstein_weights(n, x)

  fx <- vapply(
    k / n,
    f,
    numeric(1)
  )

  sum(w * fx)
}
