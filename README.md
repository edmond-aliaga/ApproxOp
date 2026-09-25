# ApproxOp

`ApproxOp` is an R package providing a unified computational framework for
positive linear approximation operators.

The package is designed for defining, evaluating, analyzing, comparing, and
visualizing classical and generalized approximation operators within a common
interface. It also provides tools for ordinary and central moments, numerical
moment verification, approximation errors, convergence analysis, parameter
studies, and user-defined operators.

## Main features

`ApproxOp` currently provides:

- a unified `family` / `subfamily` / `variant` operator architecture;
- classical Bernstein, Szasz-Mirakyan, and Baskakov operators;
- Sheffer-type operators, including Appell, Charlier, and Meixner subfamilies;
- ordinary and central moments up to order 8;
- numerical verification of moments;
- pointwise approximation and error analysis;
- convergence studies over different values of `n`;
- comparison of several approximation operators;
- one- and two-parameter scans;
- graphical tools for approximation, convergence, comparisons, and parameter
  studies;
- registration and diagnostic checking of user-defined operators.

The package is designed to be extensible, so additional operator families,
subfamilies, and variants can be incorporated into the same framework.

## Installation

Once available on CRAN, the released version can be installed with:

```r
install.packages("ApproxOp")
```

The development version can be installed from GitHub with:

```r
# install.packages("pak")
pak::pak("edmond-aliaga/ApproxOp")
```

## Basic example

Load the package:

```r
library(ApproxOp)
```

Create a Bernstein operator:

```r
B <- approx_operator(
  family = "bernstein",
  n = 20
)

B
```

Approximate the function \(f(x)=x^2\):

```r
f <- function(x) x^2

approximate(
  operator = B,
  f = f,
  x = c(0.25, 0.50, 0.75)
)
```

## Moments

Ordinary moments can be computed up to order 8:

```r
moments(
  operator = B,
  order = 4,
  x = 0.5
)
```

Central moments are obtained with:

```r
moments(
  operator = B,
  order = 4,
  x = 0.5,
  central = TRUE
)
```

The analytical/computational moment engine can also be checked against direct
numerical evaluation:

```r
verify_moments(
  operator = B,
  order = 4,
  x = 0.5
)
```

## Error and convergence analysis

Pointwise approximation errors can be computed with:

```r
approx_error(
  operator = B,
  f = f,
  x = seq(0, 1, length.out = 101)
)
```

Convergence over several values of `n` can be studied using:

```r
convergence(
  operator = B,
  f = f,
  grid = seq(0, 1, length.out = 101),
  n = c(5, 10, 20, 50)
)
```

## Comparing operators

`ApproxOp` provides a common framework for comparing different approximation
operators using numerical error measures and graphical tools.

It also supports one- and two-dimensional parameter scans for parameterized
operator families.

## User-defined operators

New operators can be incorporated through `register_operator()`. This allows
users to extend the framework without modifying the core package.

Registered operators can be examined with:

```r
check_operator(...)
```

which provides numerical diagnostics such as evaluation, finiteness,
constant reproduction, and positivity checks.

## Development

`ApproxOp` is under active development. Future versions may extend the
framework with additional operator families and integral or fractional
variants.

Bug reports and suggestions can be submitted through the GitHub issue tracker.

## Author

**Edmond Aliaga**

Department of Mathematics and Computer Sciences  
University of Prishtina, Kosovo
