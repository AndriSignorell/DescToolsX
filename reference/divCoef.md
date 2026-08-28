# Compute a diversity coefficient

Computes Rao's quadratic diversity coefficient for each column of a data
frame, optionally using a provided distance matrix.

## Usage

``` r
divCoef(x, dis = NULL, normalize = FALSE, na.rm = FALSE, tol = 0.00000001)
```

## Arguments

- x:

  a data frame or matrix of non-negative values (e.g. abundances). Rows
  correspond to entities, columns to samples.

- dis:

  optional object of class `dist`. If `NULL`, a default Euclidean
  distance is used, in which case the coefficient reduces to the
  Gini-Simpson index \\1 - \sum p_i^2\\.

- normalize:

  logical; if `TRUE`, the diversity is scaled by its theoretical maximum

- na.rm:

  logical; if `TRUE`, columns containing missing values yield `NA`
  instead of aborting

- tol:

  numeric convergence tolerance for the iterative maximisation used by
  `normalize = TRUE`

## Value

a numeric vector of diversity coefficients, one per column

## Details

The diversity coefficient is defined as \$\$D = \frac{x^T D^2 x}{2 (\sum
x)^2}\$\$ where \\x\\ is a column of `x` and \\D\\ is the distance
matrix.

If `normalize = TRUE`, values are divided by the maximum achievable
diversity under the given distance matrix. That maximum is found by a
fixed-point iteration over the simplex, which is a heuristic: it is not
guaranteed to reach the global optimum for an arbitrary distance matrix.
A warning is issued when the iteration has not converged within `tol`.

## See also

Other inequality: [`atkinson()`](atkinson.md), [`gini()`](gini.md),
[`lc()`](Lc.md), [`rosenbluth()`](rosenbluth.md), [`theil()`](theil.md)

## Examples

``` r
set.seed(1)
x <- matrix(runif(20), ncol = 4)
d <- dist(matrix(rnorm(10), ncol = 2))

divCoef(x, d)
#> [1] 2.095912 1.959129 1.740829 1.485362
divCoef(x, d, normalize = TRUE)
#> [1] 0.8327579 0.7784104 0.6916744 0.5901711

# without a distance matrix this is the Gini-Simpson index
divCoef(matrix(c(1, 1, 1, 1, 0, 0), ncol = 2))
#> [1] 0.6666667 0.0000000
```
