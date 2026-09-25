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

  logical; if `TRUE`, the diversity is scaled by its maximum over all
  relative abundances (see Details)

- na.rm:

  logical; if `TRUE`, columns containing missing values yield `NA`
  instead of aborting

- tol:

  relative accuracy of the maximum used by `normalize = TRUE`: the
  iteration stops once the maximum is certified to within this fraction

## Value

a numeric vector of diversity coefficients, one per column

## Details

The diversity coefficient is defined as \$\$D = \frac{x^T D^2 x}{2 (\sum
x)^2}\$\$ where \\x\\ is a column of `x` and \\D\\ is the distance
matrix.

If `normalize = TRUE`, values are divided by the maximum of the
coefficient over all relative abundance vectors, so that the result lies
in \\\[0, 1\]\\. The maximum is found by the replicator (Baum-Eagon)
iteration \\p_i \leftarrow p_i (Ap)\_i / p^T A p\\, which increases
\\p^T A p\\ monotonically. For a Euclidean `dis` the quadratic form is
concave on the simplex, so the iteration reaches the global maximum, and
it stops once the Frank-Wolfe duality gap certifies that maximum to a
relative accuracy of `tol`; normalized values may therefore exceed 1 by
at most that amount. For a non-Euclidean `dis` (which triggers a
warning) only a local maximum is guaranteed. A warning is issued when
the iteration has not converged.

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
#> [1] 0.5727755 0.5353949 0.4757375 0.4059229

# without a distance matrix this is the Gini-Simpson index
divCoef(matrix(c(1, 1, 1, 1, 0, 0), ncol = 2))
#> [1] 0.6666667 0.0000000
```
