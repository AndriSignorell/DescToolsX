# Partial Correlation Matrix

Computes the partial correlation matrix of a set of variables `x` while
controlling for another set of variables `y`, based on a
covariance/correlation matrix or on raw data.

## Usage

``` r
corPart(m, x, y)
```

## Arguments

- m:

  a numeric matrix, either:

  - a square, symmetric covariance or correlation matrix, or

  - a data matrix (observations in rows, variables in columns)

  The two are told apart by symmetry, not by shape alone - a data matrix
  with as many rows as columns would otherwise be mistaken for a
  correlation matrix.

- x:

  integer vector of indices specifying the variables of interest for
  which partial correlations are computed

- y:

  integer vector of indices specifying the control variables
  (conditioning set)

## Value

a symmetric numeric matrix containing the partial correlations among
variables in `x`, adjusted for variables in `y`. Row and column names
correspond to `colnames(m)[x]`.

## Details

Partial correlations are read off the precision matrix. Let \\K\\ be the
inverse of the joint covariance matrix of \\(x, y)\\; then

\$\$\rho\_{ij \cdot y} = - K\_{ij} / \sqrt{K\_{ii} K\_{jj}}\$\$

for \\i, j\\ in \\x\\. This is algebraically equivalent to forming the
Schur complement \\\Sigma\_{xx} -
\Sigma\_{xy}\Sigma\_{yy}^{-1}\Sigma\_{yx}\\ and scaling it to unit
diagonal, but needs a single inversion instead of two.

Because the result is scaled to unit diagonal, it makes no difference
whether `m` is a covariance or a correlation matrix.

## Numerical considerations

- The joint submatrix of `x` and `y` must be invertible.
  Near-singularity from collinearity among the control variables is
  detected via the reciprocal condition number, not merely by a failure
  of [`solve`](https://rdrr.io/r/base/solve.html), which succeeds and
  returns nonsense well before the matrix is numerically singular.

- `x` and `y` must not overlap.

- For raw data, correlations are computed pairwise, which can produce a
  non-positive-definite matrix when values are missing.

## See also

[`cor`](https://rdrr.io/r/stats/cor.html),
[`cov`](https://rdrr.io/r/stats/cor.html)

Other assoc.continuous: [`corPolychor()`](corPolychor.md),
[`findCorrX()`](findCorrX.md), [`hoeffdingD()`](hoeffdingD.md),
[`keepSig()`](keepSig.md), [`pearsonCor()`](pearsonCor.md),
[`spearmanCor()`](spearmanCor.md)

## Examples

``` r
# Simulated data
set.seed(1)
X <- matrix(rnorm(100 * 5), ncol = 5)
colnames(X) <- paste0("V", 1:5)

# Partial correlations of V1, V2 controlling for V3, V4
corPart(X, x = 1:2, y = 3:4)
#>              V1           V2
#> V1  1.000000000 -0.002908161
#> V2 -0.002908161  1.000000000

# Using a correlation matrix directly
C <- cor(X)
corPart(C, x = 1:2, y = 3:4)
#>              V1           V2
#> V1  1.000000000 -0.002908161
#> V2 -0.002908161  1.000000000

# a single variable of interest is allowed and returns a 1x1 matrix
corPart(C, x = 1, y = 3:4)
#>    V1
#> V1  1
```
