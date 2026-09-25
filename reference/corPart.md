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

  column indices (whole numbers, no duplicates) of the variables of
  interest for which partial correlations are computed

- y:

  column indices (whole numbers, no duplicates) of the control variables
  (conditioning set); must not overlap with `x`

## Value

a symmetric numeric matrix containing the partial correlations among
variables in `x`, adjusted for variables in `y`. Row and column names
correspond to `colnames(m)[x]`.

## Details

Only the variables in `y` are controlled for. Let \\S\\ be the joint
covariance matrix of the selected variables. The residual covariance
matrix of `x` after linear adjustment for `y` is the Schur complement

\$\$V = S\_{xx} - S\_{xy} S\_{yy}^{-1} S\_{yx}.\$\$

The result has entries \\V\_{ij}/\sqrt{V\_{ii} V\_{jj}}\\. With complete
raw data, this equals the correlation matrix of the residuals from
regressing each variable in `x` on all variables in `y`, including an
intercept. Adding another variable to `x` does not change the
correlations between the previously selected variables.

Normalizing the inverse of the joint covariance matrix would also
control for the other variables in `x`. That is a different quantity
when `length(x) > 2`.

Because the result is scaled to unit diagonal, it makes no difference
whether `m` is a covariance or a correlation matrix.

## Numerical considerations

- The joint submatrix of `x` and `y` must be invertible, and every
  selected variable must have positive variance. Near-singularity from
  collinearity is detected via the reciprocal condition number, not
  merely by a failure of
  [`base::solve()`](https://rdrr.io/r/base/solve.html), which succeeds
  and returns nonsense well before the matrix is numerically singular.
  The condition number is taken on the correlation scale, so variables
  measured in very different units are not mistaken for collinear ones.

- For raw data only the selected columns enter
  [`stats::cov()`](https://rdrr.io/r/stats/cor.html).

- `x` and `y` must not overlap.

- For raw data, correlations are computed pairwise, which can produce a
  non-positive-definite matrix when values are missing.

## See also

[`stats::cor()`](https://rdrr.io/r/stats/cor.html),
[`stats::cov()`](https://rdrr.io/r/stats/cor.html)

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
