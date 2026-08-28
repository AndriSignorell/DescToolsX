# (Weighted) Variance and Standard Deviation

`varX()` computes the variance of `x`, allowing the definition of
weights (unlike base R's [`var()`](https://rdrr.io/r/stats/cor.html)
function). Using the estimator `ml` returns the uncorrected sample
variance (which is a biased estimator for the sample variance).  
`sdX` yields the standard deviation following the same logic.

## Usage

``` r
sdX(x, estimator = c("unbiased", "ml"), weights = NULL, na.rm = FALSE, ...)

varX(x, ...)

# Default S3 method
varX(x, estimator = c("unbiased", "ml"), weights = NULL, na.rm = FALSE, ...)

# S3 method for class 'Freq'
varX(x, breaks, estimator = c("unbiased", "ml"), ...)
```

## Arguments

- x:

  a numeric vector, matrix, or data frame

- estimator:

  determines the estimator type; if `"unbiased"` (the default) then the
  usual unbiased estimate (using \\n - 1\\ as denominator) is returned,
  if `"ml"` then it is the maximum likelihood estimate for a Gaussian
  distribution (denominator \\n\\).

- weights:

  non-negative numeric vector of weights the same length as `x`,
  interpreted as frequency (replication) weights. Observations with
  larger weights contribute more strongly to the empirical distribution.
  Weights are supported for vector input only.

- na.rm:

  logical. Should missing values be removed?

- ...:

  further arguments passed to or from other methods

- breaks:

  breaks for calculating the variance for classified data as composed by
  [`freq`](freq.md)

## Value

- `varX()`:

  a numeric scalar for vector input or a covariance matrix for a matrix
  or data frame

- `sdX()`:

  a numeric scalar containing the standard deviation

## Details

Using estimator `"unbiased"` the denominator \\n - 1\\ is used (known as
"Bessel's correction") which gives an unbiased estimator of the
(co)variance for i.i.d. observations.  
`"ml"` yields the biased version using the denominator \\n\\. With
frequency weights \\n\\ is the sum of the weights.

These functions return [`NA`](https://rdrr.io/r/base/NA.html) when there
is only one observation and `NA` when `x` has length zero.

**Note:**` ` Analytic (precision) weights are not supported. For
likelihood-based weighted variance estimation, see
[`cov.wt`](https://rdrr.io/r/stats/cov.wt.html).

## References

Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) *The New S
Language*. Wadsworth & Brooks/Cole.

## See also

[`varCI`](https://andrisignorell.github.io/lumen/reference/varCI.html)
for confidence intervals,
[`varTest`](https://andrisignorell.github.io/lumen/reference/varTest.html)
for tests and base R's implementations
[`var`](https://rdrr.io/r/stats/cor.html),
[`sd`](https://rdrr.io/r/stats/sd.html),
[`cov`](https://rdrr.io/r/stats/cor.html)

Other dispersion: [`coefVar()`](coefVar.md), [`iqrX()`](iqrX.md),
[`madX()`](madX.md), [`meanAD()`](meanAD.md), [`meanSE()`](meanSE.md),
[`rangeX()`](rangeX.md)

## Examples

``` r

varX(1:10)                 # 9.166667
#> [1] 9.166667
sdX(1:10)
#> [1] 3.02765

# frequency weights replicate the observations, so the result is the
# variance of the expanded vector c(1, 2,2, 3,3,3, 4,4,4,4, 5,5,5,5,5)
varX(1:5, weights=1:5)     # 1.666667
#> [1] 1.666667
varX(rep(1:5, times=1:5))  # 1.666667
#> [1] 1.666667

# weighted Variance
set.seed(45)
(z <- as.numeric(names(w <- table(x <- sample(-10:20, size=50, replace=TRUE)))))
#>  [1] -9 -8 -7 -6 -5 -4 -3 -2  0  3  4  5  6  7  8  9 10 12 13 15 16 17 18 19 20
varX(z, weights=w)
#> [1] 86.53429
sdX(z, weights=w)
#> [1] 9.302381

# check!
all.equal(varX(x), varX(z, weights=w))
#> [1] TRUE


# Variance for frequency tables
varX(freq(as.table(c(6,16,24,25,17))),
          breaks=c(0, 10, 20, 30, 40, 50))
#> [1] 140.3213

```
