# (Weighted) Arithmetic Mean

Generic function for the (trimmed) arithmetic mean, possibly with given
weights.

## Usage

``` r
meanX(x, ...)

# S3 method for class 'Freq'
meanX(x, breaks, ...)

# Default S3 method
meanX(x, weights = NULL, trim = 0, na.rm = FALSE, ...)
```

## Arguments

- x:

  an object. Currently there are methods for numeric/logical vectors and
  [date](https://rdrr.io/r/base/Dates.html),
  [date-time](https://rdrr.io/r/base/DateTimeClasses.html) and [time
  interval](https://rdrr.io/r/base/difftime.html) objects. Complex
  vectors are allowed for `trim = 0` only.

- ...:

  further arguments passed to or from other methods

- breaks:

  breaks for calculating the mean for classified data as composed by
  [`freq`](freq.md)

- weights:

  non-negative numeric vector of weights of the same length as `x`,
  interpreted as frequency (replication) weights. Observations with
  larger weights contribute more strongly to the empirical distribution.

- trim:

  the fraction (0 to 0.5) of observations to be trimmed from each end of
  `x` before the mean is computed. Values of trim outside that range are
  taken as the nearest endpoint.

- na.rm:

  a logical value indicating whether `NA` values should be stripped
  before the computation proceeds

## Value

if `trim` is zero (the default), the arithmetic mean of the values in
`x` is computed, as a numeric or complex vector of length one. If `x` is
not logical (coerced to numeric), numeric (including integer) or
complex, `NA_real_` is returned, with a warning.

If `trim` is non-zero, a symmetrically trimmed mean is computed with a
fraction of `trim` observations deleted from each end before the mean is
computed.

## Details

The argument `weights` is interpreted as frequency (replication)
weights. Conceptually, this corresponds to computing the statistic on a
reweighted empirical distribution, where observations with larger
weights represent a higher frequency in the population.

**Note:**` ` Analytic (precision) weights, which assume
observation-specific error variances or likelihood-based weighting, are
deliberately not supported. This design ensures that all weighted
statistics remain well-defined for ordinal, robust, and
distribution-based measures such as medians, quantiles, and measures of
dispersion.

`trim` and `weights` can't be used together at the same time.

## References

Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) *The New S
Language*. Wadsworth & Brooks/Cole.

## See also

[`weighted.mean`](https://rdrr.io/r/stats/weighted.mean.html),
[`mean.POSIXct`](https://rdrr.io/r/base/DateTimeClasses.html),
[`colMeans`](https://rdrr.io/r/base/colSums.html) for row and column
means.

Other location: [`gmean()`](gmean.md), [`hmean()`](hmean.md),
[`hodgesLehmann()`](hodgesLehmann.md), [`huberM()`](huberM.md),
[`medianX()`](medianX.md), [`modeX()`](modeX.md),
[`tukeyBiweight()`](tukeyBiweight.md)

## Examples

``` r

x <- c(0:10, 50)
xm <- meanX(x)
c(xm, meanX(x, trim = 0.10))
#> [1] 8.75 5.50

 
```
