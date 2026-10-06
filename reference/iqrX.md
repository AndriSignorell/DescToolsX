# The (Weighted) Interquartile Range

Compute the interquartile range of `x`, with optional weights.

## Usage

``` r
iqrX(x, weights = NULL, na.rm = FALSE, type = NULL)
```

## Arguments

- x:

  numeric vector

- weights:

  optional numeric vector of non-negative sample weights, of the same
  length as `x`

- na.rm:

  logical; whether to remove missing values. If `FALSE` and `x` contains
  missing values, the result is `NA`.

- type:

  integer selecting the quantile algorithm, see
  [`quantileX()`](quantileX.md). The default `NULL` picks the one that
  suits the branch taken: `7` without weights, as in
  [`stats::IQR()`](https://rdrr.io/r/stats/IQR.html); `2` with weights,
  which reads them as *relative* weights and depends only on their
  ratios. Pass `type = 7` explicitly to read the weights as replication
  counts.

## Value

numeric scalar containing the interquartile range

## Details

The IQR is the difference of the 0.75 and the 0.25 quantile as computed
by [`quantileX()`](quantileX.md). Without weights the result is
identical to [`stats::IQR()`](https://rdrr.io/r/stats/IQR.html), except
that a missing value yields `NA` instead of an error when
`na.rm = FALSE`.

## See also

[`medianX()`](medianX.md), [`quantileX()`](quantileX.md),
[`stats::IQR()`](https://rdrr.io/r/stats/IQR.html),
[`stats::quantile()`](https://rdrr.io/r/stats/quantile.html)

Other dispersion: [`coefVar()`](coefVar.md), [`madX()`](madX.md),
[`meanAbsDev()`](meanAbsDev.md), [`meanSE()`](meanSE.md),
[`rangeX()`](rangeX.md), [`varX()`](varX.md)

## Examples

``` r
x <- c(3.7, 3.3, 3.5, 2.8)
w <- c(5, 5, 4, 1) / 15

iqrX(x)
#> [1] 0.375
iqrX(x, weights = w)
#> [1] 0.4
iqrX(x, weights = w * 15)                 # same: only the ratios count
#> [1] 0.4
iqrX(x, weights = w * 15, type = 7)       # replication counts
#> [1] 0.4

iqrX(c(x, NA))                            # NA, no error
#> [1] NA
iqrX(c(x, NA), na.rm = TRUE)
#> [1] 0.375
```
