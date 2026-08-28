# The (weighted) Interquartile Range

computes interquartile range of the x values. Weights are supported.

## Usage

``` r
iqrX(x, weights = NULL, na.rm = FALSE, type = NULL)
```

## Arguments

- x:

  numeric vector

- weights:

  optional numeric vector giving the sample weights

- na.rm:

  logical; whether to remove missing values

- type:

  an integer selecting one of the many quantile algorithms, see
  [`quantileX()`](quantileX.md)

## Value

numeric scalar containing the interquartile range

## Details

This implementation is based on [`quantileX()`](quantileX.md) function,
which allows to define weights.

## See also

[medianX](medianX.md), [quantileX](quantileX.md),
[IQR](https://rdrr.io/r/stats/IQR.html),
[quantile](https://rdrr.io/r/stats/quantile.html)

Other dispersion: [`coefVar()`](coefVar.md), [`madX()`](madX.md),
[`meanAD()`](meanAD.md), [`meanSE()`](meanSE.md),
[`rangeX()`](rangeX.md), [`varX()`](varX.md)

## Examples

``` r

x <- c(3.7,3.3,3.5,2.8)
w <- c(5, 5, 4, 1)/15

iqrX(x=x, weights=w)
#> [1] 0.4
```
