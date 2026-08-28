# Normalized Mean Absolute Error

Computes the normalized mean absolute error (NMAE) between predictions
and reference values.

## Usage

``` r
nmae(x, ref, trainY, na.rm = FALSE)
```

## Arguments

- x:

  numeric vector of predicted values

- ref:

  numeric vector of reference (true) values

- trainY:

  numeric vector used as the normalization baseline

- na.rm:

  logical; whether to remove incomplete cases before the computation.
  Defaults to `FALSE`, in which case a missing value anywhere makes the
  result `NA`.

## Value

a numeric scalar containing the normalized mean absolute error

## Details

The normalized mean absolute error is defined as: \$\$ \frac{\sum
\|ref - x\|}{\sum \|ref - mean(trainY)\|} \$\$

The denominator represents the absolute deviation from the mean of the
training response, providing a baseline for comparison.

If the denominator is zero, `NA` is returned.

## See also

[`mean`](https://rdrr.io/r/base/mean.html),
[`abs`](https://rdrr.io/r/base/MathFun.html)

Other model.metrics: [`auc()`](auc.md), [`brierScore()`](brierScore.md),
[`mae()`](mae.md), [`mape()`](mape.md), [`mse()`](mse.md),
[`nmse()`](nmse.md), [`rmse()`](rmse.md), [`smape()`](smape.md)

## Examples

``` r
x <- c(2.5, 3.0, 2.8)
ref <- c(3.0, 2.5, 3.0)
trainY <- c(2, 3, 4, 3)

nmae(x, ref, trainY)
#> [1] 2.4
```
