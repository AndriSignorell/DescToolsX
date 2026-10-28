# Mean Squared Error

Computes the mean squared error (MSE) between predicted and reference
values.

## Usage

``` r
mse(x, ...)

# Default S3 method
mse(x, ref, na.rm = FALSE, ...)
```

## Arguments

- x:

  a numeric vector of predicted values. Methods for fitted models are
  provided by the package alloy.

- ...:

  additional arguments passed to methods

- ref:

  numeric vector of reference (true) values

- na.rm:

  logical; whether to remove missing values

## Value

a numeric scalar containing the MSE

## Details

The mean squared error is defined as: \$\$ \frac{1}{n} \sum (ref - x)^2
\$\$

## See also

[`mean()`](https://rdrr.io/r/base/mean.html)

Other model.metrics: [`auc()`](auc.md),
[`averagePrecision()`](averagePrecision.md),
[`brierScore()`](brierScore.md), [`logLoss()`](logLoss.md),
[`mae()`](mae.md), [`mape()`](mape.md), [`nmae()`](nmae.md),
[`nmse()`](nmse.md), [`rmse()`](rmse.md), [`smape()`](smape.md)

## Examples

``` r
x <- c(2.5, 3.0, 2.8)
ref <- c(3.0, 2.5, 3.0)

mse(x, ref)
#> [1] 0.18
```
