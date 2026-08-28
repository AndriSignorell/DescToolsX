# Mean Absolute Error

Computes the mean absolute error (MAE) between predicted and reference
values.

## Usage

``` r
mae(x, ...)

# S3 method for class 'lm'
mae(x, ...)

# Default S3 method
mae(x, ref, na.rm = FALSE, ...)
```

## Arguments

- x:

  an object. Methods are available for numeric vectors and model objects
  (e.g. `lm`).

- ...:

  additional arguments passed to methods

- ref:

  numeric vector of reference (true) values

- na.rm:

  logical; whether to remove missing values

## Value

a numeric scalar containing the MAE

## Details

The mean absolute error is defined as: \$\$ \frac{1}{n} \sum \|ref - x\|
\$\$

## See also

[`mean`](https://rdrr.io/r/base/mean.html),
[`abs`](https://rdrr.io/r/base/MathFun.html)

Other model.metrics: [`auc()`](auc.md), [`brierScore()`](brierScore.md),
[`mape()`](mape.md), [`mse()`](mse.md), [`nmae()`](nmae.md),
[`nmse()`](nmse.md), [`rmse()`](rmse.md), [`smape()`](smape.md)

## Examples

``` r
x <- c(2.5, 3.0, 2.8)
ref <- c(3.0, 2.5, 3.0)

mae(x, ref)
#> [1] 0.4

# with linear model
fit <- lm(mpg ~ hp, data = mtcars)
mae(fit)
#> [1] 2.907452
```
