# Mean Squared Error

Computes the mean squared error (MSE) between predicted and reference
values.

## Usage

``` r
mse(x, ...)

# S3 method for class 'lm'
mse(x, ...)

# Default S3 method
mse(x, ref, na.rm = FALSE, ...)
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

a numeric scalar containing the MSE

## Details

The mean squared error is defined as: \$\$ \frac{1}{n} \sum (ref - x)^2
\$\$

## See also

[`mean`](https://rdrr.io/r/base/mean.html)

Other model.metrics: [`auc()`](auc.md), [`brierScore()`](brierScore.md),
[`mae()`](mae.md), [`mape()`](mape.md), [`nmae()`](nmae.md),
[`nmse()`](nmse.md), [`rmse()`](rmse.md), [`smape()`](smape.md)

## Examples

``` r
x <- c(2.5, 3.0, 2.8)
ref <- c(3.0, 2.5, 3.0)

mse(x, ref)
#> [1] 0.18

# with linear model
fit <- lm(mpg ~ hp, data = mtcars)
mse(fit)
#> [1] 13.98982
```
