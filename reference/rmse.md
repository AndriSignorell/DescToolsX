# Root Mean Squared Error

Computes the root mean squared error (RMSE) between predicted and
reference values.

## Usage

``` r
rmse(x, ...)

# S3 method for class 'lm'
rmse(x, ...)

# Default S3 method
rmse(x, ref, na.rm = FALSE, ...)
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

a numeric scalar containing the RMSE

## Details

The RMSE is defined as: \$\$ \sqrt{\frac{1}{n} \sum (ref - x)^2} \$\$

## See also

[`mse`](mse.md), [`mae`](mae.md)

Other model.metrics: [`auc()`](auc.md), [`brierScore()`](brierScore.md),
[`mae()`](mae.md), [`mape()`](mape.md), [`mse()`](mse.md),
[`nmae()`](nmae.md), [`nmse()`](nmse.md), [`smape()`](smape.md)

## Examples

``` r
x <- c(2.5, 3.0, 2.8)
ref <- c(3.0, 2.5, 3.0)

rmse(x, ref)
#> [1] 0.4242641

# with linear model
fit <- lm(mpg ~ hp, data = mtcars)
rmse(fit)
#> [1] 3.740297
```
