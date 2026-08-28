# Mean Absolute Percentage Error

Computes the mean absolute percentage error (MAPE) between predicted and
reference values.

## Usage

``` r
mape(x, ...)

# S3 method for class 'lm'
mape(x, ...)

# Default S3 method
mape(x, ref, na.rm = FALSE, ...)
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

a numeric scalar containing the MAPE, as a **fraction**, not a
percentage: a mean absolute relative error of six percent is returned as
`0.06`. Multiply by 100 for the percentage form. The name is
conventional; the definition below is the one implemented.

## Details

The MAPE is defined as: \$\$ \frac{1}{n} \sum \left\| \frac{ref -
x}{ref} \right\| \$\$

Note that values where `ref = 0` lead to division by zero and result in
`NA`.

## See also

Other model.metrics: [`auc()`](auc.md), [`brierScore()`](brierScore.md),
[`mae()`](mae.md), [`mse()`](mse.md), [`nmae()`](nmae.md),
[`nmse()`](nmse.md), [`rmse()`](rmse.md), [`smape()`](smape.md)

## Examples

``` r
x <- c(2.5, 3.0, 2.8)
ref <- c(3.0, 2.5, 3.0)

mape(x, ref)
#> [1] 0.1444444

# with linear model
fit <- lm(mpg ~ hp, data = mtcars)
mape(fit)
#> [1] 0.1566944

```
