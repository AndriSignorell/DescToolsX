# Symmetric Mean Absolute Percentage Error

Computes the symmetric mean absolute percentage error (SMAPE) between
predicted and reference values.

## Usage

``` r
smape(x, ...)

# S3 method for class 'lm'
smape(x, ...)

# Default S3 method
smape(x, ref, na.rm = FALSE, ...)
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

  logical; whether to remove missing and undefined terms

## Value

a numeric scalar containing the SMAPE

## Details

The SMAPE is defined as: \$\$ \frac{1}{n} \sum \frac{2 \|ref -
x\|}{\|x\| + \|ref\|} \$\$

Values are bounded between 0 and 2. Note that this is a ratio, not a
percentage: the factor 100 of the original definition is not applied,
which is the form for which the bound of 2 holds.

A term is undefined when `x` and `ref` are both zero, and is set to
`NA`. With the default `na.rm = FALSE` a single such pair therefore
makes the whole result `NA`; with `na.rm = TRUE` those terms are dropped
along with genuinely missing ones, so the mean is taken over fewer than
`length(x)` terms.

## See also

Other model.metrics: [`auc()`](auc.md), [`brierScore()`](brierScore.md),
[`mae()`](mae.md), [`mape()`](mape.md), [`mse()`](mse.md),
[`nmae()`](nmae.md), [`nmse()`](nmse.md), [`rmse()`](rmse.md)

## Examples

``` r
x <- c(2.5, 3.0, 2.8)
ref <- c(3.0, 2.5, 3.0)

smape(x, ref)
#> [1] 0.1442006

# with linear model
fit <- lm(mpg ~ hp, data = mtcars)
smape(fit)
#> [1] 0.1550795

```
