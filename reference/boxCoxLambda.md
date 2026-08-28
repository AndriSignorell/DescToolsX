# Automatic Selection of Box-Cox Transformation Parameter

Selects the Box-Cox transformation parameter automatically, using either
Guerrero's method or the profile log likelihood.  
Guerrero's (1993) method yields a lambda which minimizes the coefficient
of variation for subseries of `x`. For method `"loglik"`, the value of
lambda is chosen to maximize the profile log likelihood of a linear
model fitted to `x`. For non-seasonal data, a linear time trend is
fitted while for seasonal data, a linear time trend with seasonal dummy
variables is used.

## Usage

``` r
boxCoxLambda(
  x,
  method = c("guerrero", "loglik"),
  lower = -1,
  upper = 2,
  nonseasonalLength = 2
)
```

## Arguments

- x:

  a numeric vector or univariate time series. All values must be
  strictly positive and finite, as the Box-Cox transformation is
  undefined otherwise; missing values are not removed but rejected,
  since subsetting would strip a `ts` of its frequency and cycle
  positions.

- method:

  method to be used in calculating lambda. Can be either `"guerrero"`
  (default) or `"loglik"`.

- lower:

  lower limit for possible lambda values; defaults to -1

- upper:

  upper limit for possible lambda values; defaults to 2

- nonseasonalLength:

  number of observations per subseries used by the `"guerrero"` method
  for non-seasonal data, default is 2. Must be a whole number \\\ge 2\\.
  For seasonal time series the series' own frequency is used instead,
  whenever it is larger.

## Value

a numeric scalar containing the estimated Box-Cox transformation
parameter

## Details

Seasonality is taken from `x` itself: a
[`ts`](https://rdrr.io/r/stats/ts.html) object with `frequency(x) > 1`
is treated as seasonal, anything else (including a plain numeric vector)
as non-seasonal. For method `"loglik"` the profile log likelihood is
therefore computed from `lm(x ~ trend)` for non-seasonal data and from
`lm(x ~ trend + factor(cycle(x)))` for seasonal data. Both methods
optimise lambda continuously over `[lower, upper]` via
[`optimize`](https://rdrr.io/r/stats/optimize.html).

Both methods need enough data to identify their criterion, and signal an
error rather than falling back silently when they do not have it:
`"loglik"` requires at least three observations, and more than
`frequency(x) + 1` for a seasonal series, that being the number of
parameters in the seasonal model (intercept, trend and
`frequency(x) - 1` dummies); `"guerrero"` requires at least two complete
subseries. Constant series are rejected by both, since the coefficient
of variation degenerates to \\0/0\\ and the profile log likelihood is
singular.

## Note

Based on code by Leanne Chhay and Rob J Hyndman previously published as
`BoxCox.lambda()` in the forecast package, adapted to conform to package
standards.

## References

Box, G. E. P. and Cox, D. R. (1964) An analysis of transformations.
*JRSS B* **26** 211–246.

Guerrero, V.M. (1993) Time-series analysis supported by power
transformations. *Journal of Forecasting*, **12**, 37–48.

## See also

Other transform: [`boxCox()`](boxCox.md), [`logSt()`](logSt.md),
[`scaleX()`](scaleX.md), [`yeoJohnson()`](yeoJohnson.md)

## Examples

``` r
lambda <- boxCoxLambda(AirPassengers)

# profile log likelihood, seasonal trend model
boxCoxLambda(AirPassengers, method = "loglik")
#> [1] 0.1977788

# plain numeric vector, treated as non-seasonal
set.seed(1)
boxCoxLambda(rlnorm(100), method = "loglik")
#> [1] 0.02665803
```
