# Compute Area Under the Curve

Calculates the area under a curve defined by points `(x, y)` using
different numerical integration methods.

## Usage

``` r
auc(
  x,
  y,
  from = min(x, na.rm = TRUE),
  to = max(x, na.rm = TRUE),
  method = c("trapezoid", "step", "spline"),
  absoluteArea = FALSE,
  subdivisions = 100,
  na.rm = FALSE,
  ...
)
```

## Arguments

- x, y:

  numeric vectors of equal length defining the curve coordinates

- from, to:

  single numeric values specifying the integration interval; by default,
  the range of `x`

- method:

  character string specifying the integration method: `"trapezoid"`,
  `"step"`, or `"spline"`

- absoluteArea:

  logical; whether areas below zero are counted as positive

- subdivisions:

  positive whole number specifying the maximum number of subdivisions
  used for spline integration

- na.rm:

  logical; whether incomplete `(x, y)` pairs are removed

- ...:

  additional arguments passed to
  [`approx`](https://rdrr.io/r/stats/approxfun.html) for trapezoidal
  interpolation

## Value

a numeric value representing the computed area

## Details

The available methods are:

- `"trapezoid"`: linear interpolation between successive points

- `"step"`: a right-continuous step function using the value at the left
  endpoint of each interval

- `"spline"`: a natural cubic spline integrated numerically

For `method = "step"`, an integration boundary lying between two
observed `x` values retains the value of the preceding point. No linear
interpolation is performed at the boundary.

If `absoluteArea = TRUE`, the absolute value of the interpolated curve
is integrated. Sign changes in linear segments are split at their exact
zero-crossing. For the step method, the absolute values of the constant
step heights are used.

Both integration limits must lie inside the range of `x`. Extrapolation
is not performed.

## See also

[`approx`](https://rdrr.io/r/stats/approxfun.html),
[`splinefun`](https://rdrr.io/r/stats/splinefun.html),
[`integrate`](https://rdrr.io/r/stats/integrate.html)

Other model.metrics: [`brierScore()`](brierScore.md), [`mae()`](mae.md),
[`mape()`](mape.md), [`mse()`](mse.md), [`nmae()`](nmae.md),
[`nmse()`](nmse.md), [`rmse()`](rmse.md), [`smape()`](smape.md)

## Examples

``` r
x <- c(1, 2, 3, 5)
y <- c(0, 1, 1, 2)

auc(x, y)
#> [1] 4.5
auc(x, y, method = "step")
#> [1] 3
auc(x, y, method = "spline")
#> [1] 4.347826
auc(x, y, absoluteArea = TRUE)
#> [1] 4.5

# interval boundaries between observed x values
auc(
  x = c(0, 1, 2),
  y = c(-2, 10, 4),
  from = 0.5,
  to = 1.5,
  method = "step"
)
#> [1] 4

auc(
  x = c(0, 1, 2),
  y = c(-2, 10, 4),
  from = 0.5,
  to = 1.5,
  method = "step",
  absoluteArea = TRUE
)
#> [1] 6
```
