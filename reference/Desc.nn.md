# Describe a Numeric-Numeric Relationship

Computes, prints and plots a comprehensive bivariate description for two
quantitative variables. The function is dispatched automatically by
`desc(y ~ x, data)` when both `y` and `x` are numeric.

## Usage

``` r
.descNN(x, y, conf.level = 0.95)

# S3 method for class 'Desc.nn'
print(x, verbose = NULL, abs.sty = NULL, per.sty = NULL, ...)

# S3 method for class 'Desc.nn'
plot(x, main = x$meta$main, which = 1, verbose = NULL, ...)
```

## Arguments

- x:

  numeric predictor for `.descNN()`, or an object of class `"Desc.nn"`
  for the print and plot methods

- verbose:

  integer controlling the amount of output (1, 2, or 3). `NULL`
  (default) falls back to
  `x$meta$verbose %||% getOption("DescTools.verbose", 2)`.

- abs.sty:

  format style for counts. `NULL` falls back to
  `getOption("DescTools.abs.sty")`.

- per.sty:

  format style for proportions. `NULL` falls back to
  `getOption("DescTools.per.sty")`.

- ...:

  further arguments passed to the underlying plot functions

- main:

  main title for the plot. Defaults to the title stored in
  `x$meta$main`.

- which:

  integer vector selecting which plots to draw. See Details. `NULL`
  (default) selects plots automatically based on `verbose`.

- y:

  numeric response variable

- conf.level:

  confidence level for interval estimates (default 0.95)

## Value

`.descNN()` returns an object of class `c("Desc.nn", "Desc")`. Its
`lm$intercept` and `lm$slope` components contain:

- `est`:

  point estimate of the coefficient

- `lci`:

  lower confidence interval bound

- `uci`:

  upper confidence interval bound

- `p`:

  p-value

The print and plot methods return `x` invisibly.

## Details

**Print output by verbose level:**

- `verbose = 1`:

  Summary (n, missings), Pearson r and Spearman r each with confidence
  interval and effect size label, linear regression coefficients
  (estimate, CI, significance) and R².

- `verbose = 2` (default):

  All of the above, plus residual standard error and Shapiro-Wilk test
  on residuals.

- `verbose = 3`:

  All of the above, plus Breusch-Pagan test for heteroscedasticity and
  Cook's distance summary.

**Confidence intervals** are reported throughout instead of standard
errors and t-values, using
[`confint()`](https://rdrr.io/r/stats/confint.html) for regression
coefficients and
[`corCI()`](https://andrisignorell.github.io/lumen/reference/corCI.html)
(Fisher z-transform) for correlations.

**Effect size labels** for correlations follow Cohen (1988):

|              |                            |
|--------------|----------------------------|
| `negligible` | \|r\| \< 0.10              |
| `small`      | 0.10 \\\le\\ \|r\| \< 0.30 |
| `moderate`   | 0.30 \\\le\\ \|r\| \< 0.50 |
| `large`      | \|r\| \\\ge\\ 0.50         |

**Plot options via `which`:**

- `which = 1`:

  Scatterplot with linear regression line and confidence band.

- `which = 2`:

  Scatterplot with Loess smoother and confidence band (via
  `lines.loess()`).

- `which = 3`:

  Residual plot: residuals vs. fitted values.

- `which = 4`:

  Q-Q plot of residuals.

Default `which` by verbose level:

- `verbose = 1`: `which = 1`

- `verbose = 2`: `which = 1:2`

- `verbose = 3`: `which = 1:4`

## References

Cohen, J. (1988). *Statistical Power Analysis for the Behavioral
Sciences* (2nd ed.). Lawrence Erlbaum Associates.

Breusch, T.S. and Pagan, A.R. (1979). A simple test for
heteroscedasticity and random coefficient variation. *Econometrica*, 47,
1287–1294.

## See also

[`desc`](Desc.md) for the generic entry point, [`desc.nq`](desc.nq.md)
for numeric ~ categorical, [`desc.qn`](desc.qn.md) for categorical ~
numeric, [`desc.qq`](desc.qq.md) for categorical ~ categorical,
[`corCI`](https://andrisignorell.github.io/lumen/reference/corCI.html),
[`bpTest`](https://andrisignorell.github.io/lumen/reference/bpTest.html),
[`lm`](https://rdrr.io/r/stats/lm.html),
[`cor.test`](https://rdrr.io/r/stats/cor.test.html)

Other desc: [`desc()`](Desc.md), [`desc.Date()`](Desc.Date.md),
[`desc.factor()`](Desc.factor.md), [`desc.nq`](desc.nq.md),
[`desc.numeric()`](desc.numeric.md), [`desc.qn`](desc.qn.md),
[`desc.qq`](desc.qq.md), [`desc.ts()`](desc.ts.md),
[`print.Desc.qq()`](desc.table.md)

## Examples

``` r
# basic usage via desc()
desc(mpg ~ wt, mtcars)
#> ────────────────────────────────────────────────────────────────────────────── 
#> mpg ~ wt (mtcars) (Desc.nn)
#> 
#> Summary:
#> pairs: 32, valid: 32 (100.0%), missings: 0 (0.0%)
#> 
#> 
#> Pearson  r:  -0.868  (-0.934, -0.744)  ***  large
#> Spearman r:  -0.886  (-0.943, -0.778)  ***  large
#> 
#> Linear regression:
#>   Intercept:    6.0473  (  5.4168,   6.6777)  ***
#>   Slope:       -0.1409  ( -0.1710,  -0.1108)  ***
#>   R²: 0.753   adj. R²: 0.745   p: <0.001
#>   Residual SE: 0.4945 on 30 df
#>   Shapiro-Wilk on residuals: W = 0.919,  p = 0.02
#> 


# more detail
desc(mpg ~ wt, mtcars, verbose = 3)
#> ────────────────────────────────────────────────────────────────────────────── 
#> mpg ~ wt (mtcars) (Desc.nn)
#> 
#> Summary:
#> pairs: 32, valid: 32 (100.0%), missings: 0 (0.0%)
#> 
#> 
#> Pearson  r:  -0.868  (-0.934, -0.744)  ***  large
#> Spearman r:  -0.886  (-0.943, -0.778)  ***  large
#> 
#> Linear regression:
#>   Intercept:    6.0473  (  5.4168,   6.6777)  ***
#>   Slope:       -0.1409  ( -0.1710,  -0.1108)  ***
#>   R²: 0.753   adj. R²: 0.745   p: <0.001
#>   Residual SE: 0.4945 on 30 df
#>   Shapiro-Wilk on residuals: W = 0.919,  p = 0.02
#> 


# store result and plot separately
d <- desc(mpg ~ wt, mtcars, plotit = FALSE)
print(d, verbose = 1)
#> ────────────────────────────────────────────────────────────────────────────── 
#> mpg ~ wt (mtcars) (Desc.nn)
#> 
#> Summary:
#> pairs: 32, valid: 32 (100.0%), missings: 0 (0.0%)
#> 
#> 
#> Pearson  r:  -0.868  (-0.934, -0.744)  ***  large
#> Spearman r:  -0.886  (-0.943, -0.778)  ***  large
#> 
#> Linear regression:
#>   Intercept:    6.0473  (  5.4168,   6.6777)  ***
#>   Slope:       -0.1409  ( -0.1710,  -0.1108)  ***
#>   R²: 0.753   adj. R²: 0.745   p: <0.001
#>   Residual SE: 0.4945 on 30 df
#>   Shapiro-Wilk on residuals: W = 0.919,  p = 0.02
#> 
plot(d, which = 1:2)


# pipe
desc(mpg ~ wt, mtcars) |> plot(which = 3)

```
