# Cohen's and Hedges' Effect Size

Computes the Cohen's d and Hedges' g effect size statistics.

## Usage

``` r
cohenD(
  x,
  y = NULL,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  correct = FALSE,
  na.rm = FALSE
)
```

## Arguments

- x:

  a non-empty numeric vector of data values

- y:

  an optional non-empty numeric vector of data values

- conf.level:

  confidence level of the interval. If set to `NA` (the default), only
  the point estimate is returned.

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See
  [`ConfidenceIntervals`](ConfidenceIntervals.md).

- correct:

  logical; whether to apply the Hedges correction. Defaults to `FALSE`.

- na.rm:

  logical. Should missing values be removed? Defaults to `FALSE`.

## Value

if `conf.level = NA`, a numeric scalar containing the effect size;
otherwise a named numeric vector with elements:

- `est`:

  point estimate of Cohen's \\d\\ or Hedges' \\g\\.

- `lci`:

  lower confidence interval bound.

- `uci`:

  upper confidence interval bound.

The magnitude category and pooled standard deviation are stored in the
attributes `magnitude` and `sdPooled`, respectively.

## Details

For a single sample, \\d = \bar{x} / s\\; for two samples, \\d =
(\bar{x} - \bar{y}) / s\_{pooled}\\. With `correct = TRUE` Hedges' bias
correction \\J = 1 - 3/(4\nu - 1)\\, with \\\nu\\ the residual degrees
of freedom, is applied to the estimate and, where computed, to the
interval.

Confidence intervals invert the noncentral \\t\\ distribution (Steiger &
Fouladi): the noncentrality parameter is \\d\sqrt{n}\\ with \\n - 1\\
degrees of freedom in the one-sample case, and \\d / \sqrt{1/n_x +
1/n_y}\\ with \\n_x + n_y - 2\\ degrees of freedom in the two-sample
case.

`sides` names the side on which the finite bound lies: `"left"` yields
\\\[lci, \infty)\\, `"right"` yields \\(-\infty, uci\]\\.

## Note

Based on code by William Revelle.

## References

Cohen, J. (1988) *Statistical power analysis for the behavioral sciences
(2nd ed.)* Academic Press, New York.

Hedges, L. V. & Olkin, I. (1985) *Statistical methods for meta-analysis*
Academic Press, Orlando, FL

Smithson, M.J. (2003) *Confidence Intervals, Quantitative Applications
in the Social Sciences Series*, No. 140. Thousand Oaks, CA: Sage. pp.
39-41

## See also

[`glassDelta`](glassDelta.md), [`meanX`](meanX.md), [`varX`](varX.md)

Other effect.size: [`cohenH()`](cohenH.md), [`etaSq()`](etaSq.md),
[`glassDelta()`](glassDelta.md), [`oddsRatio()`](oddsRatio.md),
[`relRisk()`](relRisk.md)

## Examples

``` r

x <- Pizza$price[Pizza$driver == "Carter"]
y <- Pizza$price[Pizza$driver == "Miller"]

cohenD(x, y, conf.level = 0.95, na.rm = TRUE)
#>          est          lci          uci 
#> -0.212277884 -0.430316026  0.006058519 
#> attr(,"magnitude")
#> [1] "small"
#> attr(,"sdPooled")
#> [1] 21.54513

# Hedges' g
cohenD(x, y, conf.level = 0.95, correct = TRUE, na.rm = TRUE)
#>          est          lci          uci 
#> -0.211827825 -0.429403695  0.006045674 
#> attr(,"magnitude")
#> [1] "small"
#> attr(,"sdPooled")
#> [1] 21.54513

# one-sided: the finite bound lies on the left
cohenD(x, y, conf.level = 0.95, sides = "left", na.rm = TRUE)
#>        est        lci        uci 
#> -0.2122779 -0.3952370        Inf 
#> attr(,"magnitude")
#> [1] "small"
#> attr(,"sdPooled")
#> [1] 21.54513
```
