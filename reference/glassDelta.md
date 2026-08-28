# Glass' Delta Effect Size

Computes Glass' delta, a standardized mean difference that uses the
standard deviation of the control group only, along with a noncentral-t
based confidence interval and an optional small-sample bias correction.

## Usage

``` r
glassDelta(
  x,
  y,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  useControlSd = TRUE,
  correct = FALSE,
  na.rm = FALSE
)
```

## Arguments

- x:

  numeric vector containing the treatment group

- y:

  numeric vector containing the control group

- conf.level:

  confidence level of the interval. If set to `NA` (the default), only
  the point estimate is returned.

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See
  [`ConfidenceIntervals`](ConfidenceIntervals.md).

- useControlSd:

  logical, if `TRUE` (default) the standard deviation of the control
  group `y` is used for standardization, otherwise the one of `x`

- correct:

  logical, if `TRUE` the exact small-sample bias correction (Hedges'
  correction with \\df = n_C - 1\\) is applied. Requires at least 3
  observations in the standardizing group. Default is `FALSE`.

- na.rm:

  logical, should missing values be removed? Default is `FALSE`. If
  `FALSE` and any of the groups contains missing values, `NA` is
  returned.

## Value

a named numeric vector. If `conf.level = NA`, only `est` is returned;
otherwise the vector has elements:

- `est`:

  point estimate of Glass' delta

- `lci`:

  lower confidence interval bound

- `uci`:

  upper confidence interval bound

In both cases the result carries the attribute `"magnitude"` with the
conventional interpretation of the estimate's absolute size
(`"negligible"` \< 0.2 \\\le\\ `"small"` \< 0.5 \\\le\\ `"medium"` \<
0.8 \\\le\\ `"large"`), analogous to [`cohenD()`](cohenD.md).

## Details

Glass' delta is defined as: \$\$ \Delta = \frac{\bar{x} - \bar{y}}{s_y}
\$\$ where \\s_y\\ is the standard deviation of the control group. It is
preferred over Cohen's d when the treatment is expected to affect the
variance, so that the control group's variability is the natural
reference scale.

The confidence interval is obtained by inverting the noncentral
t-distribution with \\df = n_C - 1\\ degrees of freedom, where \\n_C\\
is the size of the group supplying the standard deviation (Kelley,
2007). Note that this interval assumes equal population variances in
both groups. Since Glass' delta is typically chosen precisely when the
variances are expected to differ, the interval should be regarded as
approximate under heteroscedasticity.

With `correct = TRUE` the exact correction factor \$\$ J(df) =
\frac{\Gamma(df/2)}{\sqrt{df/2}\\\Gamma((df-1)/2)} \$\$ is applied to
the estimate and both confidence limits.

## Note

The confidence interval method follows Ken Kelley's approach previously
published in the MBESS package, reimplemented to conform to package
standards.

## References

Glass, G. V. (1976) Primary, secondary, and meta-analysis of research.
*Educational Researcher*, 5(10), 3-8.

Hedges, L. V., Olkin, I. (1985) *Statistical Methods for Meta-Analysis*.
Orlando: Academic Press.

Kelley, K. (2007) Confidence intervals for standardized effect sizes:
Theory, application, and implementation. *Journal of Statistical
Software*, 20(8), 1-24.

## See also

[`cohenD`](cohenD.md)

Other effect.size: [`cohenD()`](cohenD.md), [`cohenH()`](cohenH.md),
[`etaSq()`](etaSq.md), [`oddsRatio()`](oddsRatio.md),
[`relRisk()`](relRisk.md)

## Examples

``` r
set.seed(5)
x <- rnorm(30, mean = 1)
y <- rnorm(30, mean = 0)

glassDelta(x, y)
#>       est 
#> 0.8118633 
#> attr(,"magnitude")
#> [1] "large"

glassDelta(x, y, conf.level = 0.95)
#>       est       lci       uci 
#> 0.8118633 0.2587262 1.3530160 
#> attr(,"magnitude")
#> [1] "large"

# one-sided: "right" bounds the interval from ABOVE
glassDelta(x, y, conf.level = 0.95, sides = "right")
#>       est       lci       uci 
#> 0.8118633      -Inf 1.2646612 
#> attr(,"magnitude")
#> [1] "large"

# ... and "left" from below
glassDelta(x, y, conf.level = 0.95, sides = "left")
#>       est       lci       uci 
#> 0.8118633 0.3463020       Inf 
#> attr(,"magnitude")
#> [1] "large"

# small-sample bias correction
glassDelta(x, y, conf.level = 0.95, correct = TRUE)
#>       est       lci       uci 
#> 0.7906533 0.2519670 1.3176684 
#> attr(,"magnitude")
#> [1] "medium"

# standardize by the treatment group instead
glassDelta(x, y, useControlSd = FALSE)
#>       est 
#> 0.8598789 
#> attr(,"magnitude")
#> [1] "large"

```
