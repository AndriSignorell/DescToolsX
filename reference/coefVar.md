# Coefficient of Variation

Calculates the coefficient of variation and its confidence limits using
various methods.

## Usage

``` r
coefVar(x, ...)

# Default S3 method
coefVar(x, weights = NULL, unbiased = FALSE, na.rm = FALSE, ...)

# S3 method for class 'lm'
coefVar(x, unbiased = FALSE, na.rm = FALSE, ...)

# S3 method for class 'aov'
coefVar(x, unbiased = FALSE, na.rm = FALSE, ...)

coefVarCI(
  x,
  conf.level = 0.95,
  sides = c("two.sided", "left", "right"),
  method = c("nct", "vangel", "mckay", "naive"),
  weights = NULL,
  unbiased = FALSE,
  na.rm = FALSE,
  ...
)
```

## Arguments

- x:

  a non-empty numeric vector of data values, or a fitted model for the
  `lm`/`aov` methods

- ...:

  further arguments

- weights:

  a numeric vector of weights the same length as `x` giving the weights
  to use for elements of `x`

- unbiased:

  logical; whether to apply a bias correction. See Details. Defaults to
  `FALSE`.

- na.rm:

  logical. Should missing values be removed? Defaults to `FALSE`, in
  which case missing values are an error.

- conf.level:

  confidence level of the interval. If set to `NA` (the default), only
  the point estimate is returned.

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See
  [`ConfidenceIntervals`](ConfidenceIntervals.md).

- method:

  character string specifying the confidence interval method: `"nct"`
  (default), `"vangel"`, `"mckay"`, or `"naive"`. Partial matching is
  supported. See Details.

## Value

an unnamed numeric scalar containing the coefficient of variation for
`coefVar()`. If recycling in `coefVarCI()` yields a single case, it
returns a named numeric vector with elements:

- `est`:

  point estimate of the coefficient of variation.

- `lci`:

  lower confidence interval bound.

- `uci`:

  upper confidence interval bound.

Otherwise, `coefVarCI()` returns a numeric matrix with one row per case
and the columns `est`, `lci`, and `uci`.

## Details

In order for the coefficient of variation to be an unbiased estimate of
the true population value, the coefficient of variation is corrected as:
\$\$ CV\_{korr} = CV \cdot \left( 1 - \frac{1}{4\cdot(n-1)} +
\frac{1}{n} \cdot CV^2 + \frac{1}{2 \cdot (n-1)^2} \right) \$\$

For determining` `**the confidence intervals**` ` for the coefficient of
variation a number of methods have been proposed. `coefVarCI()`
currently supports four different methods. The details for the methods
are given in the specific references.

The **"naive" method** ` ` is based on dividing the standard confidence
limit for the standard deviation by the sample mean.

**McKay's** ` ` approximation is asymptotically exact as n goes to
infinity. McKay recommends this approximation only if the coefficient of
variation is less than 0.33. Note that if the coefficient of variation
is greater than 0.33, either the normality of the data is suspect or the
probability of negative values in the data is non-negligible. In this
case, McKay's approximation may not be valid. Also, it is generally
recommended that the sample size should be at least 10 before using
McKay's approximation.

**Vangel's modified McKay method** ` ` is more accurate than the McKay
method in most cases, particularly for small samples. According to
Vangel, the unmodified McKay is only more accurate when both the
coefficient of variation and alpha are large. However, if the
coefficient of variation is large, then this implies either that the
data contains negative values or the data does not follow a normal
distribution. In this case, neither the McKay or the modified McKay
should be used. In general, the Vangel's modified McKay method is
recommended over the McKay method. It generally provides good
approximations as long as the data is approximately normal and the
coefficient of variation is less than 0.33.

See also:
https://www.itl.nist.gov/div898/software/dataplot/refman1/auxillar/coefvacl.htm

**nct** ` `uses the noncentral t-distribution to calculate the
confidence intervals. See Smithson (2003).

`sides` names the side on which the finite bound lies: `"left"` yields
\\\[lci, \infty)\\, `"right"` yields \\(-\infty, uci\]\\.

**Note:**` ` Analytic (precision) weights are not supported. For
likelihood-based weighted variance estimation, see
[`cov.wt`](https://rdrr.io/r/stats/cov.wt.html).

## Note

Parts of the code contributed by Michael Smithson.

## References

McKay, A. T. (1932). Distribution of the coefficient of variation and
the extended *t* distribution, *Journal of the Royal Statistical
Society*, *95*, 695–698.

Johnson, B. L., Welch, B. L. (1940). Applications of the non-central
t-distribution. *Biometrika*, 31, 362–389.

Mark Vangel (1996) Confidence Intervals for a Normal Coefficient of
Variation, *American Statistician*, Vol. 15, No. 1, pp. 21-26.

Kelley, K. (2007). Sample size planning for the coefficient of variation
from the accuracy in parameter estimation approach. *Behavior Research
Methods, 39* (4), 755-766

Kelley, K. (2007). Constructing confidence intervals for standardized
effect sizes: Theory, application, and implementation. *Journal of
Statistical Software, 20* (8), 1-24

Smithson, M.J. (2003) *Confidence Intervals, Quantitative Applications
in the Social Sciences Series*, No. 140. Thousand Oaks, CA: Sage. pp.
39-41

Steve Verrill (2003) Confidence Bounds for Normal and Lognormal
Distribution Coefficients of Variation, *Research Paper 609*, USDA
Forest Products Laboratory, Madison, Wisconsin.

Verrill, S. and Johnson, R.A. (2007) Confidence Bounds and Hypothesis
Tests for Normal Distribution Coefficients of Variation, *Communications
in Statistics Theory and Methods*, Volume 36, No. 12, pp 2187-2206.

## See also

[`meanX`](meanX.md), [`sdX`](varX.md), (both supporting weights)

Other dispersion: [`iqrX()`](iqrX.md), [`madX()`](madX.md),
[`meanAD()`](meanAD.md), [`meanSE()`](meanSE.md),
[`rangeX()`](rangeX.md), [`varX()`](varX.md)

## Examples

``` r

set.seed(15)
x <- runif(100)
coefVar(x)
#> [1] 0.5092566
coefVarCI(x)
#>       est       lci       uci 
#> 0.5092566 0.4351644 0.6151409 

# Coefficient of variation for a linear model
r.lm <- lm(Fertility ~ ., swiss)
coefVar(r.lm)
#> [1] 0.1021544

# conf.level is recycled, so several levels can be requested at once.
# Reproduces the NIST reference table for the ZARR13 dataset (see the
# URL above) from a sample rebuilt to its published mean and sd, so the
# example needs no network access.
set.seed(1)
zarr <- 9.261460 + 0.022789 * as.vector(scale(rnorm(195)))

round(coefVarCI(zarr, method = "vangel", sides = "two.sided",
                conf.level = c(.5, .8, .9, .95, .99, .999)), 5)
#>          est     lci     uci
#> [1,] 0.00246 0.00238 0.00255
#> [2,] 0.00246 0.00231 0.00263
#> [3,] 0.00246 0.00227 0.00269
#> [4,] 0.00246 0.00224 0.00273
#> [5,] 0.00246 0.00217 0.00283
#> [6,] 0.00246 0.00210 0.00294

# ---------------------------------------------------------
#  Confidence    Coefficient          Lower          Upper
#   Value (%)   of Variation          Limit          Limit
# ---------------------------------------------------------
#        50.0        0.00246        0.00238        0.00255
#        80.0        0.00246        0.00231        0.00263
#        90.0        0.00246        0.00227        0.00269
#        95.0        0.00246        0.00224        0.00273
#        99.0        0.00246        0.00217        0.00283
#        99.9        0.00246        0.00210        0.00294
```
