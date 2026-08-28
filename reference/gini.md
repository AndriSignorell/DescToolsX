# Gini Coefficient

Computes the Gini coefficient, a widely used measure of inequality,
optionally with bootstrap confidence intervals.

## Usage

``` r
gini(
  x,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  unbiased = TRUE,
  weights = NULL,
  na.rm = FALSE,
  ...
)
```

## Arguments

- x:

  numeric vector of non-negative values

- conf.level:

  confidence level of the interval. If set to `NA` (the default), only
  the point estimate is returned.

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See
  [`ConfidenceIntervals`](ConfidenceIntervals.md).

- unbiased:

  logical; whether to apply the small-sample bias correction factor
  \\n/(n-1)\\, with \\n\\ the effective sample size (the sum of the
  weights). For unweighted data this is the usual \\1/(1 - \sum
  w_i^2)\\.

- weights:

  optional non-negative numeric vector with the same length as `x`

- na.rm:

  logical; whether to remove missing values before computation

- ...:

  additional arguments passed to the bootstrap procedure:

  `type`

  :   confidence interval type (default `"bca"`)

  `R`

  :   number of bootstrap replications (default 999)

  `parallel`

  :   parallelization mode (`"no"`, `"multicore"`, or `"snow"`)

  `ncpus`

  :   number of CPUs

## Value

if `conf.level = NA`, a numeric scalar. Otherwise a named numeric vector
with elements:

- `est`:

  point estimate of the Gini coefficient

- `lci`:

  lower confidence interval bound

- `uci`:

  upper confidence interval bound

## Details

The Gini coefficient ranges from 0 (perfect equality) to 1 (maximal
inequality). For finite samples, the uncorrected estimator is biased;
setting `unbiased = TRUE` applies a standard correction factor.

Weights are interpreted as frequency (replication) weights, meaning that
each observation contributes proportionally to its weight in the
empirical distribution.

Confidence intervals are obtained via bootstrap resampling using the
boot package. The default interval type is bias-corrected and
accelerated ("bca").

The implementation uses a numerically stable formulation based on the
Lorenz curve:

\$\$ G = \frac{1}{\mu} \sum_i w_i x_i (2F_i - 1) \$\$

where \\F_i\\ are midpoints of cumulative weights and \\\mu\\ is the
weighted mean.

`sides` names the side on which the finite bound lies: `"left"` yields
an interval bounded below, `"right"` one bounded above. The coefficient
is bounded, so the open side is reported at the range boundary (0 or 1)
rather than as \\\pm\infty\\ (design_rules.md 4.1).

## Random number generation

A confidence level triggers a bootstrap and therefore advances R's
global random number generator. Call
[`set.seed`](https://rdrr.io/r/base/Random.html) beforehand for
reproducible intervals.

## See also

Other inequality: [`atkinson()`](atkinson.md),
[`divCoef()`](divCoef.md), [`lc()`](Lc.md),
[`rosenbluth()`](rosenbluth.md), [`theil()`](theil.md)

## Examples

``` r
x <- c(10, 20, 30, 40)
gini(x)
#> [1] 0.3333333

# weighted example
gini(c(10, 0), weights = c(2, 3))
#> [1] 0.75

# with confidence interval
gini(x, conf.level = 0.95, R = 499)
#>       est       lci       uci 
#> 0.3333333 0.1111111 0.4285714 
```
