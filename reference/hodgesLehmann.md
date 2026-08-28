# Hodges-Lehmann Estimator of Location

Function to compute the Hodges-Lehmann estimator of location in the one
and two sample case following a clever fast algorithm by John Monahan
(1984).

## Usage

``` r
hodgesLehmann(
  x,
  y = NULL,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  na.rm = FALSE,
  ...
)
```

## Arguments

- x:

  numeric vector

- y:

  optional numeric vector

- conf.level:

  confidence level of the interval. If set to `NA` (the default), only
  the point estimate is returned.

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See
  [`ConfidenceIntervals`](ConfidenceIntervals.md).

- na.rm:

  logical; whether to remove missing values

- ...:

  additional arguments passed to bootstrap procedures

## Value

if `conf.level = NA`, a numeric scalar. Otherwise a named numeric vector
with elements:

- `est`:

  point estimate of the Hodges-Lehmann location

- `lci`:

  lower confidence interval bound

- `uci`:

  upper confidence interval bound

## Details

The Hodges-Lehmann estimator is the median of the combined data points
and Walsh averages.

It is the same as the pseudo median returned as a by-product of
[`wilcox.test`](https://rdrr.io/r/stats/wilcox.test.html) (which however
does not calculate correctly as soon as ties are present).

Note that in the two-sample case the estimator for the difference in
location parameters does not estimate the difference in medians (a
common misconception) but rather the median of the difference between a
sample from x and a sample from y.

`sides` names the side on which the finite bound lies: `"left"` yields
\\\[lci, \infty)\\, `"right"` yields \\(-\infty, uci\]\\. The estimator
is unbounded, so the open side is reported as \\\pm\infty\\.

`x` and `y` are not modified.

## Note

C++ port of Monahan’s algorithm by Cyril Flurin Moser

## Random number generation

A confidence level triggers a bootstrap and therefore advances R's
global random number generator. Call
[`set.seed`](https://rdrr.io/r/base/Random.html) beforehand for
reproducible intervals. The point estimate itself is deterministic: the
compiled routine picks its pivots from a local generator and does not
touch R's stream.

## See also

[`wilcox.test`](https://rdrr.io/r/stats/wilcox.test.html)

Other location: [`gmean()`](gmean.md), [`hmean()`](hmean.md),
[`huberM()`](huberM.md), [`meanX()`](meanX.md),
[`medianX()`](medianX.md), [`modeX()`](modeX.md),
[`tukeyBiweight()`](tukeyBiweight.md)

## Examples

``` r
x <- c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
hodgesLehmann(x)
#> [1] 1.725

# the input is left alone
v <- c(3, 1, 2)
hodgesLehmann(v)
#> [1] 2
v
#> [1] 3 1 2

# two-sample: median of the pairwise differences, NOT the difference
# of the medians
y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
hodgesLehmann(x, y)
#> [1] 0.56

set.seed(1)
hodgesLehmann(x, conf.level = 0.95)
#>      est      lci      uci 
#> 1.725000 1.425000 2.211442 
```
