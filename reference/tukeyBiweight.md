# Tukey's Biweight Mean

Computes Tukey's biweight robust mean (also known as the bisquare mean)
of a numeric vector, optionally with a bootstrap confidence interval.

## Usage

``` r
tukeyBiweight(
  x,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  const = 9,
  na.rm = FALSE,
  ...
)
```

## Arguments

- x:

  a non-empty numeric vector of data values

- conf.level:

  confidence level of the interval. If set to `NA` (the default), only
  the point estimate is returned.

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See
  [`ConfidenceIntervals`](ConfidenceIntervals.md).

- const:

  tuning constant passed to `tbrm_cpp()`. Defaults to `9`.

- na.rm:

  logical. Should missing values be removed before computation? Defaults
  to `FALSE`.

- ...:

  further arguments passed to the bootstrap engine when a confidence
  interval is requested, namely `R` and `type`. Any other name is an
  error rather than a silent no-op.

## Value

if `conf.level = NA`, a numeric scalar. Otherwise a named numeric vector
with elements:

- `est`:

  point estimate of Tukey's biweight mean

- `lci`:

  lower confidence interval bound

- `uci`:

  upper confidence interval bound

## Details

The biweight mean is a robust location estimator that downweights
observations far from the median. It is defined via the tuning constant
`const` (default 9), which controls the breakdown point: larger values
are less resistant but more efficient under normality.

When `conf.level` is not `NA` a bootstrap confidence interval is
returned. The resampling is done in C++; the R random number generator
is used only to draw the seed, so
[`set.seed()`](https://rdrr.io/r/base/Random.html) makes the result
reproducible. Bootstrap arguments are passed through `...`:

- `R`:

  Number of bootstrap replicates (default `999`).

- `type`:

  CI type: `"perc"` or `"bca"` (default).

The biweight mean is a location estimator and therefore unbounded, so
the open side of a one-sided interval is reported at \\\pm\infty\\ -
unlike the bounded measures in this package, where it is reported at the
range limit. See [`ConfidenceIntervals`](ConfidenceIntervals.md).

## See also

Other location: [`gmean()`](gmean.md), [`hmean()`](hmean.md),
[`hodgesLehmann()`](hodgesLehmann.md), [`huberM()`](huberM.md),
[`meanX()`](meanX.md), [`medianX()`](medianX.md), [`modeX()`](modeX.md)

## Examples

``` r
set.seed(1)
x <- c(rnorm(50), 10)   # one outlier

tukeyBiweight(x)
#> [1] 0.1340368

set.seed(2)             # will yield reproducible intervals
tukeyBiweight(x, conf.level = 0.95)
#>        est        lci        uci 
#>  0.1340368 -0.1244454  0.3469211 
tukeyBiweight(x, conf.level = 0.95, type = "perc", R = 499)
#>        est        lci        uci 
#>  0.1340368 -0.0987742  0.3713162 
tukeyBiweight(x, conf.level = 0.95, type = "bca", R = 499)
#>        est        lci        uci 
#>  0.1340368 -0.1150934  0.3618559 

# one-sided: "left" carries the finite lower bound
set.seed(2)
tukeyBiweight(x, conf.level = 0.95, sides = "left")
#>         est         lci         uci 
#>  0.13403675 -0.08950033         Inf 

```
