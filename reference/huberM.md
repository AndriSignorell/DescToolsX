# Safe (Generalized) Huber M-Estimator of Location

A (generalized) Huber M-estimator of location with MAD scale that
handles the degenerate case of zero scale gracefully, where
[`huber()`](https://rdrr.io/pkg/MASS/man/huber.html) would return an
error.

## Usage

``` r
huberM(
  x,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  method = c("wald", "boot"),
  k = 1.345,
  mu = NULL,
  s = NULL,
  na.rm = FALSE,
  ...
)
```

## Arguments

- x:

  numeric vector of data values

- conf.level:

  confidence level of the interval. If set to `NA` (the default), only
  the point estimate is returned.

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See
  [`ConfidenceIntervals`](ConfidenceIntervals.md).

- method:

  confidence interval method: `"wald"` (default) or `"boot"`.

- k:

  positive tuning constant; the algorithm winsorizes at `k` standard
  deviations. Default is `1.345`.

- mu:

  initial location estimate. `NULL` (default) uses `median(x)`, computed
  after `na.rm` is applied.

- s:

  scale estimate held constant through the iterations. `NULL` (default)
  uses `mad(x, center = mu)`, computed after `na.rm` is applied.

- na.rm:

  logical; whether to remove missing values before computation; default
  is `FALSE`

- ...:

  further arguments passed to the bootstrap engine when
  `method = "boot"`: `R`, `type`, `parallel`, and `ncpus`; see Details

## Value

if `conf.level = NA`, a numeric scalar. Otherwise a named numeric vector
with elements:

- `est`:

  location estimate from Huber's M-estimator

- `lci`:

  lower confidence interval bound

- `uci`:

  upper confidence interval bound

## Details

**Wald interval** (`method = "wald"`)

Uses an empirical sandwich standard error for the Huber M-estimator:
\$\$ \hat\mu \\\pm\\ t\_{1-\alpha/2,\\n-1} \hat s
\sqrt{\frac{\hat\tau}{n}}, \$\$ where \$\$ \hat\tau = \frac{n
\sum\_{i=1}^n \psi_k(r_i)^2} {\left\\\sum\_{i=1}^n I(\|r_i\| \leq
k)\right\\^2}, \qquad r_i = \frac{x_i-\hat\mu}{\hat s}. \$\$ Here,
\\\psi_k\\ denotes the Huber score function. No additional small-sample
adjustment is applied beyond use of the t-quantile.

**Bootstrap interval** (`method = "boot"`)

The statistic \\\hat\mu\\ is resampled \\R\\ times. Note that `mu` and
`s` are fixed at their initial values (computed from the full data
before resampling) and are not re-estimated on each resample. The
bootstrap therefore targets the variability of the location estimator
with fixed scale initialization, not a fully re-estimated robust
estimator. Bootstrap arguments are passed through `...` and extracted
via `.extractBootArgs()`:

- `R`:

  number of bootstrap replicates (default `999`)

- `type`:

  confidence interval type: `"perc"` or `"bca"` (default)

- `parallel`:

  parallelization mode: `"no"`, `"multicore"`, or `"snow"` (default
  `"no"`)

- `ncpus`:

  number of CPUs for parallel bootstrap (default
  `getOption("boot.ncpus", 1L)`)

The original internal estimator is accessible as `DescToolsX:::.huberM`.

## Note

Adapted from code by Martin Maechler to conform to package standards

## Random number generation

`method = "boot"` resamples and therefore advances R's global random
number generator. Call [`set.seed`](https://rdrr.io/r/base/Random.html)
beforehand for reproducible intervals.

## References

Huber, P. J. and Ronchetti, E. M. (2009). *Robust Statistics*, 2nd ed.
Wiley.

## See also

[`huber`](https://rdrr.io/pkg/MASS/man/huber.html),
[`mad`](https://rdrr.io/r/stats/mad.html),
[`tukeyBiweight`](tukeyBiweight.md)

Other location: [`gmean()`](gmean.md), [`hmean()`](hmean.md),
[`hodgesLehmann()`](hodgesLehmann.md), [`meanX()`](meanX.md),
[`medianX()`](medianX.md), [`modeX()`](modeX.md),
[`tukeyBiweight()`](tukeyBiweight.md)

## Examples

``` r
huberM(c(1:9, 1000))
#> [1] 5.553915
mad(c(1:9, 1000))
#> [1] 3.7065

set.seed(7)
x <- c(round(rnorm(1000), 1), round(rnorm(50, mean = 10, sd = 10)))

huberM(x, conf.level = 0.95)
#>         est         lci         uci 
#>  0.05600116 -0.01020176  0.12220408 
huberM(x, conf.level = 0.95, method = "boot", R = 499, type = "bca")
#>         est         lci         uci 
#>  0.05600116 -0.01018489  0.12185434 

# degenerate case: scale zero
huberM(rep(9, 100))
#> Warning: scale 's' is zero -- returning initial 'mu'
#> [1] 9
```
