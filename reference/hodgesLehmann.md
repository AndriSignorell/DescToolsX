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
  method = c("exact", "boot"),
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
  the point estimate is returned. One-sided intervals require
  `conf.level > 0.5`.

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See
  [`ConfidenceIntervals()`](ConfidenceIntervals.md).

- method:

  interval method: `"exact"` (default), the distribution-free interval
  obtained by inverting the Wilcoxon signed-rank (one-sample) or
  rank-sum (two-sample) test; or `"boot"`, a bootstrap interval
  (one-sample only). Ignored without `conf.level`.

- na.rm:

  logical; if `TRUE`, missing values are removed from `x` and `y`
  separately before the computation

- ...:

  additional arguments for `method = "boot"` (`R`, `type`, `parallel`,
  `ncpus`); an error with `method = "exact"`. `type = "stud"` is not
  available: the bootstrap statistic carries no variance estimate per
  resample.

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
[`wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html) (which
however does not calculate correctly as soon as ties are present).

Note that in the two-sample case the estimator for the difference in
location parameters does not estimate the difference in medians (a
common misconception) but rather the median of the difference between a
sample from x and a sample from y.

`sides` names the side on which the finite bound lies: `"left"` yields
\\\[lci, \infty)\\, `"right"` yields \\(-\infty, uci\]\\. The estimator
is unbounded, so the open side is reported as \\\pm\infty\\.

**Missing values.** `NA` and `NaN` are treated alike. With
`na.rm = FALSE`, a missing value in `x` or `y` yields `NA` - a scalar,
or `c(est = NA, lci = NA, uci = NA)` when an interval was requested.
With `na.rm = TRUE` the missing values are removed from each sample
separately; `x` and `y` are independent samples, not pairs. An empty
sample, given or left over after the removal, yields `NA` as well.
Invalid arguments are an error even when data are missing.

**Infinite values** are ordinary order statistics. `NaN` is returned
when the median is undefined: if a pair yields `Inf - Inf` (`-Inf` and
`Inf` both in `x` in the one-sample case, the same infinity in `x` and
`y` in the two-sample case), or if the two middle order statistics are
`-Inf` and `Inf`.

**Exact interval.** With `method = "exact"` the interval inverts the
Wilcoxon signed-rank (one-sample) resp. rank-sum (two-sample) test
(Bauer 1972). Its bounds are Walsh averages resp. differences
`x[i] - y[j]`, selected by Monahan's partitioning instead of sorting all
\\O(n^2)\\ values, so large samples are no problem.

- Samples below 50 (both below 50 in the two-sample case) use the exact
  null distribution: the classic one without ties, the conditional
  (permutation) distribution given the midranks with ties.

- Larger samples use the normal approximation with continuity and tie
  correction.

The results agree with `wilcox.test(exact = TRUE, conf.int = TRUE)` (R
\>= 4.6.0) in the two-sample case and in the one-sample case without
ties. With ties,
[`wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html) (R 4.6.1)
takes the lower one-sample bound from \\P(V \> v)\\ instead of \\P(V \ge
v)\\, one rank too narrow, with coverage below the nominal level;
`hodgesLehmann()` uses \\P(V \ge v)\\. In the large-sample case
[`wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html) interpolates
between Walsh averages with
[`uniroot()`](https://rdrr.io/r/stats/uniroot.html), `hodgesLehmann()`
returns the order statistics that bound the inverted test. If the
requested level is not attainable (very small samples, e.g. \\n \le 5\\
at 95\\

**Bootstrap interval.** If all bootstrap replicates coincide (e.g.
constant data), the interval collapses to `lci = uci = est`. With a
single observation, or when the replicates are not all finite, no
interval can be formed: `lci` and `uci` are `NA` with a warning.

`x` and `y` are not modified.

## Note

C++ port of Monahan's algorithm by Cyril Flurin Moser

## Random number generation

Only `method = "boot"` uses R's random number generator; call
[`base::set.seed()`](https://rdrr.io/r/base/Random.html) beforehand for
reproducible intervals. The point estimate and the exact interval are
deterministic: the compiled routine picks its pivots from a local
generator and does not touch R's stream.

## References

Hodges, J. L., Lehmann, E. L. (1963). Estimates of location based on
rank tests. *The Annals of Mathematical Statistics*, 34(2), 598–611.
[doi:10.1214/aoms/1177704172](https://doi.org/10.1214/aoms/1177704172)

Monahan, J. F. (1984). Algorithm 616: Fast computation of the
Hodges-Lehmann location estimator. *ACM Transactions on Mathematical
Software*, 10(3), 265–270.
[doi:10.1145/1271.319414](https://doi.org/10.1145/1271.319414) .
Original code: <https://www4.stat.ncsu.edu/~monahan/jul10/>

Bauer, D. F. (1972). Constructing confidence sets using rank statistics.
*Journal of the American Statistical Association*, 67(339), 687–690.
[doi:10.1080/01621459.1972.10481279](https://doi.org/10.1080/01621459.1972.10481279)

Efron, B. (1987). Better bootstrap confidence intervals. *Journal of the
American Statistical Association*, 82(397), 171–185.
[doi:10.1080/01621459.1987.10478410](https://doi.org/10.1080/01621459.1987.10478410)

Davison, A. C., Hinkley, D. V. (1997). *Bootstrap Methods and Their
Application*. Cambridge University Press.

## See also

[`stats::wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html)

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

# missing values: NA, or removed per sample with na.rm
hodgesLehmann(c(x, NA), y)
#> [1] NA
hodgesLehmann(c(x, NA), y[-1], na.rm = TRUE)
#> [1] 0.54

# distribution-free interval, one- and two-sample
hodgesLehmann(x, conf.level = 0.95)
#>   est   lci   uci 
#> 1.725 1.165 2.370 
hodgesLehmann(x, y, conf.level = 0.95)
#>    est    lci    uci 
#>  0.560 -0.220  1.082 
hodgesLehmann(x, conf.level = 0.95, sides = "left")
#>   est   lci   uci 
#> 1.725 1.425   Inf 

# bootstrap interval (one-sample)
set.seed(1)
hodgesLehmann(x, conf.level = 0.95, method = "boot", R = 999)
#>      est      lci      uci 
#> 1.725000 1.425000 2.211442 
```
