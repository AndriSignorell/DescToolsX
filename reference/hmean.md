# Harmonic Mean and Its Confidence Interval

Calculates the harmonic mean and its confidence interval of a vector x.

## Usage

``` r
hmean(
  x,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  method = c("classic", "boot"),
  na.rm = FALSE,
  ...
)
```

## Arguments

- x:

  a positive numeric vector. An object which is not a vector is coerced,
  if possible, by [`as.vector()`](https://rdrr.io/r/base/vector.html).

- conf.level:

  confidence level of the interval. If set to `NA` (the default), only
  the point estimate is returned.

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See
  [`ConfidenceIntervals`](ConfidenceIntervals.md).

- method:

  a vector of character strings representing the type of intervals
  required. The value should be any subset of the values `"classic"`,
  `"boot"`. See [`boot.ci`](https://rdrr.io/pkg/boot/man/boot.ci.html).

- na.rm:

  logical, indicating whether `NA` values should be stripped before the
  computation proceeds. Defaults to `FALSE`.

- ...:

  further arguments are passed to the
  [`boot`](https://rdrr.io/pkg/boot/man/boot.html) function. Supported
  arguments are `type` (`"norm"`, `"basic"`, `"stud"`, `"perc"`,
  `"bca"`), `parallel` and the number of bootstrap replicates `R`. If
  not defined those will be set to their defaults, being `"basic"` for
  `type`, option `"boot.parallel"` (and if that is not set, `"no"`) for
  `parallel` and `999` for `R`.

## Value

if `conf.level = NA`, a numeric scalar. Otherwise a named numeric vector
with elements:

- `est`:

  point estimate of the harmonic mean

- `lci`:

  lower confidence interval bound

- `uci`:

  upper confidence interval bound

## Details

To compute the harmonic mean, `1/x` is first calculated, before the
arithmetic mean and its confidence interval are computed by
[`meanCI`](https://andrisignorell.github.io/lumen/reference/meanCI.html).
The harmonic mean is then the reciprocal of the arithmetic mean of the
reciprocals of the values. The same applies to the confidence interval.

The harmonic mean is restricted to strictly positive inputs.
Non-positive values are turned into `NA` and therefore make the result
`NA` unless `na.rm = TRUE`, in which case they are dropped. If the lower
bound of the confidence interval is not greater than zero, then the
confidence interval is not defined, and thus `NA` will be reported.

`sides` names the side on which the finite bound lies: `"left"` yields
an interval bounded below, `"right"` one bounded above. The harmonic
mean of positive values is itself positive, so the open lower side is
reported as 0 rather than as `NA` or \\-\infty\\.

Use [`sapply`](https://rdrr.io/r/base/lapply.html) to calculate the
measures from data frame, resp. from a matrix.  

## References

Snedecor, G. W., Cochran, W. G. (1989) Statistical Methods, 8th ed.
Ames, *IA: Iowa State University Press*

## See also

Other location: [`gmean()`](gmean.md),
[`hodgesLehmann()`](hodgesLehmann.md), [`huberM()`](huberM.md),
[`meanX()`](meanX.md), [`medianX()`](medianX.md), [`modeX()`](modeX.md),
[`tukeyBiweight()`](tukeyBiweight.md)

## Examples

``` r

x <- runif(5)
hmean(x)
#> [1] 0.2616124

m <- matrix(runif(50), nrow = 10)
apply(m, 2, hmean)
#> [1] 0.3173872 0.5184113 0.2761854 0.2949970 0.3583152

sapply(as.data.frame(m), hmean)
#>        V1        V2        V3        V4        V5 
#> 0.3173872 0.5184113 0.2761854 0.2949970 0.3583152 
```
