# Pearson's Contingency Coefficient

Computes Pearson's contingency coefficient for a contingency table. If
`x` and `y` are supplied, the table is constructed first; see
[`Association`](Association.md).

## Usage

``` r
contCoef(
  x,
  y = NULL,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  correct = FALSE,
  ...
)
```

## Arguments

- x:

  either a contingency table, a two-column object (matrix, data.frame or
  list), or a vector of observations (together with `y`)

- y:

  optional second vector. If `x` is not a vector, `y` must be `NULL`.

- conf.level:

  confidence level of the interval. If set to `NA` (the default), only
  the point estimate is returned.

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See
  [`ConfidenceIntervals`](ConfidenceIntervals.md).

- correct:

  logical; whether Sakoda's adjusted Pearson's C should be returned;
  defaults to `FALSE`

- ...:

  further arguments. Named arguments known to
  [`normalizeToConfusion`](normalizeToConfusion.md) or
  [`table`](https://rdrr.io/r/base/table.html) are used to build the
  table; `R` and `type` configure the bootstrap and are described under
  Details. Anything else is an error rather than a silent no-op.

## Value

if `conf.level = NA`, a numeric scalar containing Pearson's contingency
coefficient; otherwise a named numeric vector with elements:

- `est`:

  point estimate of the contingency coefficient

- `lci`:

  lower confidence interval bound

- `uci`:

  upper confidence interval bound

## Details

Pearson's contingency coefficient ranges from 0 to \\\sqrt{(m-1)/m}\\,
where \\m = \min(r, c)\\. Consequently, its attainable maximum depends
on the dimensions of the table.

Sakoda's correction divides the coefficient by this maximum:
\$\$C\_{\mathrm{S}} = \frac{C}{C\_{\max}} = C\sqrt{\frac{m}{m-1}}.\$\$
The corrected coefficient therefore ranges from 0 to 1.

Since no generally accepted analytical interval is available, only
bootstrap intervals are implemented. The interval is obtained from a
multinomial bootstrap over the cells of the table. Two arguments
configure it, both passed through `...`: `R`, the number of replicates
(default 999), and `type`, one of `"perc"` (default) or `"bca"`.

`"perc"` is the default deliberately. Under independence the parameter
sits *on* the boundary of its range, where the sampling distribution of
\\C\\ is not normal under any monotone transformation - which is
precisely what BCa assumes. Both of its ingredients degrade there: the
bias correction is read off the share of replicates below the estimate,
which collapses when the estimate is at the edge of the bootstrap
distribution, and the acceleration is a jackknife, which is not
consistent for a functional that is not smooth. `"bca"` is the better
choice for tables with a clearly non-zero association and a reasonable
number of observations.

Confidence intervals are restricted to the attainable range. Measures
such as [cramerV](cramerV.md) may be preferable when inference is
central.

For further information see
[`ConfidenceIntervals`](ConfidenceIntervals.md).

## References

Sakoda, J.M. (1977) Measures of Association for Multivariate Contingency
Tables, *Proceedings of the Social Statistics Section of the American
Statistical Association* (Part III), 777-780.

Efron, B., Tibshirani, R.J. (1993) *An Introduction to the Bootstrap*,
Chapman & Hall, chapter 14.

## See also

[bedrock::pairApply](https://andrisignorell.github.io/bedrock/reference/pairApply.html)

Other assoc.nominal: [`cramerV()`](cramerV.md), [`gkTau()`](gkTau.md),
[`lambda()`](lambda.md), [`mutInf()`](mutInf.md), [`phi()`](phi.md),
[`tschuprowT()`](tschuprowT.md), [`uncertCoef()`](uncertCoef.md),
[`yule`](yule.md)

## Examples

``` r

tab <- apply(HairEyeColor, c(1, 2), sum)
contCoef(tab)
#> [1] 0.4351585

# just x and y
with(bedrock::untable(tab), contCoef(Hair, Eye))
#> [1] 0.4351585

set.seed(1)
contCoef(tab, conf.level = 0.95)
#>       est       lci       uci 
#> 0.4351585 0.3870546 0.4905756 

set.seed(1)
contCoef(tab, conf.level = 0.95, type = "bca", R = 999)
#>       est       lci       uci 
#> 0.4351585 0.3739549 0.4754878 

```
