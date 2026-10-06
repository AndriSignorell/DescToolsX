# (Weighted) Sample Quantiles

Compute sample quantiles, with optional weights.

## Usage

``` r
quantileX(
  x,
  probs = seq(0, 1, 0.25),
  weights = NULL,
  na.rm = FALSE,
  names = TRUE,
  type = 7,
  digits = 7
)
```

## Arguments

- x:

  a numeric vector

- probs:

  numeric vector of probabilities with values in \\\[0,1\]\\; `NA` is
  allowed

- weights:

  an optional numeric vector of non-negative, finite sample weights, of
  the same length as `x`. Missing weights are handled like missing
  values in `x`.

- na.rm:

  logical; if `TRUE`, observations with a missing value or a missing
  weight are removed before the computation

- names:

  logical; if true, the result has a
  [`names()`](https://rdrr.io/r/base/names.html) attribute. Set to
  `FALSE` for speedup with many `probs`.

- type:

  an integer selecting the quantile algorithm. Without weights one of
  the nine types of
  [`stats::quantile()`](https://rdrr.io/r/stats/quantile.html) (default
  7). With weights 2 (alias 5) or 7 (default); see Details.

- digits:

  used only when `names` is true: the precision to use when formatting
  the percentages.

## Value

a numeric vector of the same length as `probs`, named when
`names = TRUE`

## Details

Without `weights` the call is handed to
[`stats::quantile()`](https://rdrr.io/r/stats/quantile.html), so all
nine types are available and the results are identical to base R. The
one deliberate difference is the treatment of missing values, see below.

With `weights` two algorithms exist, and they interpret the weights
**differently**:

- `type = 2`:

  inverse of the weighted empirical distribution function, averaging at
  discontinuities - the weighted counterpart of
  [`stats::quantile()`](https://rdrr.io/r/stats/quantile.html) type 2.
  The weights are *relative*: only their ratios matter, multiplying
  every weight by a constant leaves the result unchanged, and equal
  weights reproduce the unweighted type 2. This is the Eurostat
  definition (EU-SILC 131-rev/04). `type = 5` is accepted as an alias,
  see the note on DescTools below.

- `type = 7`:

  treats the weights as *frequency* weights, i.e. as replication counts:
  with integer weights the result equals
  `quantile(rep(x, weights), type = 7)`. The effective sample size is
  `sum(weights)`, so the result is **not** scale-invariant, and weights
  normalized to sum to 1 are degenerate. `type = 7` therefore requires
  `sum(weights) >= 2` and raises an error otherwise.

Relative weights (survey or design weights) call for `type = 2`,
replication counts for `type = 7`.

**Missing values.** `NA` and `NaN` are treated alike, in `x` and in
`weights`; an observation is missing if its value or its weight is. With
`na.rm = FALSE`, a missing observation yields `NA` for every requested
probability, with and without weights - as
[`mean()`](https://rdrr.io/r/base/mean.html) does
([`stats::quantile()`](https://rdrr.io/r/stats/quantile.html) raises an
error instead). With `na.rm = TRUE` the missing observations are removed
first; if nothing is left, the result is `NA`. Invalid arguments (e.g.
`probs` outside \\\[0,1\]\\, negative or infinite weights) are an error
even when data are missing. An `NA` in `probs` yields `NA` at that
position only.

**Difference to DescTools.** `DescTools::Quantile()` labelled the
Eurostat algorithm `type = 5`. It is not R's type 5, which interpolates
linearly between order statistics: with equal weights the old weighted
`type = 5` and the unweighted `type = 5` gave different answers. Here
the algorithm carries its correct number 2; `5` still selects it, so
results of existing calls do not change.

## Note

The weighted algorithms are based on code by Andreas Alfons and Matthias
Templ (`laeken::weightedQuantile()`), adapted to conform to package
standards.

## References

Working group on Statistics on Income and Living Conditions (2004).
Common cross-sectional EU indicators based on EU-SILC; the gender pay
gap. *EU-SILC 131-rev/04*, Eurostat.

Hyndman, R. J., Fan, Y. (1996). Sample quantiles in statistical
packages. *The American Statistician*, 50(4), 361–365.
[doi:10.1080/00031305.1996.10473566](https://doi.org/10.1080/00031305.1996.10473566)

## See also

[`medianX()`](medianX.md), [`iqrX()`](iqrX.md),
[`stats::quantile()`](https://rdrr.io/r/stats/quantile.html),
[`lumen::quantileCI()`](https://andrisignorell.github.io/lumen/reference/quantileCI.html)

Other quantile: [`extremes`](extremes.md)

## Examples

``` r
# Pizza$temperature contains missing values: without na.rm the result is
# NA for every prob
quantileX(Pizza$temperature, weights = rep(1:3, length.out = nrow(Pizza)),
          na.rm = TRUE)
#>   0%  25%  50%  75% 100% 
#> 19.3 42.1 49.8 55.3 64.8 

x <- c(3.7, 3.3, 3.5, 2.8)

# type 2 only looks at the ratios of the weights ...
quantileX(x, weights = c(5, 5, 4, 1),      type = 2)
#>   0%  25%  50%  75% 100% 
#>  2.8  3.3  3.5  3.7  3.7 
quantileX(x, weights = c(5, 5, 4, 1) / 15, type = 2)   # identical
#>   0%  25%  50%  75% 100% 
#>  2.8  3.3  3.5  3.7  3.7 

# ... and equal weights give the unweighted type 2
quantileX(x, weights = rep(1, 4), type = 2)
#>   0%  25%  50%  75% 100% 
#> 2.80 3.05 3.40 3.60 3.70 
quantileX(x, type = 2)
#>   0%  25%  50%  75% 100% 
#> 2.80 3.05 3.40 3.60 3.70 

# type 7 reads the weights as replication counts
quantileX(x, weights = c(5, 5, 4, 1), type = 7)
#>   0%  25%  50%  75% 100% 
#>  2.8  3.3  3.5  3.7  3.7 
quantileX(rep(x, c(5, 5, 4, 1)), type = 7)              # identical
#>   0%  25%  50%  75% 100% 
#>  2.8  3.3  3.5  3.7  3.7 
```
