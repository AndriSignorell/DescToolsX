# (Weighted) Sample Quantiles

Compute sample quantiles, with optional weights.

## Usage

``` r
quantileX(
  x,
  weights = NULL,
  probs = seq(0, 1, 0.25),
  na.rm = FALSE,
  names = TRUE,
  type = 7,
  digits = 7
)
```

## Arguments

- x:

  a numeric vector

- weights:

  an optional numeric vector giving the sample weights

- probs:

  numeric vector of probabilities with values in \\\[0,1\]\\

- na.rm:

  a logical indicating whether missing values in `x` should be omitted

- names:

  logical; if true, the result has a
  [`names`](https://rdrr.io/r/base/names.html) attribute. Set to `FALSE`
  for speedup with many `probs`.

- type:

  an integer between 1 and 9 selecting one of the nine quantile
  algorithms of [`quantile`](https://rdrr.io/r/stats/quantile.html). All
  nine are available for unweighted data. With `weights` only 5 and 7
  (default) exist; any other value is an error. See Details for how the
  two differ in their reading of the weights.

- digits:

  used only when `names` is true: the precision to use when formatting
  the percentages. In `R` versions up to 4.0.x, this had been set to
  `max(2, getOption("digits"))`, internally.

## Value

a numeric vector containing the weighted quantiles of `x` at
probabilities `probs`, named when `names = TRUE`

## Details

Without `weights` the call is handed to
[`quantile`](https://rdrr.io/r/stats/quantile.html) unchanged, so all
nine types are available and the results are identical to base R.

With `weights` only types 5 and 7 exist, and they interpret the weights
**differently**:

- `type = 5`:

  treats them as relative weights: only the ratios matter, and
  multiplying every weight by a constant leaves the result unchanged.
  This follows the Eurostat definition (EU-SILC 131-rev/04).

- `type = 7`:

  treats them as *frequency* weights, i.e. as replication counts. The
  effective sample size is `sum(weights)`, so the result is **not**
  scale-invariant, and weights that have been normalized to sum to 1 are
  degenerate - see below.

This difference is inherited from the two source implementations and is
not a free choice of the caller: it is worth knowing which of the two is
meant before picking a type. Because `type = 7` needs `sum(weights)` to
act as a sample size, it requires that sum to be at least 2 and raises
an error otherwise.

## Note

Based on code by Andreas Alfons, Matthias Templ, adapted to conform to
package standards.

## References

Working group on Statistics on Income and Living Conditions (2004)
Common cross-sectional EU indicators based on EU-SILC; the gender pay
gap. *EU-SILC 131-rev/04*, Eurostat.

## See also

[`medianX`](medianX.md),
[`quantile`](https://rdrr.io/r/stats/quantile.html),
[`quantileCI`](https://andrisignorell.github.io/lumen/reference/quantileCI.html)

Other quantile: [`extremes`](extremes.md)

## Examples

``` r
# Pizza$temperature contains missing values, so na.rm is needed - without
# it the function returns NA for every prob, silently.
quantileX(Pizza$temperature, rep(c(1:3), length.out = nrow(Pizza)),
          na.rm = TRUE)
#>   0%  25%  50%  75% 100% 
#> 19.3 42.1 49.8 55.3 64.8 

x <- c(3.7, 3.3, 3.5, 2.8)

# type 5 only looks at the ratios of the weights ...
quantileX(x, weights = c(5, 5, 4, 1),      type = 5)
#>   0%  25%  50%  75% 100% 
#>  2.8  3.3  3.5  3.7  3.7 
quantileX(x, weights = c(5, 5, 4, 1) / 15, type = 5)   # identical
#>   0%  25%  50%  75% 100% 
#>  2.8  3.3  3.5  3.7  3.7 

# ... while type 7 reads them as replication counts, so they have to be
# on that scale
quantileX(x, weights = c(5, 5, 4, 1), type = 7)
#>   0%  25%  50%  75% 100% 
#>  2.8  3.3  3.5  3.7  3.7 
```
