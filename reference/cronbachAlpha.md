# Cronbach's Coefficient Alpha

Cronbach's alpha is a measure of internal consistency and often used for
validating psychometric tests. The unstandardized form implemented here
is computed from the item variances and the variance of the total score,
expressing the proportion of total-score variance not attributable to
item-specific variance. This reduces to Kuder-Richardson formula 20
(KR-20) when the columns of the data matrix are dichotomous.

## Usage

``` r
cronbachAlpha(
  x,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  returnConditional = FALSE,
  na.rm = FALSE
)
```

## Arguments

- x:

  a \\n \times m\\ matrix or data frame with item responses, \\n\\
  subjects (in rows) and \\m\\ items (in columns)

- conf.level:

  confidence level of the interval. If set to `NA` (the default), only
  the point estimate is returned.

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See
  [`ConfidenceIntervals`](ConfidenceIntervals.md).

- returnConditional:

  logical; if `TRUE`, alpha is additionally calculated for the dataset
  with each item left out

- na.rm:

  logical; if `TRUE`, incomplete cases are removed before the
  computation proceeds

## Value

a named numeric vector, or a list when `returnConditional = TRUE`.

If `na.rm = FALSE` and `x` contains missing values, the same structure
is returned with `NA_real_` throughout.

If `conf.level = NA`, the numeric vector contains only `est`; otherwise
it has elements:

- `est`:

  point estimate.

- `lci`:

  lower confidence interval bound.

- `uci`:

  upper confidence interval bound.

If `returnConditional = TRUE`, a list with the components:

- `unconditional`:

  alpha for the full set of items, as above

- `conditional`:

  a data frame with one row per item, giving the alpha that would be
  realized if that item were excluded. `NULL` when `x` has fewer than 3
  items, since dropping one would leave too few to compute alpha.

## Details

The confidence interval follows Feldt (1965) and is based on the \\F\\
distribution with \\n - 1\\ and \\(m - 1)(n - 1)\\ degrees of freedom,
where \\n\\ is the number of subjects (rows) and \\m\\ the number of
items (columns). It inherits the assumptions of the underlying ANOVA
derivation - in particular normally distributed scores and essentially
parallel items with homogeneous variances and covariances - and should
be read with more caution than the point estimate when these are
doubtful, for instance with markedly skewed or heterogeneous items.

`sides` names the side on which the finite bound lies: `"left"` yields
an interval bounded below and `"right"` one bounded above. Alpha cannot
exceed 1, so the open upper side is reported at that boundary rather
than as \\\infty\\ (design_rules.md 4.1), while the open lower side
stays \\-\infty\\ because alpha is unbounded below.

Missing values are handled according to package conventions: if
`na.rm = FALSE` and `x` contains missing values, the usual structure is
returned with `NA_real_` in place of every estimate. If `na.rm = TRUE`,
complete cases are used. Infinite values leave the variances undefined
and are rejected with an error.

## Note

Based on code of Harold C. Doran, adapted to conform to package
standards.

## References

Cronbach, L. J. (1951). Coefficient alpha and the internal structure of
tests. *Psychometrika*, *16*(3), 297-334.
[doi:10.1007/BF02310555](https://doi.org/10.1007/BF02310555)

Feldt, L. S. (1965). The approximate sampling distribution of
Kuder-Richardson reliability coefficient twenty. *Psychometrika*,
*30*(3), 357-370.
[doi:10.1007/BF02289499](https://doi.org/10.1007/BF02289499)

## See also

Other assoc.agreement: [`ccc()`](ccc.md),
[`cohenKappa()`](cohenKappa.md), [`icc()`](icc.md),
[`kappaM()`](kappaM.md), [`krippAlpha()`](krippAlpha.md),
[`pabak()`](pabak.md), [`percAgreement()`](percAgreement.md),
[`randolphKappa()`](randolphKappa.md)

## Examples

``` r
set.seed(1234)
tmp <- data.frame(
  item1 = sample(c(0, 1), 20, replace = TRUE),
  item2 = sample(c(0, 1), 20, replace = TRUE),
  item3 = sample(c(0, 1), 20, replace = TRUE),
  item4 = sample(c(0, 1), 20, replace = TRUE),
  item5 = sample(c(0, 1), 20, replace = TRUE)
)

cronbachAlpha(tmp[, 1:4])
#>       est 
#> 0.4029851 

cronbachAlpha(tmp[, 1:4], conf.level = 0.95)
#>        est        lci        uci 
#>  0.4029851 -0.1799176  0.7377321 

# the conditional table is labelled with the column names of x
cronbachAlpha(tmp[, 1:4], returnConditional = TRUE, conf.level = 0.95)
#> $unconditional
#>        est        lci        uci 
#>  0.4029851 -0.1799176  0.7377321 
#> 
#> $conditional
#>    item       est          lci       uci
#> 1 item1 0.1574074 -0.774604418 0.6403307
#> 2 item2 0.5241379 -0.002224489 0.7968734
#> 3 item3 0.2537764 -0.571639304 0.6814668
#> 4 item4 0.3455056 -0.378446277 0.7206224
#> 

# fewer than 3 items: the conditional component is NULL
cronbachAlpha(tmp[, 1:2], returnConditional = TRUE, conf.level = 0.95)
#> $unconditional
#>         est         lci         uci 
#> -0.04678363 -1.64464747  0.58567031 
#> 
#> $conditional
#> NULL
#> 
```
