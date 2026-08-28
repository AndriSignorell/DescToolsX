# Cohen's h for a 2x2 Table

Computes Cohen's \\h\\, a standardized effect size for the difference
between two proportions in a 2x2 contingency table.

## Usage

``` r
cohenH(
  x,
  y = NULL,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  ...
)
```

## Arguments

- x:

  a 2x2 contingency table or matrix, or a categorical vector when `y` is
  supplied

- y:

  an optional second variable used together with `x` to create a
  contingency table via `table(x, y, ...)`

- conf.level:

  confidence level of the interval. If set to `NA` (the default), only
  the point estimate is returned.

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See
  [`ConfidenceIntervals`](ConfidenceIntervals.md).

- ...:

  additional arguments passed to
  [`table()`](https://rdrr.io/r/base/table.html)

## Value

if `conf.level = NA`, a numeric scalar containing Cohen's \\h\\;
otherwise a named numeric vector with elements:

- `est`:

  point estimate of Cohen's \\h\\.

- `lci`:

  lower confidence interval bound.

- `uci`:

  upper confidence interval bound.

## Details

Cohen's \\h\\ is defined as:

\$\$ h = 2\arcsin(\sqrt{p_1}) - 2\arcsin(\sqrt{p_2}) \$\$

where \\p_1\\ and \\p_2\\ are the event probabilities in the first and
second row, respectively.

Optionally, an approximate asymptotic confidence interval is computed.

Cohen's \\h\\ is a variance-stabilized standardized effect size for
comparing two proportions.

Approximate interpretation thresholds suggested by Cohen are:

|               |                   |
|---------------|-------------------|
| \|h\| \< 0.2  | negligible effect |
| \|h\| \>= 0.2 | small effect      |
| \|h\| \>= 0.5 | medium effect     |
| \|h\| \>= 0.8 | large effect      |

The confidence interval is based on the asymptotic standard error:

\$\$ SE(h) = \sqrt{\frac{1}{n_1} + \frac{1}{n_2}} \$\$

`sides` names the side on which the finite bound lies: `"left"` yields
\\\[lci, \infty)\\, `"right"` yields \\(-\infty, uci\]\\.

## References

Cohen J (1988). Statistical Power Analysis for the Behavioral Sciences
(2nd ed.). Lawrence Erlbaum Associates.

## See also

Other effect.size: [`cohenD()`](cohenD.md), [`etaSq()`](etaSq.md),
[`glassDelta()`](glassDelta.md), [`oddsRatio()`](oddsRatio.md),
[`relRisk()`](relRisk.md)

## Examples

``` r
tab <- matrix(
  c(26, 26,
    6, 7),
  nrow = 2,
  byrow = TRUE
)

cohenH(tab)
#> [1] 0.07699914
cohenH(tab, conf.level = 0.95)
#>         est         lci         uci 
#>  0.07699914 -0.53075989  0.68475817 

x <- c(rep("A", 52), rep("B", 13))
y <- c(rep(c("yes", "no"), c(26, 26)),
       rep(c("yes", "no"), c(6, 7)))

cohenH(x, y, conf.level = 0.95)
#>         est         lci         uci 
#> -0.07699914 -0.68475817  0.53075989 
```
