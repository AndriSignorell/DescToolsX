# Ordinal Association Measures

Computes concordance-based association measures for two ordinal
variables or a contingency table, optionally with confidence intervals.

## Usage

``` r
ordAssocs(
  x,
  y = NULL,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  which = c("all", "gamma", "tauA", "tauB", "tauC", "somers", "cstat"),
  direction = c("row", "column")
)

gkGamma(
  x,
  y = NULL,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  direction = c("row", "column")
)

kendallTauA(
  x,
  y = NULL,
  conf.level = NA,
  sides = c("two.sided", "left", "right")
)

kendallTauB(
  x,
  y = NULL,
  conf.level = NA,
  sides = c("two.sided", "left", "right")
)

stuartTauC(
  x,
  y = NULL,
  conf.level = NA,
  sides = c("two.sided", "left", "right")
)

somersDelta(
  x,
  y = NULL,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  direction = c("row", "column")
)
```

## Arguments

- x:

  numeric or ordinal vector, or a two-dimensional contingency table

- y:

  optional second vector of the same length as `x`

- conf.level:

  confidence level of the interval; `NA` returns only the estimate

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See details
  in [`ConfidenceIntervals`](ConfidenceIntervals.md). The open side is
  closed at the boundary of the parameter's range - \\\pm 1\\ for gamma,
  the tau family and Somers' \\D\\, and \\\[0, 1\]\\ for the c
  statistic - rather than at an infinity none of them can reach.

- which:

  measure returned by `ordAssocs()`: `"all"`, `"gamma"`, `"tauA"`,
  `"tauB"`, `"tauC"`, `"somers"`, or `"cstat"`

- direction:

  direction of Somers' \\D\\; `"row"` or `"column"`

## Value

The extractor functions return am unnamed numeric scalar if
`conf.level = NA`, and otherwise a named numeric vector with elements:

- `est`:

  point estimate

- `lci`:

  lower confidence interval bound

- `uci`:

  upper confidence interval bound

`ordAssocs()` returns the same structure in a named list containing the
selected measures.

## Details

Let \\P\\ and \\Q\\ denote the numbers of concordant and discordant
pairs, \\T_X\\ and \\T_Y\\ the numbers tied on \\X\\ and \\Y\\,
\\n_0=n(n-1)/2\\, and \\m\\ the smaller table dimension.
[`conDisPairs`](conDisPairs.md) returns these pair counts as `C`, `D`,
`Ties_X`, and `Ties_Y` and describes their calculation.

The measures are defined as follows:

- Goodman–Kruskal \\\gamma\\ (`gkGamma`)::

  \$\$\gamma = \frac{P-Q}{P+Q}\$\$

- Kendall \\\tau_a\\ (`kendallTauA`)::

  \$\$\tau_a = \frac{P-Q}{n_0}\$\$

- Kendall \\\tau_b\\ (`kendallTauB`)::

  \$\$ \tau_b = \frac{P-Q} {\sqrt{(n_0-T_X)(n_0-T_Y)}} \$\$

- Stuart \\\tau_c\\ (`stuartTauC`)::

  \$\$ \tau_c = \frac{2m(P-Q)} {n^2(m-1)} \$\$

- Somers \\D\_{X\mid Y}\\ (`somersDelta`)::

  \$\$ D\_{X\mid Y} = \frac{P-Q} {n_0-T_Y} \$\$

Gamma and Kendall's coefficients are symmetric.

Somers' \\D\\ is directional: for tables, `direction="row"` returns
\\D\_{R\|C}\\ and `direction="column"` returns \\D\_{C\|R}\\. In vector
mode, `ordAssocs()` returns \\D\_{X\|Y}\\; reverse `x` and `y` for the
other direction. `somersDelta()` performs this reversal when
`direction="column"`.

The c-statistic is used for a binary outcome and consistently ordered
predictions and is related to Somers D as \\C\_{stat}=(D+1)/2\\. See
[`cStat`](cStat.md) for direct estimation of the c-statistic from
predicted values and a binary response.

## References

Agresti, A. (2002) *Categorical Data Analysis*. Wiley, pp. 57–59.

Brown, M. B. and Benedetti, J. K. (1977). Sampling behavior of tests for
correlation in two-way contingency tables. *JASA*, 72, 309–315.

Goodman, L. A. and Kruskal, W. H. (1954, 1963). Measures of association
for cross classifications. *JASA*, 49, 732–764; 58, 310–364.

Kendall, M. (1955) *Rank Correlation Methods*. Charles Griffin.

Somers, R. H. (1962). A new asymmetric measure of association for
ordinal variables. *American Sociological Review*, 27, 799–811.

## See also

Other assoc.ordinal: [`cStat()`](cStat.md),
[`conDisPairs()`](conDisPairs.md), [`kendallW()`](kendallW.md)

## Examples

``` r
# Table example:
tab <- as.table(rbind(
  c(26, 26, 23, 18,  9),
  c( 6,  7,  9, 14, 23)
))

ordAssocs(tab, conf.level = 0.95)
#> $gamma
#>       est       lci       uci 
#> 0.5313123 0.3479918 0.7146328 
#> 
#> $tauA
#>       est       lci       uci 
#> 0.2068323 0.1281336 0.2855310 
#> 
#> $tauB
#>       est       lci       uci 
#> 0.3372567 0.2114030 0.4631105 
#> 
#> $tauC
#>       est       lci       uci 
#> 0.4110953 0.2546754 0.5675151 
#> 
#> $somers
#>       est       lci       uci 
#> 0.2569444 0.1591986 0.3546903 
#> 
#> $cstat
#>       est       lci       uci 
#> 0.6284722 0.5795993 0.6773451 
#> 
kendallTauB(tab, conf.level = 0.95)
#>       est       lci       uci 
#> 0.3372567 0.2114030 0.4631105 
somersDelta(tab, direction = "column")
#> [1] 0.442672

# Vector example
x <- c(1,2,2,3,3,3,4,5)
y <- c(1,3,2,1,5,3,4,5)

kendallTauA(x, y, conf.level=0.95)
#>       est       lci       uci 
#> 0.5357143 0.0648461 1.0000000 
somersDelta(x, y, direction = "column", conf.level=0.95)
#>       est       lci       uci 
#> 0.6250000 0.2776229 0.9723771 

```
