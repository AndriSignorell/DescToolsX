# Goodman Kruskal's Tau

Calculate Goodman-Kruskal's tau, a measure of association for nominal
variables in a two-way table. The function accepts either a contingency
table or two vectors.

## Usage

``` r
gkTau(
  x,
  y = NULL,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  direction = c("row", "column"),
  ...
)
```

## Arguments

- x:

  numeric vector or contingency table. A matrix is treated as a table.

- y:

  `NULL` (default) or a vector with compatible dimensions to `x`. If
  supplied, `table(x, y, ...)` is calculated.

- conf.level:

  confidence level of the interval. If set to `NA` (the default), only
  the point estimate is returned.

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See
  [`ConfidenceIntervals`](ConfidenceIntervals.md).

- direction:

  direction of the calculation. Must be `"row"` (default) or `"column"`.
  `"row"` gives tau (R\|C), i.e. the row variable is the dependent one
  and is predicted from the column variable; `"column"` gives tau
  (C\|R).

- ...:

  further arguments are passed to the function
  [`table`](https://rdrr.io/r/base/table.html), allowing i.e. to set
  useNA. This refers only to the vector interface; supplying them
  without `y` is an error.

## Value

if `conf.level = NA`, a numeric scalar. Otherwise a named numeric vector
with elements:

- `est`:

  point estimate of Goodman-Kruskal's tau

- `lci`:

  lower confidence interval bound

- `uci`:

  upper confidence interval bound

## Details

Goodman-Kruskal tau measures association for cross tabulations of
nominal level variables. Goodman-Kruskal tau is based on random category
assignment. It measures the percentage improvement in predictability of
the dependent variable (column or row variable) given the value of other
variables (row or column variables). Goodman-Kruskal tau is the same as
Goodman-Kruskal lambda except the calculations of the tau statistic are
based on assignment probabilities specified by marginal or conditional
proportions. Misclassification probabilities are based on random
category assignment with probabilities specified by marginal or
conditional proportion.

Goodman Kruskal tau reduces to \\\phi^2\\ (see: [`phi`](phi.md)) in the
2x2-table case.  

The measure lies in \\\[0, 1\]\\ by construction. Both ends are reached
by cancellation, so an estimate within a few machine epsilons of a bound
is reported as that bound. Tau is undefined when the dependent variable
has fewer than two non-empty categories (the denominator is then zero),
which is signalled with an error.

The confidence interval uses the asymptotic standard error of Liebetrau
(1983). That variance vanishes at both ends of the range: under exact
independence (\\\tau = 0\\, where the limiting distribution is a
weighted sum of chi-square variables rather than normal) and under
perfect prediction (\\\tau = 1\\). Where the estimated standard error is
zero the interval would collapse to a single point and thus exclude
every other value, which no sample supports; the bounds are returned as
`NA` with a warning instead. Close to either end the normal
approximation is poor and the interval is too narrow.

## Note

Based on code by Antti Arppe, adapted to conform to package standards.

## References

Agresti, A. (2002) *Categorical Data Analysis*. John Wiley & Sons, pp.
57-59.

Goodman, L. A., & Kruskal, W. H. (1954) Measures of association for
cross classifications. *Journal of the American Statistical
Association*, 49, 732-764.

Goodman, L. A., & Kruskal, W. H. (1963) Measures of association for
cross classifications III: Approximate sampling theory. *Journal of the
American Statistical Association*, 58, 310-364.

Liebetrau, A. M. (1983) *Measures of Association*, Sage University
Papers Series on Quantitative Applications in the Social Sciences,
07-004. Newbury Park, CA: Sage, pp. 24–30

## See also

[`lambda`](lambda.md), [`cramerV`](cramerV.md),
[`Association`](Association.md)

Other assoc.nominal: [`contCoef()`](contCoef.md),
[`cramerV()`](cramerV.md), [`lambda()`](lambda.md),
[`mutInf()`](mutInf.md), [`phi()`](phi.md),
[`tschuprowT()`](tschuprowT.md), [`uncertCoef()`](uncertCoef.md),
[`yule`](yule.md)

## Examples

``` r
# example in:
# http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf
# pp. S. 1821

tab <- as.table(rbind(c(26,26,23,18,9),c(6,7,9,14,23)))

# Goodman Kruskal's tau C|R
gkTau(tab, direction="column", conf.level=0.95)
#>         est         lci         uci 
#> 0.041216580 0.009920576 0.072512583 
# Goodman Kruskal's tau R|C
gkTau(tab, direction="row", conf.level=0.95)
#>        est        lci        uci 
#> 0.16523315 0.04921484 0.28125146 

# http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf
# pp. 1814 (143)
tab <- as.table(cbind(c(11,2),c(4,6)))

gkTau(tab, direction="row", conf.level=0.95)
#>       est       lci       uci 
#> 0.2156410 0.0000000 0.5537724 
gkTau(tab, direction="column", conf.level=0.95)
#>       est       lci       uci 
#> 0.2156410 0.0000000 0.5537724 
# reduce both to:
phi(tab)^2
#> [1] 0.215641


# example 1 in Liebetrau (1983)

tt <- matrix(c(549,93,233,119,225,455,402,  
               212,124,78,42,41,12,132,
               54,54,33,13,46,7,153), ncol=3,
             dimnames=list(rownames=c("Gov", "Mil", "Edu", "Eco", "Intel", "Rel", "For"), 
                           colnames=c("One", "Two", "Multi")))

gkTau(tt, direction = "row", conf.level = 0.95)
#>        est        lci        uci 
#> 0.02580507 0.02094902 0.03066113 
gkTau(tt, direction = "column", conf.level = 0.95)
#>        est        lci        uci 
#> 0.08601920 0.07243943 0.09959897 


# SPSS
ttt <- matrix(c(225,53,206,3,1,12), nrow=3,
              dimnames=list(rownames=c("right","center", "left"), 
                            colnames=c("us","ussr")))

round(gkTau(ttt, direction = "row", conf.level = 0.95), digits = 3)
#>   est   lci   uci 
#> 0.010 0.000 0.023 
round(gkTau(ttt, direction = "column"), digits = 3)
#> [1] 0.013
```
