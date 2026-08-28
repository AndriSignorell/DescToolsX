# Prevalence-Adjusted and Bias-Adjusted Kappa (PABAK)

Computes the prevalence-adjusted and bias-adjusted kappa (PABAK) as a
measure of inter-rater agreement, together with an asymptotic confidence
interval and the auxiliary prevalence and bias indices.

## Usage

``` r
pabak(
  x,
  y = NULL,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  ...
)
```

## Arguments

- x:

  a square confusion matrix (or data frame), or a categorical vector
  when `y` is provided.

- y:

  `NULL` (default) or a categorical vector of the same length as `x`.
  When supplied, `table(x, y, ...)` is computed internally.

- conf.level:

  confidence level of the interval. If set to `NA` (the default), only
  the point estimate is returned.

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See
  [`ConfidenceIntervals`](ConfidenceIntervals.md).

- ...:

  further arguments passed to
  [`table`](https://rdrr.io/r/base/table.html) for the vector interface,
  for example `useNA`.

## Value

a named numeric vector.

If `conf.level = NA`, a single element `est`; otherwise the elements:

- `est`:

  point estimate of PABAK.

- `lci`:

  lower confidence interval bound.

- `uci`:

  upper confidence interval bound.

In both cases the diagnostics are attached as attributes:

- nObs:

  number of observations in the table.

- prevalenceIndex:

  prevalence index, or `NA` for \\k \> 2\\.

- biasIndex:

  bias index, or `NA` for \\k \> 2\\.

## Details

Cohen's kappa can be paradoxically low when the prevalence of one
category differs markedly from 0.5 (prevalence effect) or when the two
raters systematically disagree on the marginal frequency of a category
(bias effect). PABAK corrects for both distortions by replacing the
chance agreement term with the fixed value \\p_e = 0.5\\. The statistic
simplifies to \$\$\mathrm{PABAK} = 2 p_o - 1\$\$ where \\p_o\\ is the
observed proportion of agreement (diagonal sum of the relative-frequency
table).

The asymptotic standard error is derived from the delta method applied
to the above identity: \$\$\mathrm{SE} = 2 \sqrt{p_o (1 - p_o) / n}\$\$

Two diagnostic indices quantify the prevalence and bias effects that can
cause Cohen's kappa to differ from PABAK. They are defined only for \\2
\times 2\\ tables following Byrt et al. (1993); for larger tables they
are `NA`:

- Prevalence index:

  \\\|p\_{11} - p\_{22}\|\\. A high value indicates that one outcome
  dominates, which deflates Cohen's kappa relative to PABAK.

- Bias index:

  \\\|p\_{1\cdot} - p\_{\cdot 1}\|\\. A high value signals that the two
  raters assign the positive label at systematically different rates.

When both indices are near zero, PABAK and Cohen's kappa will be nearly
identical. Both are returned as attributes rather than as elements of
the result, since they are separate diagnostics rather than further
views of the estimate, and they do not depend on `conf.level`.

The Wald confidence interval is truncated to the admissible range
\\\[-1, 1\]\\.

`sides` names the side on which the finite bound lies: `"left"` yields
\\\[lci, \infty)\\ and `"right"` \\(-\infty, uci\]\\.

Data can be passed either as a square confusion matrix (or data frame)
in `x`, or as two vectors `x` and `y`, in which case `table(x, y, ...)`
is computed internally.

Missing values are handled as
[`table`](https://rdrr.io/r/base/table.html) does - excluded by default.
Pass `useNA = "ifany"` via `...` to include them.

## References

Byrt, T., Bishop, J., & Carlin, J. B. (1993). Bias, prevalence and
kappa. *Journal of Clinical Epidemiology*, *46*(5), 423-429.
[doi:10.1016/0895-4356(93)90018-V](https://doi.org/10.1016/0895-4356%2893%2990018-V)

Hoehler, F. K. (2000). Bias and prevalence effects on kappa viewed in
terms of sensitivity and specificity. *Journal of Clinical
Epidemiology*, *53*(5), 499-503.
[doi:10.1016/S0895-4356(99)00174-2](https://doi.org/10.1016/S0895-4356%2899%2900174-2)

## See also

Other assoc.agreement: [`ccc()`](ccc.md),
[`cohenKappa()`](cohenKappa.md), [`cronbachAlpha()`](cronbachAlpha.md),
[`icc()`](icc.md), [`kappaM()`](kappaM.md),
[`krippAlpha()`](krippAlpha.md), [`percAgreement()`](percAgreement.md),
[`randolphKappa()`](randolphKappa.md)

## Examples

``` r
# 2x2 table: two raters classifying 100 patients as positive / negative
m2 <- matrix(c(45, 15, 5, 35), nrow = 2, byrow = TRUE,
             dimnames = list(rater1 = c("pos", "neg"),
                             rater2 = c("pos", "neg")))

pabak(m2)
#> est 
#> 0.6 
#> attr(,"nObs")
#> [1] 100
#> attr(,"prevalenceIndex")
#> [1] 0.1
#> attr(,"biasIndex")
#> [1] 0.1
pabak(m2, conf.level = 0.95)
#>       est       lci       uci 
#> 0.6000000 0.4432029 0.7567971 
#> attr(,"nObs")
#> [1] 100
#> attr(,"prevalenceIndex")
#> [1] 0.1
#> attr(,"biasIndex")
#> [1] 0.1

# the diagnostics are available either way
attr(pabak(m2), "prevalenceIndex")
#> [1] 0.1
attr(pabak(m2), "biasIndex")
#> [1] 0.1

# Compare with cohenKappa: the indices show where the gap comes from
cohenKappa(m2)
#> [1] 0.6

# 4x4 table: the indices are NA for k > 2
m4 <- matrix(c(400, 40, 20, 10,
                50,300, 60, 20,
                10, 40,120,  5,
                 5, 90, 50, 80),
             nrow = 4, byrow = TRUE)

pabak(m4, conf.level = 0.95)
#> Warning: Matrix has no dimnames; consider supplying 'levels=' for stable ordering.
#>       est       lci       uci 
#> 0.3846154 0.3344373 0.4347935 
#> attr(,"nObs")
#> [1] 1300
#> attr(,"prevalenceIndex")
#> [1] NA
#> attr(,"biasIndex")
#> [1] NA

# vector interface
x <- bedrock::untable(m2)
pabak(x$rater1, x$rater2, conf.level = 0.95)
#>       est       lci       uci 
#> 0.6000000 0.4432029 0.7567971 
#> attr(,"nObs")
#> [1] 100
#> attr(,"prevalenceIndex")
#> [1] 0.1
#> attr(,"biasIndex")
#> [1] 0.1
```
