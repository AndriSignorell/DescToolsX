# Compute Odds Ratios

Computes odds ratios, either from a 2x2 contingency table or from a
binomial generalized linear model. The table method returns a single
odds ratio, the model method one odds ratio per regression coefficient.

## Usage

``` r
oddsRatio(x, ...)

# Default S3 method
oddsRatio(
  x,
  y = NULL,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  method = c("wald", "exact", "midp"),
  interval = c(0, 1000),
  ...
)

# S3 method for class 'glm'
oddsRatio(
  x,
  conf.level = 0.95,
  sides = c("two.sided", "left", "right"),
  method = c("wald", "profile"),
  ...
)

# S3 method for class 'OddsRatio'
print(x, digits = 3, ...)
```

## Arguments

- x:

  a 2x2 contingency table, two vectors to be cross-tabulated, or a
  binomial [`glm`](https://rdrr.io/r/stats/glm.html) object

- ...:

  further arguments passed to methods. For the default method with two
  vectors, these reach [`table`](https://rdrr.io/r/base/table.html), so
  `useNA` can be set here.

- y:

  optional second variable. If supplied, `table(x, y, ...)` is computed.

- conf.level:

  confidence level of the interval. For the table method `NA` (the
  default) returns the point estimate only; the model method computes an
  interval by default.

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See
  [`ConfidenceIntervals`](ConfidenceIntervals.md). An odds ratio is
  bounded below by 0 and unbounded above, so the open side is reported
  at 0 or `Inf` accordingly. Ignored, with a warning, for
  `method = "profile"`.

- method:

  character string specifying the interval method. For a contingency
  table one of `"wald"`, `"exact"` or `"midp"`; for a binomial model one
  of `"wald"` or `"profile"`. See Details.

- interval:

  numeric vector of length two giving the search interval for the root
  finding in the mid-p method. Only used by `method = "midp"`; widen it
  if the reported bound sits at one of its ends.

- digits:

  number of digits used for printing

## Value

For a contingency table with `conf.level = NA` a numeric scalar,
otherwise a named numeric vector with the elements `est`, `lci` and
`uci`.

For a binomial model an object of class `"OddsRatio"`, a list with:

- `coefficients`:

  a data frame with one row per coefficient and the columns `term`,
  `est` (the exponentiated coefficient), `logEst` (the coefficient
  itself), `stdError` (on the log scale), `pValue`, `lci` and `uci`

- `source`:

  `"glm"`

- `method`, `conf.level`, `sides`:

  as supplied - with `sides` recording what was computed, which for
  `method = "profile"` is always `"two.sided"`

- `nObs`:

  number of observations used in the fit

- `call`:

  the model call

There is a `print` method; the interval bounds are on the odds scale,
the standard error on the log scale.

## Details

### Contingency tables

For a 2x2 table the odds ratio is

\$\$OR = \frac{n\_{11} n\_{22}}{n\_{12} n\_{21}}\$\$

Three interval methods are available. `"wald"` is the asymptotic
interval on the log scale, fast and adequate for reasonably large
counts. `"exact"` is the conditional interval based on the noncentral
hypergeometric distribution (Fisher); it guarantees coverage but is
conservative, sometimes markedly so. `"midp"` halves the probability of
the observed table and lies between the two: it has coverage closer to
the nominal level than the exact interval without the Wald interval's
reliance on large counts. With a zero cell the point estimate is 0 or
`Inf` and only `"exact"` and `"midp"` still deliver a finite bound on
the informative side.

### Binomial models

For a model fitted with `glm(family = binomial)`, each coefficient is
exponentiated: \\\exp(\beta_j)\\ is the factor by which the odds of the
response are multiplied when the corresponding predictor increases by
one unit, all other predictors held fixed. For a dummy variable this is
the odds ratio between the level and its reference level.

The intercept is exponentiated along with the rest, but
\\\exp(\beta_0)\\ is *not* an odds ratio - it is the odds of the
response when all predictors are zero. It is reported for completeness
and is usually not the quantity of interest.

Two interval methods are available. `"wald"` is the symmetric interval
on the log-odds scale, back-transformed. `"profile"` inverts the
likelihood ratio test through
[`confint.glm`](https://rdrr.io/r/stats/confint.html); it is asymmetric
on the odds scale, generally more reliable in small samples or with
sparse cells, and considerably slower because the model is refitted
along each coefficient. Profile intervals are two-sided by construction,
so `sides` is ignored for them and a warning is issued.

Unlike the table method, the model method computes an interval by
default (`conf.level = 0.95`): a coefficient table without intervals
would be less informative than
[`summary()`](https://rdrr.io/r/base/summary.html) itself.

## Note

No short alias is exported by default to avoid conflicts with rlang and
base R naming conventions. Call
[`attachAliases()`](attach-detach-aliases.md) once per session (or
script) to make `or()` available as a convenient shorthand.

## References

Agresti, A. (2013). *Categorical Data Analysis* (3rd ed.). Wiley.

Fisher, R. A. (1935). The logic of inductive inference. *Journal of the
Royal Statistical Society*, *98*(1), 39–82.

Gart, J. J. (1966). Alternative analyses of contingency tables. *Journal
of the Royal Statistical Society Series B*, *28*(1), 164–179.

## See also

[attachAliases](attach-detach-aliases.md), [relRisk](relRisk.md),
[`confint.glm`](https://rdrr.io/r/stats/confint.html)

Other effect.size: [`cohenD()`](cohenD.md), [`cohenH()`](cohenH.md),
[`etaSq()`](etaSq.md), [`glassDelta()`](glassDelta.md),
[`relRisk()`](relRisk.md)

## Examples

``` r
# --- 2x2 table -------------------------------------------------
tab <- matrix(c(10, 20,
                 5, 30), nrow = 2)

oddsRatio(tab)
#> [1] 3
oddsRatio(tab, conf.level = 0.95)
#>        est        lci        uci 
#>  3.0000000  0.8914747 10.0956314 

# the exact interval is the widest, the Wald interval the narrowest
sapply(c("wald", "exact", "midp"),
       function(m) oddsRatio(tab, conf.level = 0.95, method = m))
#>           wald      exact       midp
#> est  3.0000000  2.9483345  2.9118822
#> lci  0.8914747  0.7770679  0.8795254
#> uci 10.0956314 12.7381845 10.8604692

# one-sided: "left" carries the finite lower bound
oddsRatio(tab, conf.level = 0.95, sides = "left")
#>      est      lci      uci 
#> 3.000000 1.083524      Inf 


# --- binomial model --------------------------------------------
fit <- glm(vs ~ am + wt, data = mtcars, family = binomial)

oddsRatio(fit)
#> 
#> Call:
#> glm(formula = vs ~ am + wt, family = binomial, data = mtcars)
#> 
#> Odds Ratios (95% two.sided CI, method = wald):
#> 
#>                      est     lci          uci  pValue
#> (Intercept) 2.793411e+09 185.176 4.213903e+16 0.00991
#> am          2.000000e-03   0.000 4.310000e-01 0.02310
#> wt          2.000000e-03   0.000 2.060000e-01 0.00904
#> 

# the exponentiated intercept is the baseline odds, not an odds ratio
res <- oddsRatio(fit)
res$coefficients
#>          term          est    logEst stdError      pValue          lci
#> 1 (Intercept) 2.793411e+09 21.750529 8.433431 0.009906264 1.851762e+02
#> 2          am 2.169003e-03 -6.133488 2.700169 0.023115514 1.091119e-05
#> 3          wt 1.782578e-03 -6.329695 2.424632 0.009038881 1.538857e-05
#>            uci
#> 1 4.213903e+16
#> 2 4.311695e-01
#> 3 2.064898e-01

# profile likelihood intervals: asymmetric on the odds scale, slower
oddsRatio(fit, method = "profile")
#> Waiting for profiling to be done...
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> 
#> Call:
#> glm(formula = vs ~ am + wt, family = binomial, data = mtcars)
#> 
#> Odds Ratios (95% two.sided CI, method = profile):
#> 
#>                      est      lci          uci  pValue
#> (Intercept) 2.793411e+09 6739.932 5.653093e+18 0.00991
#> am          2.000000e-03    0.000 1.560000e-01 0.02310
#> wt          2.000000e-03    0.000 7.200000e-02 0.00904
#> 

```
