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
  method = c("wald", "exact", "mid-p"),
  delta = 0,
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

  a 2x2 contingency table of finite, non-negative integer counts with
  positive row and column totals, an exposure vector to be
  cross-tabulated with `y`, or a binomial
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html) object

- ...:

  further arguments passed to methods. For the default method with two
  vectors, these reach [`table()`](https://rdrr.io/r/base/table.html),
  so `useNA` can be set here.

- y:

  optional second variable. If supplied, `table(x, y, ...)` is computed
  without reordering its levels.

- conf.level:

  confidence level of the interval. For the table method `NA` (the
  default) returns the point estimate only; the model method computes an
  interval by default.

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See
  [`ConfidenceIntervals()`](ConfidenceIntervals.md). An odds ratio is
  bounded below by 0 and unbounded above, so the open side is reported
  at 0 or `Inf` accordingly. Ignored, with a warning, for
  `method = "profile"`.

- method:

  character string specifying the method. For a contingency table one of
  `"wald"`, `"exact"` or `"mid-p"`; for a binomial model one of `"wald"`
  or `"profile"`. See Details.

- delta:

  a single finite, non-negative number (default 0). Only for
  `method = "wald"`, this value is added to each of the four cells
  before computing both the point estimate and the interval, also when
  only the point estimate is requested. The other methods ignore `delta`
  after validating it.

- digits:

  number of digits used for printing

## Value

For a contingency table with `conf.level = NA` a numeric scalar,
otherwise a named numeric vector with the elements `est`, `lci` and
`uci`. The estimate depends on `method`, see Details.

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

For a 2x2 table with cells \\n\_{ij}\\ the sample odds ratio is

\$\$OR = \frac{n\_{11} n\_{22}}{n\_{12} n\_{21}}\$\$

Three methods are available, and each comes with its own point estimate.

`"wald"` uses the sample odds ratio and the asymptotic interval on the
log scale, with variance \\\sum 1/n\_{ij}\\. It is fast and adequate for
reasonably large counts. With a zero cell the estimate is 0 or `Inf` and
the interval is unavailable: a warning is issued and its bounds are
`NA`. Use `"exact"` or `"mid-p"` for such data, or explicitly choose a
correction, for example `delta = 0.5` (Haldane–Anscombe).

`"exact"` uses the conditional maximum likelihood estimate and the
conditional (Fisher) interval, both based on the noncentral
hypergeometric distribution of \\n\_{11}\\ given the margins. The
interval guarantees coverage but is conservative, sometimes markedly so.
These are the estimate and interval of
[`stats::fisher.test()`](https://rdrr.io/r/stats/fisher.test.html),
computed to full precision (see below).

`"mid-p"` uses the conditional median-unbiased estimate and the mid-p
interval, which counts only half the probability of the observed table.
Its coverage is closer to the nominal level than that of the exact
interval, without the Wald interval's reliance on large counts.

The conditional methods remain informative with a zero cell: if
\\n\_{11}\\ lies at the lower end of its support, the estimate and the
lower bound are 0; at the upper end, the estimate and the upper bound
are `Inf`. The other bound is finite.

The conditional estimates and bounds are found by root finding on the
log odds-ratio scale with a relative tolerance of about `1e-10`.
[`stats::fisher.test()`](https://rdrr.io/r/stats/fisher.test.html) uses
an absolute tolerance of about `1e-4` on the odds-ratio scale instead,
which is inaccurate for small odds ratios and bounds; results agree with
it up to that tolerance.

### Binomial models

For a model fitted with `glm(family = binomial)`, each coefficient is
exponentiated: \\\exp(\beta_j)\\ is the factor by which the odds of the
response are multiplied when the corresponding predictor increases by
one unit, all other predictors held fixed. For a dummy variable this is
the odds ratio between the level and its reference level. Quasi-binomial
models are not supported; their coefficient table reports t rather than
z statistics.

The intercept is exponentiated along with the rest, but
\\\exp(\beta_0)\\ is *not* an odds ratio - it is the odds of the
response when all predictors are zero. It is reported for completeness
and is usually not the quantity of interest.

Two interval methods are available. `"wald"` is the symmetric interval
on the log-odds scale, back-transformed. `"profile"` inverts the
likelihood ratio test through
[`stats::confint.glm()`](https://rdrr.io/r/stats/confint.html); it is
asymmetric on the odds scale, generally more reliable in small samples
or with sparse cells, and considerably slower because the model is
refitted along each coefficient. Profile intervals are two-sided by
construction, so `sides` is ignored for them and a warning is issued.

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

Hirji, K. F. (2006). *Exact Analysis of Discrete Data*. Chapman &
Hall/CRC.

## See also

[attachAliases](attach-detach-aliases.md), [relRisk](relRisk.md),
[`stats::fisher.test()`](https://rdrr.io/r/stats/fisher.test.html),
[`stats::confint.glm()`](https://rdrr.io/r/stats/confint.html)

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

# each method has its own estimate; for this table the exact interval
# is the widest and the Wald interval the narrowest
sapply(c("wald", "exact", "mid-p"),
       function(m) oddsRatio(tab, conf.level = 0.95, method = m))
#>           wald      exact      mid-p
#> est  3.0000000  2.9483338  2.9118815
#> lci  0.8914747  0.7770667  0.8795264
#> uci 10.0956314 12.7407668 10.8598558

# one-sided: "left" carries the finite lower bound
oddsRatio(tab, conf.level = 0.95, sides = "left")
#>      est      lci      uci 
#> 3.000000 1.083524      Inf 

# a zero cell: the conditional methods keep the informative bound
z <- matrix(c(10, 0, 5, 7), nrow = 2)
oddsRatio(z, conf.level = 0.95, method = "mid-p")
#>      est      lci      uci 
#>      Inf 2.463328      Inf 
oddsRatio(z, conf.level = 0.95, method = "wald", delta = 0.5)
#>        est        lci        uci 
#>  28.636364   1.365812 600.405723 


# --- binomial model --------------------------------------------
fit <- glm(vs ~ am + wt, data = mtcars, family = binomial)

oddsRatio(fit)
#> 
#> Call:
#> glm(formula = vs ~ am + wt, family = binomial, data = mtcars)
#> 
#> Odds Ratios (95% two.sided CI, method = wald):
#> 
#>                   est     lci       uci pValue
#> (Intercept) 2.793e+09 185.176 4.214e+16  0.010
#> am              0.002   0.000     0.431  0.023
#> wt              0.002   0.000     0.206  0.009
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
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> 
#> Call:
#> glm(formula = vs ~ am + wt, family = binomial, data = mtcars)
#> 
#> Odds Ratios (95% two.sided CI, method = profile):
#> 
#>                   est      lci       uci pValue
#> (Intercept) 2.793e+09 6739.932 5.653e+18  0.010
#> am              0.002    0.000     0.156  0.023
#> wt              0.002    0.000     0.072  0.009
#> 

```
