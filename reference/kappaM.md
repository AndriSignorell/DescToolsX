# Kappa for m Raters

Computes agreement among \\m \ge 2\\ raters on categorical items, using
Fleiss' kappa (the default), Conger's exact kappa, or Light's kappa.

## Usage

``` r
kappaM(
  x,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  estimator = c("fleiss", "conger", "light"),
  use = c("complete.obs", "everything", "pairwise.complete.obs")
)
```

## Arguments

- x:

  a \\n \times m\\ matrix or data frame, \\n\\ subjects in rows and
  \\m\\ raters in columns

- conf.level:

  confidence level of the interval. If set to `NA` (the default), only
  the point estimate is returned.

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See
  [`ConfidenceIntervals`](ConfidenceIntervals.md).

- estimator:

  a character string specifying the coefficient to compute. One of
  `"fleiss"` (default), `"conger"`, or `"light"`. These are three
  different coefficients for the same quantity, not three interval
  methods - hence `estimator` and not `method`.

- use:

  a character string giving the treatment of missing values. One of
  `"complete.obs"` (default), which drops any subject rated
  incompletely; `"everything"`, which returns `NA` if any value is
  missing; or `"pairwise.complete.obs"`, which uses all subjects rated
  by both members of each rater pair. The last is available for
  `estimator = "light"` only, since Fleiss' and Conger's coefficients
  require a complete row per subject.

## Value

a named numeric vector. If `conf.level = NA`, only `est` is returned;
otherwise the vector has elements:

- `est`:

  point estimate of the selected kappa coefficient

- `lci`:

  lower confidence interval bound

- `uci`:

  upper confidence interval bound

## Details

All three coefficients contrast the mean observed agreement with the
agreement expected by chance, and differ in how that chance agreement is
derived.

- `"fleiss"`:

  Fleiss (1971) bases chance agreement on the category proportions
  pooled across all raters. It generalises Scott's pi (Scott, 1955), and
  for \\m = 2\\ it equals Scott's pi - not Cohen's kappa.

- `"conger"`:

  Conger (1980) derives chance agreement from each rater's own marginal
  distribution, and reduces exactly to Cohen's kappa when \\m = 2\\.

- `"light"`:

  Light (1971) is the unweighted mean of all \\\binom{m}{2}\\ pairwise
  Cohen kappas.

Confidence intervals are of Wald type. For `"fleiss"` and `"conger"` the
standard error is the analytic one given by the respective author. For
`"light"` no closed-form variance exists; since the estimate is the mean
of the pairwise kappas, its standard error is estimated as their
standard deviation divided by \\\sqrt{\binom{m}{2}}\\. This treats the
pairwise kappas as independent, which they are not - they share raters -
so the interval is approximate and tends to be too narrow. A bootstrap
interval is preferable when the assumption is doubtful.

Kappa lies in \\\[-1, 1\]\\, so the interval is restricted to that range
and the open side of a one-sided interval is reported at the boundary
rather than at \\\pm\infty\\. See
[`ConfidenceIntervals`](ConfidenceIntervals.md).

## Note

Based on code by Matthias Gamer previously published as
`kappaM.fleiss()` in the irr package, rewritten to conform to package
standards.

## References

Conger, A. J. (1980). Integration and generalisation of Kappas for
multiple raters. *Psychological Bulletin*, *88*(2), 322-328.

Fleiss, J. L. (1971). Measuring nominal scale agreement among many
raters. *Psychological Bulletin*, *76*(5), 378-382.

Fleiss, J. L., Levin, B., & Paik, M. C. (2003). *Statistical Methods for
Rates and Proportions* (3rd ed.). New York: John Wiley & Sons.

Light, R. J. (1971). Measures of response agreement for qualitative
data: Some generalizations and alternatives. *Psychological Bulletin*,
*76*(5), 365-377.

Scott, W. A. (1955). Reliability of content analysis: The case of
nominal scale coding. *Public Opinion Quarterly*, *19*(3), 321-325.

## See also

Other assoc.agreement: [`ccc()`](ccc.md),
[`cohenKappa()`](cohenKappa.md), [`cronbachAlpha()`](cronbachAlpha.md),
[`icc()`](icc.md), [`krippAlpha()`](krippAlpha.md),
[`pabak()`](pabak.md), [`percAgreement()`](percAgreement.md),
[`randolphKappa()`](randolphKappa.md)

## Examples

``` r
statement <- data.frame(
  A = c(2,3,1,3,1,2,1,2,3,3,3,3,3,2,1,3,3,2,2,1,
        2,1,3,3,2,2,1,2,1,1,2,3,3,3,3,3,1,2,1,1),
  B = c(2,2,2,1,1,2,1,2,3,3,2,3,1,3,1,1,3,2,1,2,
        2,1,3,2,2,2,3,2,1,1,2,2,3,3,3,3,2,2,2,3),
  C = c(2,2,2,1,1,2,1,2,3,3,2,3,3,3,3,2,2,2,2,3,
        2,2,3,3,2,2,3,2,2,2,2,3,3,3,3,3,3,2,2,2),
  D = c(2,2,2,1,1,2,1,2,3,3,2,3,3,3,3,3,2,2,2,2,
        3,1,3,2,2,2,1,2,2,1,2,3,3,3,3,3,3,2,2,1),
  E = c(2,2,2,3,3,2,3,1,3,3,2,3,3,3,3,3,2,2,2,3,
        2,3,3,2,2,2,3,2,1,3,2,3,3,1,3,3,3,2,2,1)
)

kappaM(statement)
#>       est 
#> 0.4809414 

# Conger's exact kappa
kappaM(statement, estimator = "conger")
#>       est 
#> 0.4844042 

# Light's kappa, the mean of the pairwise Cohen kappas
kappaM(statement, estimator = "light")
#>       est 
#> 0.4868577 

# Fleiss' kappa with a confidence interval
kappaM(statement, conf.level = 0.95)
#>       est       lci       uci 
#> 0.4809414 0.4086236 0.5532593 
```
