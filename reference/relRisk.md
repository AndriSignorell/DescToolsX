# Relative Risk

Computes the relative risk for a 2x2 contingency table together with
optional confidence intervals.

## Usage

``` r
relRisk(
  x,
  y = NULL,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  method = c("score", "wald", "use-or"),
  delta = 0.5,
  ...
)
```

## Arguments

- x:

  a numeric 2x2 matrix or table containing non-negative counts

- y:

  an optional vector. If supplied, `table(x, y, ...)` is computed.

- conf.level:

  confidence level for the interval estimate. If `NA` (default), only
  the point estimate is returned.

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See details
  in [`ConfidenceIntervals`](ConfidenceIntervals.md).

- method:

  character string specifying the confidence interval method. One of
  `"score"`, `"wald"`, or `"use-or"`.

- delta:

  small continuity correction added to the event counts in the *standard
  error* of the Wald interval. Only used if `method = "wald"`; see the
  note below.

- ...:

  further arguments passed to
  [`table`](https://rdrr.io/r/base/table.html)

## Value

if `conf.level = NA`, a numeric scalar. Otherwise a named numeric vector
with elements:

- `est`:

  relative risk estimate

- `lci`:

  lower confidence interval bound

- `uci`:

  upper confidence interval bound

## Details

The relative risk compares the event probability in the exposed group
with the event probability in the unexposed group.

The function expects the exposure groups in the rows and the outcome in
the columns:


                   outcome = 1   outcome = 0
    exposed = 1        x1           n1 - x1
    exposed = 0        x2           n2 - x2

The relative risk is defined as:

\$\$ RR = \frac{x_1 / n_1}{x_2 / n_2} \$\$

Confidence intervals can be calculated using the score method of Koopman
(1984), a Wald approximation, or via transformation of the odds ratio.

The score interval is based on the method of Koopman (1984) and
Miettinen and Nurminen (1985). It is obtained from the closed-form
solution of the cubic equation in the constrained maximum likelihood
estimate; if the unexposed group has no non-events (`x2 == n2`) that
cubic has a root on the parameter boundary and its roots can no longer
be assigned to the two interval bounds by their order alone. This case
is therefore solved directly from the score statistic
([`uniroot`](https://rdrr.io/r/stats/uniroot.html)); both routes agree
to numerical precision wherever the closed form applies.

The Wald interval is asymptotic and may perform poorly for small counts
or extreme probabilities. Note that `delta` enters the standard error
only, not the point estimate the interval is centred on: with a zero
cell the point estimate is `0` or `Inf` and the Wald interval
degenerates accordingly. Use `method = "score"` for tables with zero
cells.

If the table orientation differs from the required structure, rows or
columns can be reversed using
[`revX`](https://andrisignorell.github.io/bedrock/reference/revX.html)
or transposed with [`t`](https://rdrr.io/r/base/t.html).

## References

Koopman, P. A. R. (1984). Confidence intervals for the ratio of two
binomial proportions. *Biometrics*, *40*(2), 513–517.

Miettinen, O., & Nurminen, M. (1985). Comparative analysis of two rates.
*Statistics in Medicine*, *4*(2), 213–226.

Rothman, K. J., Greenland, S., & Lash, T. L. (2008). *Modern
Epidemiology* (3rd ed.). Lippincott Williams & Wilkins.

## See also

Other effect.size: [`cohenD()`](cohenD.md), [`cohenH()`](cohenH.md),
[`etaSq()`](etaSq.md), [`glassDelta()`](glassDelta.md),
[`oddsRatio()`](oddsRatio.md)

## Examples

``` r
m <- matrix(
  c(78, 50,
    1422, 950),
  nrow = 2,
  dimnames = list(
    water = c("cont", "clean"),
    diarrhea = c("yes", "no")
  )
)

relRisk(m, conf.level = 0.95)
#>       est       lci       uci 
#> 1.0400000 0.7375967 1.4682951 


mm <- cbind(c(9, 20), c(41, 29))

relRisk(t(mm), conf.level = 0.95)
#>       est       lci       uci 
#> 0.5298570 0.2869143 0.8869267 

relRisk(
  t(mm),
  conf.level = 0.95,
  method = "wald"
)
#>       est       lci       uci 
#> 0.5298570 0.3037489 0.9242780 

relRisk(
  t(mm),
  conf.level = 0.95,
  method = "use-or"
)
#>       est       lci       uci 
#> 0.5298570 0.2597810 0.9051415 

# unexposed group without non-events: the score interval is still valid
relRisk(matrix(c(2, 5, 3, 0), nrow = 2), conf.level = 0.95)
#>       est       lci       uci 
#> 0.4000000 0.1176208 0.9378042 

```
