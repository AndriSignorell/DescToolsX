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
  delta = 0,
  ...
)
```

## Arguments

- x:

  a numeric 2x2 matrix or table of finite, non-negative integer counts,
  with positive row totals; alternatively an exposure vector when `y` is
  supplied. The first row is the exposed group and the first column is
  the event, regardless of their names.

- y:

  an optional outcome vector. If supplied, `table(x, y, ...)` is
  computed without reordering its levels. For binary 0/1 data, use
  `factor(x, levels = c(1, 0))` and `factor(y, levels = c(1, 0))` to
  obtain the orientation shown above. Missing values follow
  [`table()`](https://rdrr.io/r/base/table.html) conventions.

- conf.level:

  a single confidence level strictly between 0 and 1, or `NA` (the
  default) to return only the point estimate. One-sided intervals
  require a confidence level greater than 0.5.

- sides:

  character string specifying the sidedness of the confidence interval:
  `"two.sided"` (default), `"left"`, or `"right"`. `"left"` retains the
  lower bound and sets the upper bound to `Inf`; `"right"` sets the
  lower bound to 0 and retains the upper bound. See also
  [`ConfidenceIntervals()`](ConfidenceIntervals.md).

- method:

  character string specifying the interval method: `"score"` (default),
  `"wald"`, or `"use-or"`. See Details.

- delta:

  a single finite, non-negative number (default 0), the cell correction
  of the Wald-type methods. For `method = "wald"` it is added to each of
  the four cells before computing both the point estimate and the
  interval, so each row total increases by `2 * delta`; this also
  applies when only the point estimate is requested. For
  `method = "use-or"` it is passed to the Wald odds-ratio interval only;
  the estimate and `p2` stay uncorrected. `"score"` ignores `delta`
  after validating it.

- ...:

  further arguments passed to
  [`table()`](https://rdrr.io/r/base/table.html) when `y` is supplied

## Value

if `conf.level = NA`, a numeric scalar. Otherwise a named numeric vector
with elements `est`, `lci`, and `uci` for the estimate and bounds.
Without a Wald correction, no events in either group give `est = NaN`;
events only in the first group give `est = Inf`, and events only in the
second group give `est = 0`. The score interval remains available in
these cases, including `[0, Inf]` when neither group has events.

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

The score interval uses the uncorrected Koopman (1984) score statistic.
It does not include the `N / (N - 1)` variance correction associated
with the Miettinen–Nurminen variant, where `N = n1 + n2`. Bounds are
found by inverting the score statistic on the log risk-ratio scale.
Closed forms apply when both groups have no events or both groups
contain only events. If only the reference group has no events, the rows
are reversed and the resulting bounds are inverted. The numerical search
expands adaptively; failure to bracket a bound within numerical limits
raises an error rather than returning an artificial zero or infinite
endpoint.

The Wald method uses the log risk ratio and variance
`1 / x1 - 1 / n1 + 1 / x2 - 1 / n2`, evaluated after any cell
correction. With `delta = 0`, a zero event count in either group makes
the log-Wald interval unavailable: a warning is issued and its bounds
are `NA`. Use the score method for such data, or explicitly choose a
positive correction, for example `delta = 0.5`. Wald intervals are
asymptotic and can perform poorly with small counts or probabilities
near the boundaries. If both groups contain only events and `delta = 0`,
the formal Wald interval is `[1, 1]`; it is returned with a warning
because it is degenerate. For a one-sided interval, the corresponding
open endpoint is still applied.

The `"use-or"` method transforms the Wald bounds returned by
[`oddsRatio()`](oddsRatio.md) using `RR = OR / (1 - p2 + p2 * OR)`, with
`p2 = x2 / n2` fixed at its observed value. This is a plug-in
approximation: monotonicity of the transformation does not establish
nominal coverage when `p2` is estimated. It requires `0 < p2 < 1`, and,
like the Wald odds-ratio interval, positive cells unless `delta > 0`;
otherwise a warning is issued and the interval bounds are `NA`. An
infinite OR bound is mapped to its limit `1 / p2`.

For unavailable intervals, one-sided output still uses the natural open
endpoint (0 or `Inf`); the estimated endpoint remains `NA`. Rows or
columns can be reversed with
[`bedrock::revX()`](https://andrisignorell.github.io/bedrock/reference/revX.html),
or the table transposed with [`t()`](https://rdrr.io/r/base/t.html), to
obtain the required orientation.

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
#> 0.5298570 0.2974925 0.9437161 

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


# explicitly corrected Wald estimate and interval
relRisk(matrix(c(0, 4, 10, 6), nrow = 2),
        method = "wald", delta = 0.5, conf.level = 0.95)
#>         est         lci         uci 
#> 0.111111111 0.006758933 1.826572271 
```
