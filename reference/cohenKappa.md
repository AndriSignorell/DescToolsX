# Cohen's Kappa and Weighted Kappa

Computes Cohen's kappa and weighted kappa as measures of inter-rater
agreement, together with asymptotic confidence intervals.

## Usage

``` r
cohenKappa(
  x,
  y = NULL,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  weights = c("unweighted", "equal-spacing", "fleiss-cohen"),
  ...
)
```

## Arguments

- x:

  a square confusion matrix (or data frame), or a categorical vector
  when `y` is provided

- y:

  `NULL` (default) or a categorical vector with compatible dimensions to
  `x`. When supplied, `table(x, y, ...)` is computed. The vector
  interface is available for unweighted kappa only (see Details).

- conf.level:

  confidence level of the interval. If set to `NA` (the default), only
  the point estimate is returned.

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See
  [`ConfidenceIntervals`](ConfidenceIntervals.md).

- weights:

  either a character string selecting a built-in weight scheme –
  `"unweighted"` (default), `"equal-spacing"`, or `"fleiss-cohen"` – or
  a numeric matrix with the same dimensions as `x` supplying
  user-defined weights for each cell

- ...:

  further arguments passed to
  [`table`](https://rdrr.io/r/base/table.html) for the vector interface,
  for example `useNA`

## Value

if `conf.level = NA`, a numeric scalar containing kappa; otherwise a
named numeric vector with elements:

- `est`:

  point estimate of kappa.

- `lci`:

  lower confidence interval bound.

- `uci`:

  upper confidence interval bound.

## Details

Cohen's kappa is the diagonal sum of the (possibly weighted) relative
frequencies, corrected for chance agreement and standardised by its
maximum value.

The equal-spacing weights (Cicchetti & Allison, 1971) are defined as
\$\$1 - \frac{\|i - j\|}{r - 1}\$\$ and the Fleiss-Cohen weights as
\$\$1 - \frac{(i - j)^2}{(r - 1)^2}\$\$ where \\r\\ is the number of
rows/columns. The Fleiss-Cohen weights attach greater importance to
closer disagreements.

Data can be passed either as a square confusion matrix (or data frame)
in `x`, or as two vectors `x` and `y`, in which case `table(x, y, ...)`
is computed internally. Note that the vector interface supports
**unweighted kappa only**: the function raises an error if `weights` is
not `"unweighted"` and `y` is supplied, because the level ordering of
two independent factors cannot be guaranteed to be consistent when
constructing the confusion table.

Missing values are handled as
[`table`](https://rdrr.io/r/base/table.html) does – excluded by default.
Pass `useNA = "ifany"` via `...` to include them.

`sides` names the side on which the finite bound lies: `"left"` yields
an interval bounded below, with the upper limit fixed at 1, and
`"right"` one bounded above, with the lower limit fixed at -1. Kappa is
a bounded parameter, so the open side is reported at the range boundary
rather than as \\\pm\infty\\ (design_rules.md 4.1), matching
[`ccc`](ccc.md).

## Note

Based on code by David Meyer, adapted to conform to package standards.

## References

Cohen, J. (1960). A coefficient of agreement for nominal scales.
*Educational and Psychological Measurement*, *20*(1), 37–46.

Everitt, B. S. (1968). Moments of statistics kappa and weighted kappa.
*The British Journal of Mathematical and Statistical Psychology*,
*21*(1), 97–103.

Fleiss, J. L., Cohen, J., & Everitt, B. S. (1969). Large sample standard
errors of kappa and weighted kappa. *Psychological Bulletin*, *72*(5),
323–327.

Cicchetti, D. V., & Allison, T. (1971). A new procedure for assessing
reliability of scoring EEG sleep recordings. *American Journal of EEG
Technology*, *11*(3), 101–109.

## See also

[`pairApply`](https://andrisignorell.github.io/bedrock/reference/pairApply.html)

Other assoc.agreement: [`ccc()`](ccc.md),
[`cronbachAlpha()`](cronbachAlpha.md), [`icc()`](icc.md),
[`kappaM()`](kappaM.md), [`krippAlpha()`](krippAlpha.md),
[`pabak()`](pabak.md), [`percAgreement()`](percAgreement.md),
[`randolphKappa()`](randolphKappa.md)

## Examples

``` r
# from Bortz et al. (1990), p. 459
m <- matrix(c(53,  5, 2,
              11, 14, 5,
               1,  6, 3), nrow = 3, byrow = TRUE,
            dimnames = list(rater1 = c("V", "N", "P"),
                            rater2 = c("V", "N", "P")))

cohenKappa(m)
#> [1] 0.4285714
cohenKappa(m, conf.level = 0.95)
#>       est       lci       uci 
#> 0.4285714 0.2796949 0.5774480 

# vector interface (unweighted only)
x <- bedrock::untable(m)
cohenKappa(x$rater1, x$rater2)
#> [1] 0.4285714

# equal-spacing weights
cats <- c("<10%", "11-20%", "21-30%", "31-40%", "41-50%", ">50%")
mw <- matrix(
  c(5,8,1,2,4,2, 3,5,3,5,5,0, 1,2,6,11,2,1,
    0,1,5,4,3,3, 0,0,1,2,5,2, 0,0,1,2,1,4),
  nrow = 6, byrow = TRUE,
  dimnames = list(rater1 = cats, rater2 = cats))

cohenKappa(mw, weights = "equal-spacing", conf.level = 0.95)
#>       est       lci       uci 
#> 0.3156685 0.1968117 0.4345252 

# user-supplied weight matrix
wm <- outer(1:6, 1:6, function(i, j) 1 - abs(i - j) / (6 - 1))
cohenKappa(mw, weights = wm, conf.level = 0.95)
#>       est       lci       uci 
#> 0.3156685 0.1968117 0.4345252 

# pairwise kappa across raters
rating <- data.frame(
  rtr1 = c(4,2,2,5,2,1,3,1,1,5,1,1,2,1,2,3,1,1,2,1,5,2,2,1,1,2,1,2,1,5),
  rtr2 = c(4,2,3,5,2,1,3,1,1,5,4,2,2,4,2,3,1,1,2,3,5,4,2,1,4,2,1,2,3,5))

bedrock::pairApply(rating, FUN = cohenKappa, symmetric = TRUE)
#>           rtr1      rtr2
#> rtr1 1.0000000 0.6511628
#> rtr2 0.6511628 1.0000000
```
