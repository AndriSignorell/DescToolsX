# Cramer's V

Measures the strength of association between two categorical variables.
These can be provided as two data vectors `x` and `y`, or as a
contingency table (see [Association](Association.md)).

## Usage

``` r
cramerV(
  x,
  y = NULL,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  method = c("ncchisq", "ncchisqadj", "fisher", "fisheradj"),
  correct = FALSE,
  ...
)
```

## Arguments

- x:

  either a contingency table, a two-column object (matrix, data.frame or
  list), or a vector of observations (together with `y`)

- y:

  optional second vector. If `x` is not a vector, `y` must be `NULL`.

- conf.level:

  confidence level of the interval. If set to `NA` (the default), only
  the point estimate is returned.

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See
  [`ConfidenceIntervals`](ConfidenceIntervals.md).

- method:

  character string specifying the confidence interval method:
  `"ncchisq"` (default, using the noncentral chi-squared distribution),
  `"ncchisqadj"`, `"fisher"` (using fisher z transformation), or
  `"fisheradj"` (using the Fisher z transformation and bias correction)

- correct:

  logical; whether to apply the bias correction of Bergsma (2013);
  defaults to `FALSE`

- ...:

  further arguments, passed on to
  [`normalizeToConfusion`](normalizeToConfusion.md) and
  [`table`](https://rdrr.io/r/base/table.html) for building the table -
  `useNA` is the usual one.

## Value

if `conf.level = NA`, a numeric scalar containing Cramer's V; otherwise
a named numeric vector with elements:

- `est`:

  point estimate of Cramer's V.

- `lci`:

  lower confidence interval bound.

- `uci`:

  upper confidence interval bound.

## Details

Cramer's V ranges from 0 to 1, with 0 indicating statistical
independence.

`correct = TRUE` applies Bergsma's bias correction to the point estimate
*and* to the interval: both are put through the same transformation of
the chi-squared statistic, so estimate and bounds always live on the
same scale. Formerly the two noncentral chi-squared methods returned an
uncorrected interval around a corrected estimate.

All four methods are analytical; there is no bootstrap here, and none is
needed - unlike [contCoef](contCoef.md), where no generally accepted
analytical interval exists.

For a one-sided interval the bound is computed at the adjusted level
\\2\gamma - 1\\ and the open side is closed at the boundary of the
attainable range, 0 or 1, rather than at an infinity V can never reach.
`sides = "left"` names the side carrying the *finite* bound and is the
analogue of `alternative = "greater"` in a test.

The two Fisher methods rest on \\\mathrm{Var}(\mathrm{atanh}(V)) \approx
1/(n-3)\\. Where that approximation has nothing to say - three or fewer
observations, or a perfect association, where the transformation is
infinite - the bounds are `NA` and a warning names the reason. The
estimate itself is still returned.

## Note

Based on code by Michael Smithson (confidence intervals), adapted to
conform to package standards.

## References

Cramer, H. (1946) *Mathematical Methods of Statistics*. Princeton
University Press

Agresti, Alan (1996) *Introduction to categorical data analysis*. NY:
John Wiley and Sons

Bergsma, W. (2013) A bias-correction for Cramer's V and Tschuprow's T
*Journal of the Korean Statistical Society* 42(3) DOI:
10.1016/j.jkss.2012.10.002

## See also

[base::table](https://rdrr.io/r/base/table.html),
[pharos::plotCor](https://andrisignorell.github.io/pharos/reference/plotCor.html),
[bedrock::pairApply](https://andrisignorell.github.io/bedrock/reference/pairApply.html),
[Association](Association.md)

Other assoc.nominal: [`contCoef()`](contCoef.md), [`gkTau()`](gkTau.md),
[`lambda()`](lambda.md), [`mutInf()`](mutInf.md), [`phi()`](phi.md),
[`tschuprowT()`](tschuprowT.md), [`uncertCoef()`](uncertCoef.md),
[`yule`](yule.md)

## Examples

``` r

tab <- table(Pizza$driver, Pizza$wine_delivered)
cramerV(tab)
#> [1] 0.1328222

# just x and y
cramerV(Pizza$driver, Pizza$wine_delivered)
#> [1] 0.1328222

# data.frame
bedrock::pairApply(Pizza[, c("driver", "operator", "area")], cramerV,
                   symmetric = TRUE)
#>             driver   operator       area
#> driver   1.0000000 0.23585686 0.65018461
#> operator 0.2358569 1.00000000 0.08670047
#> area     0.6501846 0.08670047 1.00000000

# useNA is passed on to table()
bedrock::pairApply(Pizza[, c("driver", "operator", "area")], cramerV,
                   useNA = "ifany", symmetric = TRUE)
#>             driver   operator       area
#> driver   1.0000000 0.20253639 0.53066544
#> operator 0.2025364 1.00000000 0.07847762
#> area     0.5306654 0.07847762 1.00000000

d.frm <- Pizza[, c("driver", "operator", "area")]
bedrock::pairApply(d.frm[complete.cases(d.frm), ], cramerV, symmetric = TRUE)
#>             driver  operator      area
#> driver   1.0000000 0.2345141 0.6504665
#> operator 0.2345141 1.0000000 0.0869935
#> area     0.6504665 0.0869935 1.0000000

# one-sided: "left" carries the finite lower bound, the upper one opens
# to the maximum V can attain
cramerV(tab, conf.level = 0.95, sides = "left")
#>        est        lci        uci 
#> 0.13282221 0.06256252 1.00000000 


# Bootstrap confidence intervals for Cramer's V
# http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf, p. 1821

tab <- as.table(rbind(
  c(26, 26, 23, 18,  9),
  c( 6,  7,  9, 14, 23)))
d.frm <- bedrock::untable(tab)

set.seed(1)
n <- 1000
idx <- matrix(sample(nrow(d.frm), size = nrow(d.frm) * n, replace = TRUE),
              ncol = n, byrow = FALSE)
v <- apply(idx, 2, function(x) cramerV(d.frm[x, 1], d.frm[x, 2]))
quantile(v, probs = c(0.025, 0.975))
#>      2.5%     97.5% 
#> 0.2771562 0.5591357 

# compare this to the analytical ones
cramerV(tab, conf.level = 0.95)
#>       est       lci       uci 
#> 0.4064888 0.2211575 0.5410535 
```
