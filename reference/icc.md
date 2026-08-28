# Intraclass Correlation Coefficient (ICC)

Computes intraclass correlation coefficients (ICC) according to Shrout
and Fleiss (1979) and McGraw and Wong (1996).

## Usage

``` r
icc(
  x,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  method = c("anova", "reml", "boot"),
  model = c("twoway", "oneway"),
  type = c("agreement", "consistency"),
  unit = c("single", "average"),
  na.rm = FALSE,
  ...
)
```

## Arguments

- x:

  numeric matrix or data frame with subjects in rows and raters in
  columns

- conf.level:

  confidence level of the interval. If set to `NA` (the default), only
  the point estimate is returned.

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See
  [`ConfidenceIntervals`](ConfidenceIntervals.md).

- method:

  character string specifying the estimation and confidence interval
  method; defaults to `"anova"`

- model:

  character string, either `"oneway"` or `"twoway"`

- type:

  character string, either `"agreement"` or `"consistency"`

- unit:

  character string, either `"single"` or `"average"`

- na.rm:

  logical; if `TRUE`, complete cases are used

- ...:

  additional arguments. For `method = "boot"`, the number of bootstrap
  resamples can be specified via `R`.

## Value

if `conf.level = NA`, a numeric scalar. Otherwise a named numeric vector
with elements:

- `est`:

  point estimate of the intraclass correlation

- `lci`:

  lower confidence interval bound

- `uci`:

  upper confidence interval bound

## Details

The ICC is a measure of reliability for ratings of \\n\\ subjects by
\\k\\ raters. The specific coefficient depends on three design
decisions:

- **model**: one-way or two-way ANOVA design

- **type**: agreement or consistency

- **unit**: single rating or average of k ratings

The six classical Shrout–Fleiss cases are:

|        |             |                 |
|--------|-------------|-----------------|
| model  | type        | unit            |
| oneway | agreement   | single (ICC1)   |
| oneway | agreement   | average (ICC1k) |
| twoway | agreement   | single (ICC2)   |
| twoway | agreement   | average (ICC2k) |
| twoway | consistency | single (ICC3)   |
| twoway | consistency | average (ICC3k) |

For `model = "oneway"` only `type = "agreement"` is meaningful.

Confidence intervals can be computed using different inference methods:

- `"anova"`: exact F-based intervals following Shrout and Fleiss (1979)

- `"reml"`: variance components estimated via REML. Point estimate only;
  no confidence interval is available for this method.

- `"boot"`: nonparametric percentile bootstrap

ICC(1) is based on a one-way random effects ANOVA and measures absolute
agreement. ICC(2) assumes raters are randomly sampled and generalizable,
while ICC(3) assumes a fixed set of raters.

The average forms (k) reflect the reliability of the mean of k raters
and correspond to the Spearman–Brown adjusted reliability.

The ANOVA-based confidence intervals follow the exact formulas of Shrout
and Fleiss (1979), including the variance approximation for ICC(2).

## Random number generation

`method = "boot"` resamples subjects and therefore advances R's global
random number generator. Call
[`set.seed`](https://rdrr.io/r/base/Random.html) beforehand for
reproducible intervals.

## References

Shrout, P. E., Fleiss, J. L. (1979). Intraclass correlations: uses in
assessing rater reliability. *Psychological Bulletin*, 86, 420–428.

McGraw, K. O., Wong, S. P. (1996). Forming inferences about some
intraclass correlation coefficients. *Psychological Methods*, 1, 30–46.

## See also

Other assoc.agreement: [`ccc()`](ccc.md),
[`cohenKappa()`](cohenKappa.md), [`cronbachAlpha()`](cronbachAlpha.md),
[`kappaM()`](kappaM.md), [`krippAlpha()`](krippAlpha.md),
[`pabak()`](pabak.md), [`percAgreement()`](percAgreement.md),
[`randolphKappa()`](randolphKappa.md)

## Examples

``` r
#example from Shrout and Fleiss (1979)
sf <- matrix(c( 9, 2, 5, 8,    6, 1, 3, 2,    8, 4, 6, 8,     
                7, 1, 2, 6,   10, 5, 6, 9,    6, 2, 4, 7),
      ncol=4, byrow=TRUE,
      dimnames=list(c("S1","S2","S3","S4","S5","S6"), 
                    c("J1","J2","J3","J4"))  )

icc(sf)
#> [1] 0.2897638

# get all versions
args <- formals(icc)[c("model","type","unit")]
grid <- expand.grid(lapply(args, eval), 
                    stringsAsFactors = FALSE)[-c(4,8),]
                    
out <- apply(grid, 1, function(row)
  icc(sf,
      model = row["model"],
      type  = row["type"],
      unit  = row["unit"],
      method = "anova",
      conf.level = 0.95) )
      
t(simplify2array(out))
#>         est         lci       uci
#> 1 0.2897638  0.01878651 0.7610844
#> 2 0.1657418 -0.13293232 0.7225601
#> 3 0.7148407  0.34246477 0.9458583
#> 5 0.6200505  0.07113682 0.9272320
#> 6 0.4427971 -0.88444216 0.9124154
#> 7 0.9093155  0.67567471 0.9858917
```
