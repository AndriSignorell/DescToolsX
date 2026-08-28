# Spearman Rank Correlation

Calculate Spearman correlation coefficient and its confidence interval.
In addition to the base R function
[`cor`](https://rdrr.io/r/stats/cor.html)`(x, method="spearman")`,
frequency tables are also accepted as arguments (i.e. actually weights
are used).

## Usage

``` r
spearmanCor(
  x,
  y = NULL,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  na.rm = FALSE
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

- na.rm:

  logical; whether to remove incomplete pairs. Applies to the vector
  interface; a frequency table must not contain missing counts.

## Value

if `conf.level = NA`, a numeric scalar. Otherwise a named numeric vector
with elements:

- `est`:

  point estimate of Spearman's rank correlation

- `lci`:

  lower confidence interval bound

- `uci`:

  upper confidence interval bound

## Details

The function calculates Spearman's rho statistic by means of
`cor(..., method="spearman")` when two variables `x` and `y` are
supplied. If a frequency table is provided an implementation based on
SAS documentation is used. Both routes use midranks for ties and agree
exactly; see the examples.  
The confidence intervals are calculated via z-Transformation.  

The number of observations entering the z-transformation is `length(x)`
in the vector interface and `sum(x)`, the table total, in the table
interface. At least four observations are needed for an interval.

`sides` names the side of the interval that carries the finite bound, so
`"left"` returns `[lci, 1]` and `"right"` returns `[-1, uci]`. Since rho
is bounded, the open side is reported at the range boundary rather than
as infinite.

## References

Conover W. J. (1999) *Practical Nonparametric Statistics (3rd edition)*.
Wiley

## See also

[`Association`](Association.md)

Other assoc.continuous: [`corPart()`](corPart.md),
[`corPolychor()`](corPolychor.md), [`findCorrX()`](findCorrX.md),
[`hoeffdingD()`](hoeffdingD.md), [`keepSig()`](keepSig.md),
[`pearsonCor()`](pearsonCor.md)

## Examples

``` r

# Example from SAS documentation (PROC FREQ)
pain <- as.table(matrix(c(26,  6, 26, 7, 23, 
                           9, 18, 14, 9, 23), 
                           ncol=5, 
        dimnames=list(adverse=c("no", "yes"), dose=1:5)))

spearmanCor(pain)
#> [1] 0.3770609

spearmanCor(pain, conf.level=0.95)
#>       est       lci       uci 
#> 0.3770609 0.2361593 0.5024329 
  
# must be the same as
with(lapply(
       bedrock::untable(pain, 
                        colnames = c("adverse","dose")), 
       ordered), 
     spearmanCor(adverse, dose, conf.level=0.95))
#>       est       lci       uci 
#> 0.3770609 0.2361593 0.5024329 
```
