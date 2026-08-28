# Confidence Intervals for Pearson Correlation

Find the confidence intervals for a specified correlation based on
Fisher's z-transformation.

## Usage

``` r
pearsonCor(
  x,
  y = NULL,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  scoresType = "table",
  na.rm = FALSE
)
```

## Arguments

- x:

  a numeric vector, matrix, or table

- y:

  `NULL` (default) or a vector with compatible dimensions to `x`. If `y`
  is supplied, `table(x, y, ...)` is calculated.

- conf.level:

  confidence level of the interval. If set to `NA` (the default), only
  the point estimate is returned.

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See
  [`ConfidenceIntervals`](ConfidenceIntervals.md).

- scoresType:

  score calculation method for table input

- na.rm:

  logical, default `FALSE` determining if complete cases should be
  respected

## Value

if `conf.level = NA`, a numeric scalar. Otherwise a named numeric vector
with elements:

- `est`:

  point estimate of Pearson's correlation coefficient

- `lci`:

  lower confidence interval bound

- `uci`:

  upper confidence interval bound

## Details

The sampling distribution of Pearson's r is not normal. Fisher developed
a transformation now called "Fisher's z-transformation" used for the
calculation of normal distributed confidence intervals.

## Note

Based on code by William Revelle, adapted to conform to package
standards.

## See also

[lumen::fisherZ](https://andrisignorell.github.io/lumen/reference/fisherZ.html),
[lumen::fisherZInv](https://andrisignorell.github.io/lumen/reference/fisherZ.html)

Other assoc.continuous: [`corPart()`](corPart.md),
[`corPolychor()`](corPolychor.md), [`findCorrX()`](findCorrX.md),
[`hoeffdingD()`](hoeffdingD.md), [`keepSig()`](keepSig.md),
[`spearmanCor()`](spearmanCor.md)

## Examples

``` r

with(swiss, pearsonCor(Fertility, Agriculture))
#> [1] 0.3530792
with(swiss, pearsonCor(Fertility, Agriculture, conf.level=0.95))
#>        est        lci        uci 
#> 0.35307918 0.07334947 0.58130587 

bedrock::pairApply(swiss, pearsonCor)
#>                   Fertility Agriculture Examination   Education   Catholic
#> Fertility         1.0000000  0.35307918  -0.6458827 -0.66378886  0.4636847
#> Agriculture       0.3530792  1.00000000  -0.6865422 -0.63952252  0.4010951
#> Examination      -0.6458827 -0.68654221   1.0000000  0.69841530 -0.5727418
#> Education        -0.6637889 -0.63952252   0.6984153  1.00000000 -0.1538589
#> Catholic          0.4636847  0.40109505  -0.5727418 -0.15385892  1.0000000
#> Infant.Mortality  0.4165560 -0.06085861  -0.1140216 -0.09932185  0.1754959
#>                  Infant.Mortality
#> Fertility              0.41655603
#> Agriculture           -0.06085861
#> Examination           -0.11402160
#> Education             -0.09932185
#> Catholic               0.17549591
#> Infant.Mortality       1.00000000

bedrock::pairApply(swiss, 
           function(x, y) fmCI(pearsonCor(x, y, conf.level=0.95), 
                               digits=3, leadDigits=0))
#>                  Fertility              Agriculture           
#> Fertility        "1.000 [1.000, 1.000]" ".353 [.073, .581]"   
#> Agriculture      ".353 [.073, .581]"    "1.000 [1.000, 1.000]"
#> Examination      "-.646 [-.787, -.440]" "-.687 [-.813, -.497]"
#> Education        "-.664 [-.799, -.465]" "-.640 [-.783, -.432]"
#> Catholic         ".464 [.204, .663]"    ".401 [.129, .617]"   
#> Infant.Mortality ".417 [.147, .629]"    "-.061 [-.342, .230]" 
#>                  Examination            Education             
#> Fertility        "-.646 [-.787, -.440]" "-.664 [-.799, -.465]"
#> Agriculture      "-.687 [-.813, -.497]" "-.640 [-.783, -.432]"
#> Examination      "1.000 [1.000, 1.000]" ".698 [.514, .821]"   
#> Education        ".698 [.514, .821]"    "1.000 [1.000, 1.000]"
#> Catholic         "-.573 [-.738, -.342]" "-.154 [-.422, .139]" 
#> Infant.Mortality "-.114 [-.388, .179]"  "-.099 [-.376, .193]" 
#>                  Catholic               Infant.Mortality      
#> Fertility        ".464 [.204, .663]"    ".417 [.147, .629]"   
#> Agriculture      ".401 [.129, .617]"    "-.061 [-.342, .230]" 
#> Examination      "-.573 [-.738, -.342]" "-.114 [-.388, .179]" 
#> Education        "-.154 [-.422, .139]"  "-.099 [-.376, .193]" 
#> Catholic         "1.000 [1.000, 1.000]" ".175 [-.118, .440]"  
#> Infant.Mortality ".175 [-.118, .440]"   "1.000 [1.000, 1.000]"
```
