# Skewness

`skew` computes the skewness, `kurt` the excess kurtosis of the values
in x.

## Usage

``` r
skew(
  x,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  method = c("boot", "classic"),
  estimator = 3,
  weights = NULL,
  na.rm = FALSE,
  ...
)
```

## Arguments

- x:

  a numeric vector

- conf.level:

  confidence level of the interval. If set to `NA` (the default), only
  the point estimate is returned.

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See
  [`ConfidenceIntervals`](ConfidenceIntervals.md).

- method:

  character string specifying the confidence interval method. `"boot"`
  (default) uses a nonparametric bootstrap, with BCa intervals unless
  another bootstrap type is supplied through `...`; `"classic"` uses a
  Wald interval based on the asymptotic standard error. See Details and
  [`ConfidenceIntervals`](ConfidenceIntervals.md).

- estimator:

  integer, either 1, 2 or 3 (default) defining the algorithm used for
  calculation. See Details.

- weights:

  a numerical vector of weights the same length as `x` giving the
  weights to use for elements of `x`. The weights are read as
  frequencies, so that their sum takes the place of the sample size in
  the estimator's bias corrections and in the standard error.

- na.rm:

  logical, indicating whether `NA` values should be stripped before the
  computation proceeds. Defaults to `FALSE`.

- ...:

  further arguments passed to
  [`boot`](https://rdrr.io/pkg/boot/man/boot.html) when confidence
  intervals are calculated

## Value

if `conf.level = NA`, a numeric scalar. Otherwise a named numeric vector
with elements:

- `est`:

  skewness estimate

- `lci`:

  lower confidence interval bound

- `uci`:

  upper confidence interval bound

## Details

If `na.rm` is `TRUE` then missing values are removed before computation
proceeds.  

The estimator for calculating the skewness can either be:  
`1: g_1 = m_3 / m_2^(3/2) `  
`2: G_1 = g_1 * sqrt(n(n-1)) / (n-2) `  
`3: b_1 = m_3 / s^3 = g_1 ((n-1)/n)^(3/2) `  

1 is the typical definition used in Stata and in many older textbooks.  
2 is used in SAS and SPSS.  
3 is used in MINITAB and BMDP.  

Cramer (1997) mentions the asymptotic standard error of `G_1`, that is
of `estimator = 2`:

    ASE.skew = sqrt( 6*n*(n-1)/((n-2)*(n+1)*(n+3)) )

to be used for calculating the confidence intervals. The standard errors
of the other two estimators follow from it by the same factors that
relate the estimators themselves, so that `est/se` does not depend on
the choice of `estimator`. This is implemented here with
`method="classic"`.  
However, Joanes and Gill (1998) advise against this approach, pointing
out that the normal assumptions would virtually always be violated. They
suggest using the bootstrap method. That's why the default method for
the confidence interval type is set to `"boot"`. If not further
specified the boot ci type will be chosen as `"bca"`.  

The standard error is only defined for `n >= 3`; for shorter input the
variance, and with it any `method="classic"` interval, is `NA`.

This implementation of the two functions is comparably fast, as the
expensive sums are coded in C++.

## References

Cramer, D. (1997): *Basic Statistics for Social Research* Routledge.

Joanes, D. N., Gill, C. A. (1998): Comparing measures of sample skewness
and kurtosis. *The Statistician*, 47, 183-189.

## See also

[meanX](meanX.md), [sdX](varX.md), similar code in e1071

Other shape: [`kurt()`](kurt.md)

## Examples

``` r

skew(bedrock::Pizza$price, na.rm=TRUE)
#>       est 
#> 0.4970801 

# use sapply to calculate skewness for a data.frame
sapply(bedrock::Pizza[,c("temperature","price","delivery_min")], 
       skew, na.rm=TRUE)
#>  temperature.est        price.est delivery_min.est 
#>       -0.8418683        0.4970801        0.6106322 

# the estimate lies inside its own confidence interval
set.seed(1)
skew(rlnorm(50), conf.level=0.95, method="classic")
#>       est       lci       uci 
#> 1.2578214 0.6371518 1.8784909 

```
