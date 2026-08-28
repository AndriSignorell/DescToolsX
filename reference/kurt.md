# Kurtosis

`kurt()` returns the excess kurtosis, therefore the kurtosis calculates
as `kurt(x) + 3` if required.

## Usage

``` r
kurt(
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

  a numeric vector. An object that is not a vector is coerced by
  `as.vector` if possible.

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
  weights to use for elements of `x`

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

  kurtosis estimate

- `lci`:

  lower confidence interval bound

- `uci`:

  upper confidence interval bound

## Details

If `na.rm` is `TRUE` then missing values are removed before computation
proceeds.  

The estimator for calculating kurtosis can either be:  
`1: g_2 = m_4 / m_2^2 - 3 `  
`2: G_2 = ((n+1) g_2 + 6) * (n-1) / ((n-2)(n-3)) `  
`3: b_2 = m_4 / s^4 - 3 = (g_2 + 3) (1 - 1/n)^2 - 3 `  

1 is the typical definition used in Stata and in many older textbooks.  
2 is used in SAS and SPSS.  
3 is used in MINITAB and BMDP.  

Cramer (1997) mentions the asymptotic standard error of the kurtosis:  

    ASE.kurt = sqrt((24*n*(n - 1)^2) / ((n - 3)*(n - 2)*(n + 3)*(n + 5)))

to be used for calculating the confidence intervals. This is implemented
here with `method="classic"`.  
However, Joanes and Gill (1998) advise against this approach, pointing
out that the normal assumptions would virtually always be violated. They
suggest using the bootstrap method. That's why the default method for
the confidence interval type is set to `"boot"`. If not further
specified the boot ci type will be chosen as `"bca"`.  

This implementation is comparably fast, as the expensive sums are coded
in C.

## Random number generation

`method = "boot"` - the default - resamples and therefore advances R's
global random number generator. Call
[`set.seed`](https://rdrr.io/r/base/Random.html) beforehand for
reproducible intervals.

## References

Cramer, D. (1997): *Basic Statistics for Social Research* Routledge.

Joanes, D. N., Gill, C. A. (1998): Comparing measures of sample skewness
and kurtosis. *The Statistician*, 47, 183-189.

## See also

[meanX](meanX.md), [sdX](varX.md), similar code in e1071

Other shape: [`skew()`](skew.md)

## Examples

``` r

kurt(Pizza$price, na.rm=TRUE)
#> [1] 0.1076097

# use sapply to calculate skewness for a data.frame
sapply(Pizza[,c("temperature","price","delivery_min")], kurt, na.rm=TRUE)
#>  temperature        price delivery_min 
#>   0.05058327   0.10760970   0.09541410 

# or apply to do that columnwise with a matrix
apply(as.matrix(Pizza[,c("temperature","price","delivery_min")]), 2, 
      kurt, na.rm=TRUE)
#>  temperature        price delivery_min 
#>   0.05058327   0.10760970   0.09541410 
```
