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

  a numeric vector. A matrix or array contributes all its elements, as
  `as.vector(x)`.

- conf.level:

  confidence level of the interval. If set to `NA` (the default), only
  the point estimate is returned. One-sided intervals require
  `conf.level > 0.5`.

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See
  [`ConfidenceIntervals()`](ConfidenceIntervals.md).

- method:

  character string specifying the confidence interval method. `"boot"`
  (default) uses a nonparametric bootstrap, with BCa intervals unless
  another bootstrap type is supplied through `\dots`; `"classic"` uses a
  Wald interval based on the standard error under normality. See Details
  and [`ConfidenceIntervals()`](ConfidenceIntervals.md).

- estimator:

  integer, either 1, 2 or 3 (default) defining the algorithm used for
  calculation. See Details.

- weights:

  a non-negative numeric vector of frequency weights, the same length as
  `x`. See Details.

- na.rm:

  logical, indicating whether `NA` values should be stripped before the
  computation proceeds. Defaults to `FALSE`.

- ...:

  bootstrap settings for `method = "boot"`, and only these: `R` (number
  of replicates), `type` (one of `"bca"`, `"perc"`, `"basic"`, `"norm"`;
  `"stud"` is not supported, as no suitable variance estimate is
  implemented here), `parallel` and `ncpus` as in
  [`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html). Other
  arguments are an error.

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

If `na.rm` is `TRUE` then missing values in `x` and in `weights` are
removed, pairwise, before computation proceeds.  

The estimator for calculating kurtosis can either be:  
`1: g_2 = m_4 / m_2^2 - 3 `  
`2: G_2 = ((n+1) g_2 + 6) * (n-1) / ((n-2)(n-3)) `  
`3: b_2 = m_4 / s^4 - 3 = (g_2 + 3) (1 - 1/n)^2 - 3 `  

1 is the typical definition used in Stata and in many older textbooks.  
2 is used in SAS and SPSS; it is unbiased under normality, not in
general.  
3 is used in MINITAB and BMDP.  

Estimators 1 and 3 need \\n \ge 2\\, estimator 2 needs \\n \ge 4\\. For
shorter or constant input the kurtosis is not defined: the result is
`NA` (all three elements if an interval was requested) with a warning.

`method = "classic"` uses a Wald interval with the standard error of the
kurtosis under normality. For estimator 1 this is the exact standard
error of \\g_2\\

    SE(g_2) = sqrt(24*n*(n - 2)*(n - 3) / ((n + 1)^2*(n + 3)*(n + 5)))

and since estimators 2 and 3 are linear in \\g_2\\, their standard
errors follow by multiplying with the respective slope,
\\(n-1)(n+1)/((n-2)(n-3))\\ and \\((n-1)/n)^2\\. For estimator 2 this
gives the familiar formula of Cramer (1997), as reported by SPSS:

    SES = sqrt(6*n*(n - 1) / ((n - 2)*(n + 1)*(n + 3)))
    SEK = 2 * SES * sqrt((n^2 - 1) / ((n - 3)*(n + 5)))
        = sqrt(24*n*(n - 1)^2 / ((n - 3)*(n - 2)*(n + 3)*(n + 5)))

The standard error is defined from \\n = 4\\ on; below, the interval is
`NA` with a warning. The interval remains a Wald approximation even
though the variance is exact under normality.  
However, Joanes and Gill (1998) advise against this approach, pointing
out that the normal assumptions would virtually always be violated. They
suggest using the bootstrap method. That's why the default method for
the confidence interval type is set to `"boot"`. If not further
specified the boot ci type will be chosen as `"bca"`. If some bootstrap
replicates are undefined (a constant resample) or
[`boot::boot.ci()`](https://rdrr.io/pkg/boot/man/boot.ci.html) cannot
compute the interval, the bounds are `NA` with a warning.  

**Weights** are frequency weights: `sum(weights)` takes the place of
\\n\\ in the bias corrections and in the standard error. For integer
weights `kurt(x, weights = w)` equals `kurt(rep(x, w))`. An observation
with weight 0 is absent: its `x` may be `NA` or infinite without effect.
A missing weight makes the observation missing. The bootstrap resamples
the expanded sample and therefore requires integer weights (memory grows
with `sum(weights)`, and BCa's jackknife with it). Non-integer weights
are admitted for the point estimate and `method = "classic"` as a
computational extension of the same formulas; they correspond to no
actual sample, so the exact normal-theory variance does not strictly
apply to them. Analysis or survey weights are not supported: neither the
bias correction nor the row bootstrap would reflect them.

This implementation is comparably fast, as the expensive sums are coded
in C++. The data are rescaled before centring and again after, so that
neither the deviations nor their powers can overflow for any finite
input.

## Random number generation

`method = "boot"` - the default - resamples and therefore advances R's
global random number generator. Call
[`base::set.seed()`](https://rdrr.io/r/base/Random.html) beforehand for
reproducible intervals.

## References

Cramer, D. (1997): *Basic Statistics for Social Research* Routledge.

Joanes, D. N., Gill, C. A. (1998): Comparing measures of sample skewness
and kurtosis. *The Statistician*, 47, 183-189.

## See also

[skew](skew.md), [meanX](meanX.md), [sdX](varX.md), similar code in
e1071

Other shape: [`skew()`](skew.md)

## Examples

``` r

kurt(bedrock::Pizza$price, na.rm=TRUE)
#> [1] 0.1076097

# use sapply to calculate kurtosis for a data.frame
sapply(bedrock::Pizza[,c("temperature","price","delivery_min")],
       kurt, na.rm=TRUE)
#>  temperature        price delivery_min 
#>   0.05058327   0.10760970   0.09541410 

# or apply to do that columnwise with a matrix
apply(as.matrix(bedrock::Pizza[,c("temperature","price","delivery_min")]),
      2, kurt, na.rm=TRUE)
#>  temperature        price delivery_min 
#>   0.05058327   0.10760970   0.09541410 

# frequency weights are the same as repeating the values
kurt(c(1, 2, 3, 10), weights = c(3, 1, 2, 1))
#> [1] 0.2778356
kurt(rep(c(1, 2, 3, 10), c(3, 1, 2, 1)))
#> [1] 0.2778356
```
