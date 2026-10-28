# Skewness

`skew` computes the skewness, [kurt](kurt.md) the excess kurtosis of the
values in x.

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
  Wald interval based on the asymptotic standard error. See Details and
  [`ConfidenceIntervals()`](ConfidenceIntervals.md).

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

  skewness estimate

- `lci`:

  lower confidence interval bound

- `uci`:

  upper confidence interval bound

## Details

If `na.rm` is `TRUE` then missing values in `x` and in `weights` are
removed, pairwise, before computation proceeds.  

The estimator for calculating the skewness can either be:  
`1: g_1 = m_3 / m_2^(3/2) `  
`2: G_1 = g_1 * sqrt(n(n-1)) / (n-2) `  
`3: b_1 = m_3 / s^3 = g_1 ((n-1)/n)^(3/2) `  

1 is the typical definition used in Stata and in many older textbooks.  
2 is used in SAS and SPSS.  
3 is used in MINITAB and BMDP.  

Estimators 1 and 3 need \\n \ge 2\\, estimator 2 needs \\n \ge 3\\. For
shorter or constant input the skewness is not defined: the result is
`NA` (all three elements if an interval was requested) with a warning.

Cramer (1997) mentions the asymptotic standard error of `G_1`, that is
of `estimator = 2`:

    ASE.skew = sqrt( 6*n*(n-1)/((n-2)*(n+1)*(n+3)) )

to be used for calculating the confidence intervals. The standard errors
of the other two estimators follow from it by the same factors that
relate the estimators themselves, so that `est/se` does not depend on
the choice of `estimator`. This is implemented here with
`method="classic"`. The standard error is defined from \\n = 3\\ on;
below, the interval is `NA` with a warning.  
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
weights `skew(x, weights = w)` equals `skew(rep(x, w))`. An observation
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

[kurt](kurt.md), [meanX](meanX.md), [sdX](varX.md), similar code in
e1071

Other shape: [`kurt()`](kurt.md)

## Examples

``` r

skew(bedrock::Pizza$price, na.rm=TRUE)
#> [1] 0.4970801

# use sapply to calculate skewness for a data.frame
sapply(bedrock::Pizza[,c("temperature","price","delivery_min")],
       skew, na.rm=TRUE)
#>  temperature        price delivery_min 
#>   -0.8418683    0.4970801    0.6106322 

# the estimate lies inside its own confidence interval
set.seed(1)
skew(rlnorm(50), conf.level=0.95, method="classic")
#>       est       lci       uci 
#> 1.2578214 0.6371518 1.8784909 

# frequency weights are the same as repeating the values
skew(c(1, 2, 3, 10), weights = c(3, 1, 2, 1))
#> [1] 1.367629
skew(rep(c(1, 2, 3, 10), c(3, 1, 2, 1)))
#> [1] 1.367629
```
