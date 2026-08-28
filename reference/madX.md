# Median Absolute Deviation

Compute the median absolute deviation, i.e., the (lo-/hi-) median of the
absolute deviations from the median, and (by default) adjust by a factor
for asymptotically normal consistency. This function wraps the specific
base R function [`mad`](https://rdrr.io/r/stats/mad.html) and extends it
for the use of weights.

## Usage

``` r
madX(
  x,
  weights = NULL,
  center = medianX,
  constant = 1.4826,
  medianType = c("standard", "low", "high"),
  na.rm = FALSE
)
```

## Arguments

- x:

  a numeric vector

- weights:

  a numerical vector of weights the same length as `x` giving the
  weights to use for elements of `x`

- center:

  a numeric center or a function applied to `x`. When weights are
  supplied, the function must support a `weights` argument. Defaults to
  `medianX`.

- constant:

  scale factor (default is `1.4826`)

- medianType:

  character string selecting the `"standard"`, `"low"`, or `"high"`
  median for even sample sizes

- na.rm:

  if `TRUE` then `NA` values are stripped from `x` before computation
  takes place

## Value

a numeric scalar containing the scaled median absolute deviation

## Details

The actual value calculated is `constant * cMedian(abs(x - center))`
with the default value of `center` being `median(x)`, and `cMedian`
being the usual, the ‘low’ or ‘high’ median, see the arguments
description for `low` and `high` above.

The default `constant = 1.4826` (approximately \\1/\Phi^{-1}(\frac 3
4)\\ = `1/qnorm(3/4)`) ensures consistency, i.e.,
\$\$E\[mad(X_1,\dots,X_n)\] = \sigma\$\$ for \\X_i\\ distributed as
\\N(\mu, \sigma^2)\\ and large \\n\\.

If `na.rm` is `TRUE` then `NA` values are stripped from `x` before
computation takes place. If this is not done then an `NA` value in `x`
will cause `madX` to return `NA`.

Confidence intervals are provided by
[`lumen::madCI()`](https://andrisignorell.github.io/lumen/reference/mad-confints.html).

## See also

[`IQR`](https://rdrr.io/r/stats/IQR.html) which is simpler but less
robust, [`iqrX`](iqrX.md) for the same using weights,  
[`mad`](https://rdrr.io/r/stats/mad.html),
[`median`](https://rdrr.io/r/stats/median.html),
[`var`](https://rdrr.io/r/stats/cor.html) the base R equivalents  
[`madCI`](https://andrisignorell.github.io/lumen/reference/mad-confints.html)
(confidence intervals).

Other dispersion: [`coefVar()`](coefVar.md), [`iqrX()`](iqrX.md),
[`meanAD()`](meanAD.md), [`meanSE()`](meanSE.md),
[`rangeX()`](rangeX.md), [`varX()`](varX.md)

## Examples

``` r

madX(c(1:9))
#> [1] 2.9652
print(madX(c(1:9),     constant = 1)) ==
      madX(c(1:8, 100), constant = 1)       # = 2 ; TRUE
#> [1] 2
#> [1] TRUE
x <- c(1,2,3,5,7,8)
sort(abs(x - median(x)))
#> [1] 1 1 2 3 3 4
c(madX(x, constant = 1, medianType="standard"),
  madX(x, constant = 1, medianType="low"),
  madX(x, constant = 1, medianType="high"))
#> [1] 2.5 2.0 3.0

# use weights
x <- sample(20, 30, replace = TRUE)
z <- as.numeric(names(w <- table(x)))

(m1 <- madX(z, weights=w))
#> [1] 6.6717
(m2 <- madX(x))
#> [1] 6.6717
stopifnot(identical(m1, m2))

```
