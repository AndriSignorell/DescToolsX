# (Robust) Range

Computes the width of a numeric vector, optionally after trimming, or a
robust range based on a trimmed mean and absolute deviations.

## Usage

``` r
rangeX(x, trim = NULL, robust = FALSE, na.rm = FALSE, ...)
```

## Arguments

- x:

  a numeric vector whose range width is to be computed. In robust mode,
  `Inf` and `-Inf` are excluded; see `na.rm` for missing values.

- trim:

  a single numeric trimming fraction between 0 and 0.5, or `NULL` (the
  default). `NULL` selects 0 when `robust = FALSE` and 0.2 when
  `robust = TRUE`. In conventional mode, this fraction is removed from
  each end of `x` before computing the extrema. In robust mode, it
  controls trimming of both the centre and the absolute deviations as
  described above; it does not directly specify the fraction of
  observations outside the returned bounds. A non-finite value in robust
  mode is replaced by 0.2.

- robust:

  a single logical value; `FALSE` (the default) returns the
  conventional, optionally trimmed range width. `TRUE` returns the width
  between the robust bounds described above.

- na.rm:

  a single logical value indicating whether `NA` and `NaN` values should
  be removed before computation. The default is `FALSE`; missing values
  then produce a missing range width and missing bounds. In robust mode,
  infinite values are excluded regardless of `na.rm`.

- ...:

  additional arguments passed to the internal helper `.robRange` when
  `robust = TRUE`. Currently, the supported argument is `fac`, a
  positive finite numeric scalar (default 3) multiplying the deviation
  scale. Larger values widen the robust bounds up to the observed finite
  extrema; smaller values narrow them. For example,
  `rangeX(x, robust = TRUE, fac = 2)` uses a multiplier of 2. This
  factor does not specify a confidence level or a guaranteed coverage
  probability. Arguments in `...` are ignored when `robust = FALSE`.

## Value

a numeric scalar containing the range width. The corresponding lower and
upper bounds are returned in the `"bounds"` attribute. Note that the
attribute is dropped by subsetting and by
[`as.vector()`](https://rdrr.io/r/base/vector.html), so read it before
computing on the result.

## Details

Unlike [`range()`](https://rdrr.io/r/base/range.html), which returns the
minimum and maximum, `rangeX()` returns their difference and stores the
endpoints in the `"bounds"` attribute.

With `robust = TRUE`, only finite observations are used after handling
missing values according to `na.rm`. Let `n` be their number and `m`
their mean after trimming a fraction `trim` from each tail. The absolute
deviations from `m` are sorted, and the smallest
`k = ceiling((1 - trim) * n)` deviations are retained. The scale `s` is
their sum divided by `k - 1`; it is not a variance or standard
deviation. The bounds are `max(m - fac * s, min(x))` and
`min(m + fac * s, max(x))`, with extrema taken over the finite
observations. Thus, `trim` acts on both tails when estimating the centre
but only on the upper tail of the absolute deviations when estimating
the scale.

If `k < 3` or the scaled deviation measure is zero, a warning is issued
and the ordinary range of the finite observations is returned. If no
finite observations remain, the robust computation stops with an error.

## Note

Robust range contributed by Werner Stahel.

## See also

[`range()`](https://rdrr.io/r/base/range.html),
[`min()`](https://rdrr.io/r/base/Extremes.html),
[`max()`](https://rdrr.io/r/base/Extremes.html)

Other dispersion: [`coefVar()`](coefVar.md), [`iqrX()`](iqrX.md),
[`madX()`](madX.md), [`meanAbsDev()`](meanAbsDev.md),
[`meanSE()`](meanSE.md), [`varX()`](varX.md)

## Examples

``` r

x <- c(0:10, 50)
xm <- rangeX(x)
c(xm, rangeX(x, trim = 0.10))
#> [1] 50  9

x <- c(rnorm(20), rnorm(3, 5, 20))
rangeX(x, robust = TRUE)
#> [1] 3.574194
#> attr(,"bounds")
#> [1] -1.904542  1.669652
rangeX(x, robust = TRUE, trim = 0.1, fac = 2)
#> [1] 3.180857
#> attr(,"bounds")
#> [1] -1.651108  1.529749
attr(rangeX(x, robust = TRUE), "bounds")
#> [1] -1.904542  1.669652

# compared to
rangeX(x)
#> [1] 71.43202
#> attr(,"bounds")
#> [1] -30.73876  40.69326
```
