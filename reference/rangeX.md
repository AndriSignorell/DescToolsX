# (Robust) Range

Determines the range of the data, which can possibly be trimmed before
calculating the extreme values. The robust range version is calculated
on the basis of the trimmed mean and variance (see Details).

## Usage

``` r
rangeX(x, trim = NULL, robust = FALSE, na.rm = FALSE, ...)
```

## Arguments

- x:

  a numeric vector

- trim:

  the fraction (0 to 0.5) of observations to be trimmed from each end of
  `x` before the range is computed. Values of `trim` outside that range
  are taken as the nearest endpoint. Default is 0 for `robust = FALSE`
  and 0.2 for `robust = TRUE`.

- robust:

  logical; whether to return the robust or conventional range

- na.rm:

  a logical value indicating whether `NA` values should be stripped
  before the computation proceeds

- ...:

  further arguments passed to `.robRange`, including `fac`. Only used if
  `robust = TRUE`.

## Value

a numeric scalar containing the range width. The corresponding lower and
upper bounds are returned in the `"bounds"` attribute. Note that the
attribute is dropped by subsetting and by
[`as.vector`](https://rdrr.io/r/base/vector.html), so read it before
computing on the result.

## Details

The R base function range returns the minimum and maximum value of a
numeric object. Here we return the span of a (possibly trimmed) numeric
vector, say the difference of maximum and minimum value.

If robust is set to `TRUE` the function determines the trimmed mean m
and then the "upper trimmed mean" s of absolute deviations from m,
multiplied by `fac` (fac is 3 by default). The robust minimum is then
defined as m-fac\*s or min(x), whichever is larger, and similarly for
the maximum.

## Note

Robust range contributed by Werner Stahel.

## See also

[`range`](https://rdrr.io/r/base/range.html),
[`min`](https://rdrr.io/r/base/Extremes.html),
[`max`](https://rdrr.io/r/base/Extremes.html)

Other dispersion: [`coefVar()`](coefVar.md), [`iqrX()`](iqrX.md),
[`madX()`](madX.md), [`meanAD()`](meanAD.md), [`meanSE()`](meanSE.md),
[`varX()`](varX.md)

## Examples

``` r

x <- c(0:10, 50)
xm <- rangeX(x)
c(xm, rangeX(x, trim = 0.10))
#> [1] 50  9

x <- c(rnorm(20), rnorm(3, 5, 20))
rangeX(x, robust=TRUE)
#> [1] 3.546073
#> attr(,"bounds")
#> [1] -1.539950  2.006123

# compared to
rangeX(x)
#> [1] 22.62374
#> attr(,"bounds")
#> [1] -1.53995 21.08379
```
