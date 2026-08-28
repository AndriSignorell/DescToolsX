# (Robust) Scaling and Centering

Centers and scales the columns of a numeric matrix. The conventional
version uses mean and standard deviation, the robust one median and MAD
(see Details).

## Usage

``` r
scaleX(x, center = TRUE, scale = TRUE, robust = FALSE, na.rm = TRUE)
```

## Arguments

- x:

  a numeric matrix-like object

- center:

  logical scalar or numeric vector. If `TRUE`, the column means (or
  medians, for `robust = TRUE`) are subtracted; if `FALSE`, no centering
  is performed. Alternatively, a numeric vector of length `ncol(x)`
  supplies the values to subtract directly.

- scale:

  logical scalar or numeric vector. If `TRUE`, the columns are divided
  by their standard deviation (or MAD, for `robust = TRUE`); if `FALSE`,
  no scaling is performed. Alternatively, a numeric vector of length
  `ncol(x)` supplies the divisors directly.

- robust:

  logical; whether to standardize by median and MAD rather than by mean
  and standard deviation

- na.rm:

  logical; if `TRUE` (default), missing values are omitted when the
  column centers and scales are computed. Ignored for whichever of
  `center` and `scale` is given as a numeric vector. Missing entries of
  `x` itself always remain missing in the result.

## Value

the centered and scaled matrix. The numeric centerings and scalings used
(if any) are returned as attributes `"scaled:center"` and
`"scaled:scale"`.

## Details

The R base function [`scale`](https://rdrr.io/r/base/scale.html) centers
each column by its mean and divides by the root mean square of the
centered column, which for centered data is the standard deviation. Both
are sensitive to outliers: a single extreme value moves the mean and
inflates the standard deviation, so the remaining observations are
compressed towards zero.

If `robust` is set to `TRUE` the column median takes the place of the
mean and the median absolute deviation
([`mad`](https://rdrr.io/r/stats/mad.html)) that of the standard
deviation. Both have a breakdown point of 50 percent, so the
standardization reflects the bulk of the data rather than its extremes,
and genuine outliers keep large scores instead of being pulled in.

Whichever is chosen, `center` and `scale` accept either a logical flag
or a numeric vector of values to use directly, in which case that vector
must have one entry per column of `x`, as in
[`scale`](https://rdrr.io/r/base/scale.html).

The two versions differ in one further respect. The MAD is invariant to
location shifts, so for `robust = TRUE` the returned `"scaled:scale"` is
the same whether or not the columns were centered first, and does not
depend on `center`. The root mean square is not invariant; for
`robust = FALSE` it is computed after centering, matching
[`scale`](https://rdrr.io/r/base/scale.html), which is what makes it
equal the standard deviation when `center` is `TRUE` and not otherwise.

A zero or non-finite scaling factor can produce undefined or non-finite
results. `scaleX` emits a warning naming the affected columns rather
than failing, since the result may still be usable when those columns
are subsequently dropped.

## See also

[`scale`](https://rdrr.io/r/base/scale.html),
[`sweep`](https://rdrr.io/r/base/sweep.html),
[`mad`](https://rdrr.io/r/stats/mad.html), [`rangeX`](rangeX.md)

Other transform: [`boxCox()`](boxCox.md),
[`boxCoxLambda()`](boxCoxLambda.md), [`logSt()`](logSt.md),
[`yeoJohnson()`](yeoJohnson.md)

## Examples

``` r
x <- bedrock::Pizza$temperature

# robust standardization is far less affected by the extreme values
plot(scaleX(x, robust = TRUE), col = "black", pch = 16, cex = 0.4,
     ylab = "standardized temperature")
points(scaleX(x), col = "red", pch = 16, cex = 0.4)
legend("topright", legend = c("robust (median/MAD)", "conventional"),
       col = c("black", "red"), pch = 16, bty = "n")


# the centerings and scalings used are recoverable
z <- scaleX(cbind(a = c(1, 2, 3, 4, 100), b = c(10, 20, 30, 40, 50)))
attr(z, "scaled:center")
#>  a  b 
#> 22 30 
attr(z, "scaled:scale")
#>        a        b 
#> 43.61766 15.81139 

# compared to the robust version, which the extreme value barely moves
attr(scaleX(cbind(a = c(1, 2, 3, 4, 100)), robust = TRUE), "scaled:scale")
#>      a 
#> 1.4826 

# supplying the values directly, as base::scale allows
scaleX(matrix(1:6, ncol = 2), center = c(0, 0), scale = c(1, 2))
#>      [,1] [,2]
#> [1,]    1  2.0
#> [2,]    2  2.5
#> [3,]    3  3.0
#> attr(,"scaled:center")
#> [1] 0 0
#> attr(,"scaled:scale")
#> [1] 1 2
```
