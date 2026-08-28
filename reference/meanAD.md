# Mean Absolute Deviation From a Center Point

Calculates the mean absolute deviation from a center point, typically
the sample mean or the median.

## Usage

``` r
meanAD(x, weights = NULL, center = meanX, na.rm = FALSE)
```

## Arguments

- x:

  a vector containing the observations

- weights:

  a numerical vector of weights the same length as `x` giving the
  weights to use for elements of `x`

- center:

  a numeric center or a function applied to `x`. User-defined functions
  must accept `weights` when weights are supplied. Defaults to
  [`meanX()`](meanX.md).

- na.rm:

  logical; whether to remove missing values. Defaults to `FALSE`.

## Value

a numeric scalar containing the mean absolute deviation

## Details

The `meanAD` function calculates the mean absolute deviation from the
mean value (or from another supplied center point) of x, after having
removed `NA` values (if requested): \$\$\frac{1}{n} \cdot
\sum\_{i=1}^{n}\left \| x\_{i}-c \right \| \\ \\ \\ \textup{where} \\
c=mean(x) \\ \textup{or} \\ c=med(x)\$\$  
The function supports the use of weights. The default function for the
center value [`meanX()`](meanX.md) has a weights arguments, too. If a
user defined function is used it must be assured that it has a weights
argument.

## Note

Rewritten following an idea of Danielle Navarro (`aad` in the lsr
package).

## See also

[`mad`](https://rdrr.io/r/stats/mad.html)

Other dispersion: [`coefVar()`](coefVar.md), [`iqrX()`](iqrX.md),
[`madX()`](madX.md), [`meanSE()`](meanSE.md), [`rangeX()`](rangeX.md),
[`varX()`](varX.md)

## Examples

``` r

x <- runif(100)
meanAD(x)
#> [1] 0.2136102

speed <- c(58, 88, 40, 60, 72, 66, 80, 48, NA)
meanAD(speed)
#> [1] NA
meanAD(speed, na.rm=TRUE)
#> [1] 12.5


# using the median as centerpoint
x <- c(2,3,5,3,1,15,23)

meanAD(x, center=mean)
#> [1] 6.612245
meanAD(x, center=median)
#> [1] 5.285714

# define a fixed center
meanAD(x, center=4)
#> [1] 5.428571

# use of weights
meanAD(x=0:6, weights=c(21,46,54,40,24,10,5))
#> [1] 1.1825
```
