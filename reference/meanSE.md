# Standard Error of Mean

Calculates the standard error of mean.

## Usage

``` r
meanSE(x, sd = NULL, na.rm = FALSE)
```

## Arguments

- x:

  a non-empty numeric vector of data values

- sd:

  the standard deviation of `x`. If supplied, it is interpreted as the
  population standard deviation. If `NULL` (default), the sample
  standard deviation `sd(x)` is used.

- na.rm:

  logical. Should missing values be removed? Defaults to `FALSE`.

## Value

the standard error as a numeric scalar

## Details

`meanSE()` calculates the standard error of the mean defined as:
\$\$\frac{\sigma}{\sqrt{n}}\$\$ \\\sigma\\ being standard deviation of
`x` and n the length of `x`.

## See also

[lumen::meanCI](https://andrisignorell.github.io/lumen/reference/meanCI.html)

Other dispersion: [`coefVar()`](coefVar.md), [`iqrX()`](iqrX.md),
[`madX()`](madX.md), [`meanAD()`](meanAD.md), [`rangeX()`](rangeX.md),
[`varX()`](varX.md)

## Examples

``` r

meanSE(Pizza$price, na.rm=TRUE)
#> [1] 0.6252242

# evaluate data.frame
sapply(Pizza[, 1:4], meanSE, na.rm=TRUE)
#>       index        date        week     weekday 
#> 10.04158022  0.26218843  0.03873807  0.05886462 

```
