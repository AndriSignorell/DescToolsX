# Mode (most Frequent Value(s))

Calculate the mode, the most frequent value, of a numeric or character
vector x.

## Usage

``` r
modeX(x, na.rm = FALSE)
```

## Arguments

- x:

  a non-empty numeric vector of data values

- na.rm:

  logical. Should missing values be removed? Defaults to `FALSE`.

## Value

the most frequent value as a number or character, depending on
`class(x)`. If there is more than one, all are returned in a vector.  
The modal frequency is attached as attribute named `"freq"`.

## Details

The mode is mainly useful for qualitative data, sometimes still for
integer vectors.

For numeric vectors, the interest lies less in central tendency than in
conspicuous accumulation points, which can indicate data errors.
[`desc()`](Desc.md) therefore reports it within the numeric description
once the frequency of a single value exceeds a threshold, since ties are
generally unexpected in numeric data unless the generating process
explains them.

`NA` handling follows the package standard: a single `NA` yields `NA`.
This is conservative, as the mode is sometimes determined unambiguously
despite missing values. For `x = c(1,1,1,1,2,2,NA)` the mode is 1
whatever the missing value is, and the modal frequency lies between 4
and 5. Exploiting this is left to the user.

The mode is elsewhere often obtained by tabulating every element and
returning the most frequent. This function uses a dedicated C++ data
structure and determines only the most frequent element, making it
orders of magnitude faster, especially for large numeric vectors with
many distinct values.

**Note:** ` `There are other approaches for determining the mode, e.g.
one might use  
`density(x)$x[which.max(density(x)$y)]`  
for quantitative data, resp.
[`hist()`](https://rdrr.io/r/graphics/hist.html).  
Another interesting idea for a more robust estimation of the mode:

     peak <- optimize(function(x, model)
      predict(model, data.frame(x = x)),
        c(min(x), max(x)), maximum = TRUE, model = y.loess)
      points(peak$maximum, peak$objective)

## Note

Great Rcpp part contributed by Joseph Wood and Ralf Stubner.

## References

[rcpp-fast-statistical-mode](https://stackoverflow.com/questions/55212746/rcpp-fast-statistical-mode-function-with-vector-input-of-any-type/)

## See also

Other location: [`gmean()`](gmean.md), [`hmean()`](hmean.md),
[`hodgesLehmann()`](hodgesLehmann.md), [`huberM()`](huberM.md),
[`meanX()`](meanX.md), [`medianX()`](medianX.md),
[`tukeyBiweight()`](tukeyBiweight.md)

## Examples

``` r

# normal mode
modeX(c(0:5, 5))
#> [1] 5
#> attr(,"freq")
#> [1] 2

modeX(5)
#> [1] NA
#> attr(,"freq")
#> [1] NA
modeX(NA)
#> [1] NA
#> attr(,"freq")
#> [1] NA
modeX(c(NA, NA))
#> [1] NA
#> attr(,"freq")
#> [1] NA
modeX(c(NA, 0:5))
#> [1] NA
#> attr(,"freq")
#> [1] NA
modeX(c(NA, 0:5), na.rm=TRUE)
#> [1] NA
#> attr(,"freq")
#> [1] NA
modeX(c(NA, 0:5, 5), na.rm=TRUE)
#> [1] 5
#> attr(,"freq")
#> [1] 2

# returns all encountered modes, if several exist
modeX(c(0:5, 4, 5, 6))
#> [1] 4 5
#> attr(,"freq")
#> [1] 2

modeX(Pizza$driver)
#> [1] NA
#> attr(,"freq")
#> [1] NA
modeX(Pizza$driver, na.rm=TRUE)
#> [1] Carpenter
#> attr(,"freq")
#> [1] 272
#> Levels: Butcher Carpenter Carter Farmer Hunter Miller Taylor
modeX(as.character(Pizza$driver), na.rm=TRUE)
#> [1] "Carpenter"
#> attr(,"freq")
#> [1] 272

# use sapply for evaluating data.frames (resp. apply for matrices)
sapply(Pizza[,c("driver", "temperature", "date")], modeX, na.rm=TRUE)
#>      driver temperature        date 
#>         2.0        51.3     16137.0 

```
