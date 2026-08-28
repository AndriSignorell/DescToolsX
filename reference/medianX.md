# (Weighted) Median Value

Compute the sample median. The function basically wraps the function
[`quantileX()`](quantileX.md), which offers the option to define
weights.  
For grouped data the median can be estimated by linear interpolation
within the class containing the median, which is implemented in the
interface for `Freq`-objects.

## Usage

``` r
medianX(x, ...)

# Default S3 method
medianX(x, weights = NULL, na.rm = FALSE, ...)

# S3 method for class 'factor'
medianX(x, na.rm = FALSE, ...)

# S3 method for class 'Freq'
medianX(x, breaks, ...)
```

## Arguments

- x:

  an object for which a method has been defined, or a numeric vector
  containing the values whose median is to be computed

- ...:

  further arguments passed to or from other methods

- weights:

  a numerical vector of weights the same length as `x` giving the
  weights to use for elements of `x`

- na.rm:

  a logical value indicating whether `NA` values should be stripped
  before the computation proceeds

- breaks:

  breaks for calculating the mean for classified data as composed by
  [`freq`](freq.md)

## Value

the default method returns a length-one object of the same type as `x`,
except when `x` is integer of even length, when the result will be
double.

If there are no values or if `na.rm = FALSE` and there are `NA` values
the result is `NA` of the same type as `x` (or more generally the result
of `x[FALSE][NA]`).

## Details

This is a generic function for which methods can be written. However,
the default method makes use of `is.na`, `sort` and `mean` from package
base all of which are generic, and so the default method will work for
most classes (e.g., `"`[`Date`](https://rdrr.io/r/base/Dates.html)`"`)
for which a median is a reasonable concept.

Calculating the median for ordered factors is not implemented in
standard R, as it's not well defined (it is not clear what to do if the
median sits between two levels in factors of even length). This function
returns the high median and prints a warning if the low median would be
different (which is supposed to be a rare event). There's a vivid
discussion between experts going on whether this should be defined or
not. We'll wait for definitive results and enjoy the function's comfort
so far...

## Note

There are alternative approaches for calculating weighted median (e.g.
`matrixstats::weightedMedian`).

## References

Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) *The New S
Language*. Wadsworth & Brooks/Cole.

## See also

[`quantile`](https://rdrr.io/r/stats/quantile.html) for general
quantiles,  
[Maechler on R-help mailing list,
2003-Nov](https://stat.ethz.ch/pipermail/r-help/2003-November/042684.html)  
[StackOverflow discussion on
median](https://stackoverflow.com/questions/7925102/idiomatic-method-of-finding-the-median-of-an-ordinal)

Other location: [`gmean()`](gmean.md), [`hmean()`](hmean.md),
[`hodgesLehmann()`](hodgesLehmann.md), [`huberM()`](huberM.md),
[`meanX()`](meanX.md), [`modeX()`](modeX.md),
[`tukeyBiweight()`](tukeyBiweight.md)

## Examples

``` r

medianX(1:4)                # = 2.5 [even number]
#> [1] 2.5
medianX(c(1:3, 100, 1000))  # = 3 [odd, robust]
#> [1] 3

# Approximation for classified data
breaks <- seq(10,70, 10)
medianX(
  freq(cut(Pizza$temperature, breaks=breaks)),
  breaks=breaks)
#> [1] 49.91404

# compared to
medianX(Pizza$temperature)
#> [1] NA

# starting from a classified table
# from     to  income
#    0   4000      20
# 4000   6000      42
# 6000   8000      31
# 8000  10000      12

# freq(as.table(c(20,42,31,12)))
#    level  freq   perc  cumfreq  cumperc
# 1      A    20  19.0%       20    19.0%
# 2      B    42  40.0%       62    59.0%
# 3      C    31  29.5%       93    88.6%
# 4      D    12  11.4%      105   100.0%

medianX(freq(as.table(c(20,42,31,12))), breaks=c(0,4000,6000,8000,10000))
#> [1] 5547.619

# use weights
x <- sample(20, 30, replace = TRUE)
z <- as.numeric(names(w <- table(x)))

(m1 <- medianX(z, weights=w))
#> [1] 8.5
(m2 <- medianX(x))
#> [1] 8.5
stopifnot(identical(m1, m2))

```
