# Create a Factor Variable Using the Quantiles of a Continuous Variable

Creates a factor variable using the quantiles of a continuous variable.

## Usage

``` r
cutQ(
  x,
  breaks = quantile(x, seq(0, 1, by = 0.25), na.rm = TRUE),
  labels = NULL,
  na.rm = FALSE,
  ...
)
```

## Arguments

- x:

  continuous variable

- breaks:

  cut points used to create groups. By default, quartiles are used. See
  [`quantile`](https://rdrr.io/r/stats/quantile.html) for details. A
  single integer specifies the intended number of groups; for example,
  `breaks = 10` creates deciles.

- labels:

  labels for the levels of the resulting category. By default, labels
  are defined as `Q1`, `Q2`, and so on. The argument is passed to
  [`cut`](https://rdrr.io/r/base/cut.html), so `labels = FALSE` returns
  integer codes instead of a factor. When quantiles are tied, the levels
  are built from the observed interval bounds instead and `labels` is
  ignored - a warning is issued in that case.

- na.rm:

  logical. Should missing values be removed before the quantiles are
  computed? Defaults to `FALSE`. Note that the quantiles themselves are
  always taken with `na.rm = TRUE`; this argument decides whether the
  missing values are dropped from the returned factor as well.

- ...:

  optional arguments passed to [`cut`](https://rdrr.io/r/base/cut.html)

## Value

a factor with one level for each quantile interval, or an integer vector
when `labels = FALSE`

## Details

This function uses [`quantile`](https://rdrr.io/r/stats/quantile.html)
to obtain the specified quantiles of `x`, then calls
[`cut`](https://rdrr.io/r/base/cut.html) to create a factor variable
using the intervals specified by these quantiles.

It properly handles cases where more than one quantile obtains the same
value, as in the second example below. Note that in this case, there
will be fewer generated factor levels than the specified number of
quantile intervals.

## Note

Based on code by Gregory R. Warnes, adapted to conform to package
standards.

## See also

[`cut`](https://rdrr.io/r/base/cut.html),
[`quantile`](https://rdrr.io/r/stats/quantile.html)

Other cut: [`cut.integer()`](cut.integer.md), [`cutAge()`](cutAge.md)

## Examples

``` r

# create example data
set.seed(1234)
x <- rnorm(1000)

# cut into quartiles
quartiles <- cutQ(x)
table(quartiles)
#> quartiles
#>  Q1  Q2  Q3  Q4 
#> 250 250 250 250 

# cut into deciles
deciles <- cutQ(x, breaks=10, labels=NULL)
table(deciles)
#> deciles
#>  Q1  Q2  Q3  Q4  Q5  Q6  Q7  Q8  Q9 Q10 
#> 100 100 100 100 100 100 100 100 100 100 

# show handling of 'tied' quantiles.
x <- round(x)  # discretize to create ties
stem(x)        # display the ties
#> 
#>   The decimal point is at the |
#> 
#>   -3 | 0000000000000
#>   -2 | 
#>   -2 | 000000000000000000000000000000000000000000000000
#>   -1 | 
#>   -1 | 00000000000000000000000000000000000000000000000000000000000000000000+181
#>   -0 | 
#>   -0 | 
#>    0 | 00000000000000000000000000000000000000000000000000000000000000000000+310
#>    0 | 
#>    1 | 00000000000000000000000000000000000000000000000000000000000000000000+140
#>    1 | 
#>    2 | 000000000000000000000000000000000000000000000000000000000000000
#>    2 | 
#>    3 | 00000
#> 
deciles <- cutQ(x, breaks=10)

table(deciles) # note that there are only 5 groups (not 10) 
#> deciles
#> [-3,-1)      -1       0       1   (1,3] 
#>      61     261     390     220      68 
               # due to duplicates
```
