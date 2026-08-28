# Kth Smallest/Largest Values

Find the kth smallest, resp. largest values from a vector `x` and return
the values and their frequencies.

## Usage

``` r
large(x, k = 5L, unique = FALSE, na.last = NA)

small(x, k = 5L, unique = FALSE, na.last = NA)

highLow(x, nlow = 5L, nhigh = nlow, na.last = NA)
```

## Arguments

- x:

  a numeric vector

- k:

  a positive integer defining how many extreme values are returned.
  Default is `k = 5`. If `k > length(x)`, all values will be returned.

- unique:

  logical, defining if unique values should be considered or not. If
  this is set to `TRUE`, a list with the `k` extreme values and their
  frequencies is returned. Default is `FALSE` (as unique is a rather
  expensive function).

- na.last:

  for controlling the treatment of `NA`s. If `TRUE`, missing values in
  the data are put last; if `FALSE`, they are put first; if `NA`, they
  are removed.

- nlow:

  number of smallest values included in the formatted output; defaults
  to 5

- nhigh:

  number of largest values included in the formatted output; defaults to
  `nlow`

## Value

for `large()` and `small()`, a vector of extreme values when
`unique = FALSE`, otherwise a list with components:

- `value`:

  extreme values

- `frequency`:

  corresponding frequencies

`highLow()` returns a character scalar containing formatted lowest and
highest values with frequencies.

## Details

This does not seem to be a difficult problem at first sight. We could
simply tabulate and sort the vector and finally take the first or last k
values. However sorting and tabulating the whole vector when we're just
interested in the few smallest values is a considerable waste of
resources. This approach becomes already impracticable for medium vector
lengths (~10⁵). There are several points and solutions of this problem
discussed out there. The present implementation is based on highly
efficient C++ code and proved to be very fast.

`highLow()` combines the two upper functions and reports the k extreme
values on both sides together with their frequencies in parentheses. It
is used for describing univariate variables and is interesting for
checking the ends of the vector, where in real data often wrong values
accumulate. In essence this is nothing more than a combine printing
routine for `large()` and `small()`.

## Note

Based on C++ code by Nathan Russell and Romain Francois, adapted to
conform to package standards.

## References

[StackOverflow: Largest n unique
values](https://stackoverflow.com/questions/36993935)

[Rcpp Gallery
article](https://gallery.rcpp.org/articles/top-elements-from-vectors-using-priority-queue/)

## See also

[`max`](https://rdrr.io/r/base/Extremes.html),
[`min`](https://rdrr.io/r/base/Extremes.html),
[`sort`](https://rdrr.io/r/base/sort.html),
[`rank`](https://rdrr.io/r/base/rank.html)

Other quantile: [`quantileX()`](quantileX.md)

## Examples

``` r

x <- sample(1:10, 1000, rep=TRUE)
large(x, 3)
#> [1] 10 10 10
large(x, k=3, unique=TRUE)
#> $value
#> [1]  8  9 10
#> 
#> $frequency
#> [1]  94 107 108
#> 

# works fine up to x ~ 1e6
x <- runif(1000000)
small(x, 3, unique=TRUE)
#> $value
#> [1] 1.548324e-07 5.243346e-07 1.089182e-06
#> 
#> $frequency
#> [1] 1 1 1
#> 
small(x, 3, unique=FALSE)
#> [1] 1.548324e-07 5.243346e-07 1.089182e-06

# Both ends
cat(highLow(bedrock::Pizza$temperature, na.last=NA))
#> lowest : 19.3, 19.4, 20, 20.2 (2), 20.35
#> highest: 63.8, 64.1, 64.6, 64.7, 64.8

 
```
