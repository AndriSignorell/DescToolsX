# Cut an Integer Variable into Intervals

A [`cut`](https://rdrr.io/r/base/cut.html) method for integer vectors.
[`cut.default()`](https://rdrr.io/r/base/cut.html) labels its intervals
in the mathematical `"(a,b]"` notation, which is right for a continuous
variable but reads awkwardly for counts: for integers, `"11-20"` says
the same thing more plainly than `"(10,20]"`.

## Usage

``` r
# S3 method for class 'integer'
cut(
  x,
  breaks,
  labels = NULL,
  include.lowest = FALSE,
  right = TRUE,
  ordered_result = FALSE,
  ...
)
```

## Arguments

- x:

  an integer vector

- breaks:

  either a vector of cut points or a single number giving the number of
  intervals

- labels:

  labels for the levels. The integer-style labels described above are
  used when this is `NULL`.

- include.lowest, right, ordered_result, ...:

  passed to [`cut.default`](https://rdrr.io/r/base/cut.html)

## Value

a factor of the same length as `x`

## Details

The integer labels are only constructed when `labels` is `NULL` *and*
`breaks` is a vector of whole numbers. A scalar `breaks` (a number of
intervals) is passed straight to
[`cut.default`](https://rdrr.io/r/base/cut.html), which computes the cut
points itself, and fractional break points fall back to the default
interval notation as well - `"34.3-66.6"` would suggest an integer range
that does not exist.

Infinite outer breaks are rendered as `".."`, so
`breaks = c(0, 10, Inf)` yields `"1-10"` and `"11-.."`.

## See also

[`cut`](https://rdrr.io/r/base/cut.html), [`cutAge`](cutAge.md),
[`cutQ`](cutQ.md)

Other cut: [`cutAge()`](cutAge.md), [`cutQ()`](cutQ.md)

## Examples

``` r
x <- as.integer(c(1, 5, 10, 11, 20, 21))

cut(x, breaks = c(0, 10, 20, Inf))
#> [1] 1-10  1-10  1-10  11-20 11-20 21-..
#> Levels: 1-10 11-20 21-..

# left-closed intervals shift the labels accordingly
cut(x, breaks = c(0, 10, 20, Inf), right = FALSE)
#> [1] 0-9   0-9   10-19 10-19 20-.. 20-..
#> Levels: 0-9 10-19 20-..

# a scalar breaks is left to cut.default()
cut(x, breaks = 3)
#> [1] (0.98,7.67] (0.98,7.67] (7.67,14.3] (7.67,14.3] (14.3,21]   (14.3,21]  
#> Levels: (0.98,7.67] (7.67,14.3] (14.3,21]
```
