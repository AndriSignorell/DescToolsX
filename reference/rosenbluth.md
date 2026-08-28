# Rosenbluth Index

Computes the Rosenbluth index as a measure of concentration.

## Usage

``` r
rosenbluth(x, n = rep(1, length(x)), na.rm = FALSE)
```

## Arguments

- x:

  numeric vector of non-negative values, such as market shares or
  frequencies

- n:

  optional frequency weights. Each element of `x` is replicated `n`
  times.

- na.rm:

  logical. If `TRUE`, missing values are removed.

## Value

a numeric scalar containing the Rosenbluth index

## Details

The Rosenbluth index is based on the ranked shares and is directly
related to market concentration. Larger values indicate stronger
concentration.

With the shares \\p_i = x_i / \sum x\\ sorted in decreasing order and
\\i\\ their rank, the index is \$\$HT = 1 / (2 \sum i p_i - 1).\$\$ It
ranges from \\1/k\\ for \\k\\ units of equal size to 1 for a single unit
holding everything, so it is read on the same scale as the Herfindahl
index rather than as an inequality measure.

If negative values or missing values (when `na.rm = FALSE`) are present,
`NA` is returned. The index is undefined when all values are zero, and
`NA` is returned in that case as well.

## References

Rosenbluth, G. (1955). Measures of concentration. In: *Business
Concentration and Price Policy*. Princeton University Press, 57-99.

Hall, M., Tideman, N. (1967). Measures of concentration. *Journal of the
American Statistical Association*, 62, 162-168.

## See also

Other inequality: [`atkinson()`](atkinson.md),
[`divCoef()`](divCoef.md), [`gini()`](gini.md), [`lc()`](Lc.md),
[`theil()`](theil.md)

## Examples

``` r
# four units of equal size: the index takes its minimum 1/4
rosenbluth(c(1, 1, 1, 1))
#> [1] 0.25

# one unit holding everything: the maximum 1
rosenbluth(c(1, 0, 0, 0))
#> [1] 1

# a dominant unit next to three small ones
rosenbluth(c(10, 1, 1, 1))
#> [1] 0.52

# frequency weights replicate the values
rosenbluth(c(10, 1), n = c(1, 3))
#> [1] 0.52
```
