# Herfindahl Index

Computes the Herfindahl (or Herfindahl-Hirschman) index as a measure of
concentration or inequality.

## Usage

``` r
herfindahl(x, n = rep(1, length(x)), parameter = 1, na.rm = FALSE)
```

## Arguments

- x:

  numeric vector of non-negative values, such as market shares, incomes,
  or frequencies

- n:

  optional frequency weights. Each element of `x` is replicated `n`
  times.

- parameter:

  parameter \\m\\ controlling sensitivity to concentration; must be
  strictly positive, default is `1`. `m = 0` is rejected: it degenerates
  to a constant 1 for every input.

- na.rm:

  logical; whether to remove missing values

## Value

numeric scalar containing the Herfindahl index

## Details

The index is defined as the power mean of order \\m+1\\ of the relative
shares. For `parameter = 1`, the classical Herfindahl-Hirschman Index
(HHI) is obtained.

Larger values indicate higher concentration. If negative values or
missing values (when `na.rm = FALSE`) are present, `NA` is returned.

## Note

Based on code by Achim Zeileis, rewritten to conform to package
standards.

## References

Cowell, F. A. (2000) Measurement of Inequality, in Atkinson, A. B.,
Bourguignon, F. *Handbook of Income Distribution*. (Eds) Amsterdam

Cowell, F. A. (1995) *Measuring Inequality*. Prentice Hall/Harvester
Wheatshef

Hall, M., Tidemann, N. (1967) *Measures of Concentration*, JASA 62,
162-168.

Hirschman, A. O. (1964). The paternity of an index.

## See also

[`gini`](gini.md), [`atkinson`](atkinson.md)

Other diversity.concentration: [`entropy()`](entropy.md),
[`simpson()`](simpson.md)

## Examples

``` r
# generate vector (of sales)
x <- c(541, 1463, 2445, 3438, 4437, 5401, 6392, 8304, 11904, 22261)

# compute Herfindahl coefficient with parameter 1
herfindahl(x)
#> [1] 0.1840812

# Some more examples
herfindahl(c(261,29,33,15,39,28,95,5,6,28,69,8,105,38,15))
#> [1] 0.1668737
herfindahl(c(783,121,112,70,201,153,425,19,37,126,325,51,442,193,41))
#> [1] 0.1301292
 
```
