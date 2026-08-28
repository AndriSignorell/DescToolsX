# Atkinson Index

Computes the Atkinson inequality index.

## Usage

``` r
atkinson(
  x,
  n = rep(1, length(x)),
  epsilon = 0.5,
  na.rm = FALSE,
  tol = 0.00000001
)
```

## Arguments

- x:

  numeric vector of non-negative values, such as incomes

- n:

  optional frequency weights; either a single non-negative whole number
  or a vector having the same length as `x`

- epsilon:

  single non-negative numeric value specifying the inequality aversion
  parameter

- na.rm:

  logical; whether missing values in `x` are removed

- tol:

  single non-negative numeric value specifying the tolerance for
  treating `epsilon` as equal to one

## Value

a numeric value in the interval `[0, 1]`, or `NA_real_` if the index is
undefined

## Details

With frequency weights \\n_i\\, the weighted arithmetic mean is

\$\$ \bar{x}\_n = \frac{\sum_i n_i x_i}{\sum_i n_i}. \$\$

For \\\varepsilon \ne 1\\, the Atkinson index is

\$\$ A(\varepsilon) = 1 - \frac{ \left( \frac{\sum_i n_i
x_i^{1-\varepsilon}} {\sum_i n_i} \right)^{1/(1-\varepsilon)}
}{\bar{x}\_n}. \$\$

For \\\varepsilon = 1\\,

\$\$ A(1) = 1 - \frac{ \exp\left( \frac{\sum_i n_i \log(x_i)} {\sum_i
n_i} \right) }{\bar{x}\_n}. \$\$

The calculation uses normalized frequency weights and logarithmic power
means. It therefore does not construct the potentially very large vector
that would result from `rep(x, n)`.

Observations with zero frequency are ignored. If all frequencies are
zero or no observations remain after removing missing values, `NA_real_`
is returned.

If all values are zero, the index is defined as zero. If at least one
value is zero and `epsilon >= 1`, the equally distributed equivalent
value is zero and the index is one.

Negative values, non-finite values, and missing values when
`na.rm = FALSE` produce `NA_real_`. A negative `epsilon` also produces
`NA_real_`.

## References

Atkinson, A. B. (1970). On the measurement of inequality. *Journal of
Economic Theory*, 2(3), 244–263.

## See also

Other inequality: [`divCoef()`](divCoef.md), [`gini()`](gini.md),
[`lc()`](Lc.md), [`rosenbluth()`](rosenbluth.md), [`theil()`](theil.md)

## Examples

``` r
x <- c(541, 1463, 2445, 3438, 4437,
       5401, 6392, 8304, 11904, 22261)

atkinson(x)
#> [1] 0.1796591
atkinson(x, epsilon = 1)
#> [1] 0.3518251
atkinson(x, epsilon = 2)
#> [1] 0.6290111

# frequency weights
atkinson(c(10, 20, 30), n = c(3, 1, 1))
#> [1] 0.05558586

# zero incomes
atkinson(c(0, 10, 20), epsilon = 1)
#> [1] 1
```
