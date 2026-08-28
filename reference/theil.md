# Theil Index

Computes the Theil inequality index (Theil T).

## Usage

``` r
theil(x, n = rep(1, length(x)), na.rm = FALSE)
```

## Arguments

- x:

  numeric vector of non-negative values, such as incomes

- n:

  optional frequency weights. Each element of `x` is replicated `n`
  times. Must be a vector of non-negative integers of the same length as
  `x`.

- na.rm:

  logical. If `TRUE`, missing values are removed.

## Value

a numeric scalar containing the Theil index. The value is 0 under
perfect equality and increases with inequality, up to a maximum of
\\\log(n)\\, attained when a single unit holds the entire total.

## Details

The Theil index is an entropy-based measure of inequality. It belongs to
the class of Generalized Entropy measures with parameter \\\alpha = 1\\.

The Theil T index is defined as

\$\$ T = \frac{1}{n} \sum \frac{x_i}{\bar{x}}
\log\left(\frac{x_i}{\bar{x}}\right) \$\$

where \\\bar{x}\\ is the mean of `x` and \\n\\ the number of
(replicated) observations.

Zero values are admissible: following the usual convention \\x \log x
\to 0\\ as \\x \to 0\\, they contribute 0 to the sum.

The index is decomposable into within- and between-group components,
which makes it particularly useful in applied inequality analysis.

If negative values or missing values (when `na.rm = FALSE`) are present,
`NA` is returned. The same holds if no observation remains after
removing missing values.

## References

Theil, H. (1967). Economics and Information Theory.

## See also

Other inequality: [`atkinson()`](atkinson.md),
[`divCoef()`](divCoef.md), [`gini()`](gini.md), [`lc()`](Lc.md),
[`rosenbluth()`](rosenbluth.md)

## Examples

``` r
theil(c(10, 10, 10, 10))          # perfect equality: 0
#> [1] 0
theil(c(0, 0, 0, 40))             # everything in one hand: log(4)
#> [1] 1.386294
theil(c(1, 2, 3, 4, 5))
#> [1] 0.1196876

# frequency weights replicate the observations
theil(1:3, n = c(1, 2, 3))
#> [1] 0.05699495
theil(rep(1:3, times = c(1, 2, 3)))
#> [1] 0.05699495
```
