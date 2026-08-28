# Mutual Information

Computes the mutual information (MI) between two variables from a
contingency table.

## Usage

``` r
mutInf(x, y = NULL, base = 2, normalize = FALSE, ...)
```

## Arguments

- x:

  a contingency table, matrix, or vector that can be coerced into a
  contingency table

- y:

  an optional second variable used together with `x` to create a
  contingency table via `table(x, y, ...)`

- base:

  logarithm base. Defaults to `2` (bits).

- normalize:

  logical. If `TRUE`, returns normalized mutual information (NMI).

- ...:

  additional arguments passed to
  [`table()`](https://rdrr.io/r/base/table.html)

## Value

a numeric scalar containing the mutual information

## Details

Mutual information quantifies the amount of information obtained about
one variable through observing the other.

It is defined as:

\$\$ I(X;Y) = H(X) + H(Y) - H(X,Y) \$\$

where \\H(X)\\ and \\H(Y)\\ are marginal entropies and \\H(X,Y)\\ is the
joint entropy.

Mutual information is always nonnegative:

\$\$ I(X;Y) \ge 0 \$\$

Larger values indicate stronger dependence.

If `normalize = TRUE`, the returned value is:

\$\$ \frac{I(X;Y)} {\sqrt{H(X)H(Y)}} \$\$

which approximately scales the measure to \\\[0,1\]\\.

## References

Cover TM, Thomas JA (2006). Elements of Information Theory (2nd ed.).
Wiley.

## See also

[`entropy`](entropy.md)

Other assoc.nominal: [`contCoef()`](contCoef.md),
[`cramerV()`](cramerV.md), [`gkTau()`](gkTau.md),
[`lambda()`](lambda.md), [`phi()`](phi.md),
[`tschuprowT()`](tschuprowT.md), [`uncertCoef()`](uncertCoef.md),
[`yule`](yule.md)

## Examples

``` r
tab <- matrix(
  c(10, 20,
    30, 40),
  nrow = 2
)

mutInf(tab)
#> [1] 0.005802149

mutInf(tab, normalize = TRUE)
#> [1] 0.006272356

x <- sample(letters[1:3], 100, TRUE)
y <- sample(LETTERS[1:2], 100, TRUE)

mutInf(x, y)
#> [1] 0.05455465
```
