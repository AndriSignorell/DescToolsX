# Shannon Entropy

Computes the Shannon entropy of a categorical vector, contingency table,
or matrix.

## Usage

``` r
entropy(x, y = NULL, base = 2, normalize = FALSE, na.rm = FALSE, ...)
```

## Arguments

- x:

  a table, matrix or array of counts, or a categorical vector (factor,
  character or logical), which is tabulated first

- y:

  an optional second variable used together with `x` to create a
  contingency table via `table(x, y, ...)`

- base:

  logarithm base; defaults to `2` (bits)

- normalize:

  logical. If `TRUE`, entropy is normalized to the interval \\\[0,1\]\\.

- na.rm:

  logical; if `TRUE`, missing counts are dropped. A categorical `x` is
  tabulated with [`table`](https://rdrr.io/r/base/table.html), which
  excludes `NA` by default in any case.

- ...:

  additional arguments passed to
  [`table()`](https://rdrr.io/r/base/table.html)

## Value

a numeric scalar containing the entropy

## Details

Entropy is defined as:

\$\$ H(X) = - \sum_i p_i \log_b(p_i) \$\$

where \\p_i\\ are empirical probabilities and \\b\\ is the logarithm
base.

Common logarithm bases:

|               |                 |
|---------------|-----------------|
| base = 2      | entropy in bits |
| base = exp(1) | entropy in nats |
| base = 10     | entropy in bans |

Zero probabilities are ignored in the summation.

`normalize = TRUE` divides by \\\log_b k\\, with \\k\\ the number of
*occupied* categories rather than the number of possible ones. The
maximum of 1 is therefore reached whenever the observed categories are
equally frequent, independently of how many empty levels the input
carries. With a single occupied category the normalized value is 0.

## References

Shannon CE (1948). A Mathematical Theory of Communication. Bell System
Technical Journal, 27, 379-423.

## See also

Other diversity.concentration: [`herfindahl()`](herfindahl.md),
[`simpson()`](simpson.md)

## Examples

``` r
x <- c("A", "A", "B", "B", "C")

entropy(x)
#> [1] 1.521928

tab <- matrix(c(10, 20,
                30, 40), nrow = 2)

entropy(tab)
#> [1] 1.846439
entropy(tab, normalize = TRUE)
#> [1] 0.9232197

# a fair coin carries exactly one bit
entropy(c(50, 50))
#> [1] 1
```
