# Simpson Diversity Indices

Computes Simpson-type diversity indices, including the Gini-Simpson
index, the Hunter-Gaston index (bias-corrected for sample size), and the
Deltas-corrected Gini-Simpson index (bias-corrected for number of
categories).

## Usage

``` r
simpson(x, method = c("gini", "hunter", "deltas"), na.rm = FALSE)
```

## Arguments

- x:

  a factor or character vector of observations, or a vector of
  non-negative counts. Note that a *numeric* vector is always read as
  counts, never as observations; wrap it in
  [`factor`](https://rdrr.io/r/base/factor.html) to have it tabulated
  instead.

- method:

  character string specifying the index to compute: `"gini"`,
  `"hunter"`, or `"deltas"`

- na.rm:

  logical. If `TRUE`, missing values are removed before computation. If
  `FALSE` and `x` contains `NA`, the result will be `NA_real_`.

## Value

a numeric scalar between 0 and 1. Returns `NA_real_` if input is invalid
or empty.

## Details

The Gini-Simpson index is defined as \\1 - \sum p_i^2\\, where \\p_i\\
are the relative frequencies of categories.

The Hunter-Gaston index is defined as \\1 - \sum n_i (n_i - 1) / (N (N -
1))\\, where \\n_i\\ are counts and \\N\\ is the total sample size.

The Deltas correction is defined as \\(1 - \sum p_i^2) \* k / (k - 1)\\,
where \\k\\ is the number of observed (non-empty) categories. The factor
is the reciprocal of \\(k-1)/k\\, the largest value the Gini-Simpson
index can attain with \\k\\ categories, so the corrected index reaches 1
for a uniform distribution over the observed categories.

All indices represent the probability that two randomly selected
observations belong to different categories.

The Hunter-Gaston index corrects for finite sample size, while the
Deltas correction adjusts for a small number of observed categories.
Note that the finite-sample correction \\N/(N-1)\\ applied to the
Gini-Simpson index reproduces the Hunter-Gaston index exactly; the two
are the same adjustment with \\N\\ and \\k\\ in the correction factor.

A sample concentrated in a single category is perfectly homogeneous
rather than undefined, so `"gini"` and `"hunter"` return 0 for it. Only
the Deltas correction requires \\k \ge 2\\, since \\k - 1\\ appears in
its denominator.

When `x` is numeric, it is treated as a vector of counts. Non-integer
values produce a warning; the Hunter-Gaston index requires integer
counts.

## References

Sachs, L. (1997). *Angewandte Statistik*. Springer.

Hunter, P. R., & Gaston, M. A. (1988). Numerical index of the
discriminatory ability of typing systems. *Journal of Clinical
Microbiology*, 26(11), 2465-2466.
https://doi.org/10.1128/jcm.26.11.2465-2466.1988

Deltas, G. (2003). The small-sample bias of the Gini coefficient:
Results and implications for empirical research. *Review of Economics
and Statistics*, 85(1), 226-234.
https://doi.org/10.1162/rest.2003.85.1.226

## See also

Other diversity.concentration: [`entropy()`](entropy.md),
[`herfindahl()`](herfindahl.md)

## Examples

``` r
x <- c("A", "A", "B", "C", "C", "C")

simpson(x, method = "gini")
#> [1] 0.6111111
simpson(x, method = "hunter")
#> [1] 0.7333333
simpson(x, method = "deltas")
#> [1] 0.9166667

# Using counts directly
counts <- c(A = 2, B = 1, C = 3)
simpson(counts, method = "hunter")
#> [1] 0.7333333

# a numeric vector of observations must be tabulated first, otherwise
# its values are read as counts
simpson(factor(c(1, 1, 2, 2, 3)), method = "gini")
#> [1] 0.64

# With missing values
x <- c("A", "A", NA, "B")
simpson(x, method = "gini", na.rm = TRUE)
#> [1] 0.4444444
```
