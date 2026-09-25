# Simpson Diversity Indices

Computes Simpson-type diversity indices: the Gini-Simpson index, the
Hunter-Gaston index (its unbiased, finite-sample version) and the index
of qualitative variation (the Gini-Simpson index rescaled to a maximum
of 1).

## Usage

``` r
simpson(
  x,
  method = c("gini", "hunter", "iqv"),
  categories = NULL,
  na.rm = FALSE
)
```

## Arguments

- x:

  a factor or character vector of observations, or a vector of
  non-negative counts. Note that a *numeric* vector is always read as
  counts, never as observations; wrap it in
  [`factor()`](https://rdrr.io/r/base/factor.html) to have it tabulated
  instead. A data frame with a single row of numeric columns (e.g. one
  site of a community matrix) is read as counts.

- method:

  character string specifying the index to compute: `"gini"`,
  `"hunter"`, or `"iqv"`.

- categories:

  the possible categories for `method = "iqv"`: either their number (a
  single number) or the categories themselves (a vector, e.g.
  `levels(x)`). If `NULL` (default), the number of observed, non-empty
  categories is used. Ignored, with a warning, for the other methods.

- na.rm:

  logical. If `TRUE`, missing values are removed before computation. If
  `FALSE` and `x` contains `NA`, the result will be `NA_real_`.

## Value

a numeric scalar between 0 and 1. `NA_real_` (with a warning) when the
index is undefined: no observations, \\N \< 2\\ for `"hunter"`, \\K \<
2\\ for `"iqv"`; `NA_real_` without a warning for missing values with
`na.rm = FALSE`. Invalid input (negative, infinite or, for `"hunter"`,
non-integer counts; invalid `categories`) is an error.

## Details

The Gini-Simpson index is defined as \\1 - \sum p_i^2\\, where \\p_i\\
are the relative frequencies of the categories.

The Hunter-Gaston index is defined as \\1 - \sum n_i (n_i - 1) / (N (N -
1))\\, where \\n_i\\ are the counts and \\N\\ is the total sample size.
It equals the Gini-Simpson index times \\N / (N - 1)\\. The complement,
\\\sum n_i (n_i - 1) / (N (N - 1))\\, is Simpson's unbiased \\\lambda\\,
the probability that two observations are of the *same* category.

The index of qualitative variation (IQV) is defined as \\(1 - \sum
p_i^2) \\ K / (K - 1)\\, where \\K\\ is the number of categories. Since
\\(K - 1)/K\\ is the largest value the Gini-Simpson index can attain
with \\K\\ categories, the IQV reaches 1 exactly for a uniform
distribution over all \\K\\ categories.

`"gini"` and `"hunter"` are the probability that two randomly selected
observations belong to different categories, drawn with and without
replacement respectively. `"iqv"` is not a probability but the
Gini-Simpson index relative to its maximum.

**Number of categories in the IQV.** \\K\\ should be the number of
categories that were *possible*, not only those observed. With the
default, a sample spread evenly over 3 of 5 possible categories gets an
IQV of 1, although it is far from the maximum diversity the coding
scheme allows. Supply `categories` whenever the set of categories is
known; for a factor, `categories = levels(x)` uses all levels, including
empty ones.

A sample concentrated in a single category is perfectly homogeneous
rather than undefined, so `"gini"` returns 0 for it, and so does
`"hunter"` as long as \\N \ge 2\\. The IQV requires \\K \ge 2\\, since
\\K - 1\\ appears in its denominator; with `categories` given, a single
occupied category gives an IQV of 0.

When `x` is numeric, it is treated as a vector of counts. Relative
frequencies are fine for `"gini"` and `"iqv"`; the Hunter-Gaston index
counts pairs of observations and therefore requires integer counts
(non-integer counts can push it above 1).

## References

Sachs, L. (1997). *Angewandte Statistik*. Springer.

Hunter, P. R., & Gaston, M. A. (1988). Numerical index of the
discriminatory ability of typing systems. *Journal of Clinical
Microbiology*, 26(11), 2465-2466.
https://doi.org/10.1128/jcm.26.11.2465-2466.1988

Mueller, J. H., & Schuessler, K. F. (1961). *Statistical Reasoning in
Sociology*. Houghton Mifflin.

Agresti, A., & Agresti, B. F. (1978). Statistical analysis of
qualitative variation. *Sociological Methodology*, 9, 204-237.
https://doi.org/10.2307/270810

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
simpson(x, method = "iqv")
#> [1] 0.9166667

# the same sample, if five categories were possible
simpson(x, method = "iqv", categories = 5)
#> [1] 0.7638889
simpson(x, method = "iqv", categories = c("A", "B", "C", "D", "E"))
#> [1] 0.7638889

# Using counts directly
counts <- c(A = 2, B = 1, C = 3)
simpson(counts, method = "hunter")
#> [1] 0.7333333

# Hunter-Gaston = Gini-Simpson * N / (N - 1)
simpson(counts, method = "gini") * 6 / 5
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
