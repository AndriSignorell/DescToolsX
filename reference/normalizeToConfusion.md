# Normalize Input to a Contingency or Agreement Table

Converts diverse input formats into a numeric contingency matrix
suitable for agreement measures (e.g. Cohen's Kappa) or association
measures (e.g. Cramer's V). Accepted input formats include tables,
matrices, data frames, lists, and raw rating vectors.

## Usage

``` r
normalizeToConfusion(
  x,
  y = NULL,
  levels = NULL,
  useNA = "no",
  mode = c("agreement", "association")
)
```

## Arguments

- x:

  input object. Accepted formats: a
  [`table`](https://rdrr.io/r/base/table.html) representing a
  pre-computed contingency table; a square numeric `matrix` representing
  a pre-computed contingency table; a numeric `matrix` with exactly 2
  columns (one per rater); a `list` or `data.frame` with exactly 2
  elements; or a vector if `y` is also supplied.

- y:

  optional second rating vector. If supplied, `x` and `y` are tabulated
  together.

- levels:

  optional category levels. These *rename* the categories in place - the
  counts are not moved - so supplying an order different from the one a
  table already carries reassigns the counts to other labels. For
  `mode = "agreement"`, use an atomic vector of common levels shared by
  both raters; for `mode = "association"`, use a list of length 2,
  `list(x_levels, y_levels)`.

- useNA:

  passed to [`table`](https://rdrr.io/r/base/table.html). Controls
  whether `NA` values appear as a level. One of `"no"` (default),
  `"ifany"`, or `"always"`.

- mode:

  character string, either `"agreement"` (default) or `"association"`.
  Agreement mode enforces a square table with identical row and column
  names. Association mode allows rectangular tables with independent row
  and column levels.

## Value

a numeric contingency matrix with dimnames set according to `levels`, if
supplied, or derived from the input

## Details

The function handles the following input formats:

- `table`:

  A pre-computed 2D contingency table which is validated via
  [`isConfusionTable`](isConfusionTable.md). For `mode = "agreement"`,
  the table must be square with identical row and column names.

- `matrix`:

  Either a pre-computed contingency matrix (square, passing
  [`isConfusionTable`](isConfusionTable.md) validation) or a two-rater
  matrix with exactly 2 columns. A 2-column non-square matrix is always
  treated as a rater matrix; each column is one rater's ratings.

- two vectors:

  If both `x` and `y` are supplied, they are tabulated via
  [`table`](https://rdrr.io/r/base/table.html) after coercing to
  factors.

- `list` or `data.frame` with 2 elements:

  Each element is treated as one rater's ratings.

For `mode = "agreement"`, levels must be shared between both raters and
the resulting table is square. For `mode = "association"`, row and
column levels may differ and the table may be rectangular.

## See also

[`isConfusionTable()`](isConfusionTable.md),
[`raterFrame()`](raterFrame.md),
[`pairApply()`](https://andrisignorell.github.io/bedrock/reference/pairApply.html)

Other agreement: [`blandAltmanData()`](blandAltmanData.md),
[`raterFrame()`](raterFrame.md)

## Examples

``` r
A <- c("pos","neg","pos","inc")
B <- c("pos","pos","neg","inc")
normalizeToConfusion(A, B)
#>      b
#> a     inc neg pos
#>   inc   1   0   0
#>   neg   0   0   1
#>   pos   0   1   1

tab <- table(A, B)
normalizeToConfusion(tab)
#>      B
#> A     inc neg pos
#>   inc   1   0   0
#>   neg   0   0   1
#>   pos   0   1   1

set.seed(1)
C <- sample(c("pos","neg","inc"), length(A), TRUE)
df <- data.frame(R1=A, R2=B, R3=C)
normalizeToConfusion(df[, 1:2])      # R1 vs R2
#>      b
#> a     inc neg pos
#>   inc   1   0   0
#>   neg   0   0   1
#>   pos   0   1   1
normalizeToConfusion(df[, c(1,3)])   # R1 vs R3
#>      b
#> a     inc neg pos
#>   inc   0   1   0
#>   neg   1   0   0
#>   pos   0   0   2

# list of rating vectors:
normalizeToConfusion(list(A, B))
#>      b
#> a     inc neg pos
#>   inc   1   0   0
#>   neg   0   0   1
#>   pos   0   1   1

# use NAs
B[2] <- NA
normalizeToConfusion(A, B, useNA = "always")
#>       b
#> a      inc neg pos <NA>
#>   inc    1   0   0    0
#>   neg    0   0   0    1
#>   pos    0   1   1    0
#>   <NA>   0   0   0    0

anxiety <- data.frame(
  rater1 = c(3,3,3,4,5,5,2),
  rater2 = c(3,6,4,6,2,4,2),
  rater3 = c(2,1,4,4,3,2,1)
)

x <- anxiety[, 1]
y <- anxiety[, 2]

# two vectors:
normalizeToConfusion(x, y)
#>    b
#> a   2 3 4 5 6
#>   2 1 0 0 0 0
#>   3 0 1 1 0 1
#>   4 0 0 0 0 1
#>   5 1 0 1 0 0
#>   6 0 0 0 0 0

# matrix / data.frame with 2 columns (subjects x raters):
normalizeToConfusion(cbind(x, y))
#>    b
#> a   2 3 4 5 6
#>   2 1 0 0 0 0
#>   3 0 1 1 0 1
#>   4 0 0 0 0 1
#>   5 1 0 1 0 0
#>   6 0 0 0 0 0
normalizeToConfusion(data.frame(x, y))
#>    b
#> a   2 3 4 5 6
#>   2 1 0 0 0 0
#>   3 0 1 1 0 1
#>   4 0 0 0 0 1
#>   5 1 0 1 0 0
#>   6 0 0 0 0 0

# list with 2 elements:
normalizeToConfusion(list(x, y))
#>    b
#> a   2 3 4 5 6
#>   2 1 0 0 0 0
#>   3 0 1 1 0 1
#>   4 0 0 0 0 1
#>   5 1 0 1 0 0
#>   6 0 0 0 0 0

# pre-built table:
ratingscale <- sort(unique(c(x, y)))
normalizeToConfusion(table(factor(x, levels = ratingscale),
                           factor(y, levels = ratingscale)))
#>    
#>     2 3 4 5 6
#>   2 1 0 0 0 0
#>   3 0 1 1 0 1
#>   4 0 0 0 0 1
#>   5 1 0 1 0 0
#>   6 0 0 0 0 0

d.anxiety <- data.frame(
  rater  = c("rater1", "rater1", "rater1", "rater1", "rater1", "rater1", "rater1",
             "rater2", "rater2", "rater2", "rater2", "rater2", "rater2", "rater2",
             "rater3", "rater3", "rater3", "rater3", "rater3", "rater3", "rater3"),
  rating = c(3, 3, 3, 4, 5, 5, 2,
             3, 6, 4, 6, 2, 4, 2,
             2, 1, 4, 4, 3, 2, 1),
  subj   = c(1, 2, 3, 4, 5, 6, 7,
             1, 2, 3, 4, 5, 6, 7,
             1, 2, 3, 4, 5, 6, 7)
)

# via raterFrame (wide format, subjects x raters):
normalizeToConfusion(
  raterFrame(rating ~ subj | rater, data = d.anxiety,
             subset = rater %in% c("rater1", "rater2"), dropSubj = TRUE)
)
#>    b
#> a   2 3 4 5 6
#>   2 1 0 0 0 0
#>   3 0 1 1 0 1
#>   4 0 0 0 0 1
#>   5 1 0 1 0 0
#>   6 0 0 0 0 0

```
