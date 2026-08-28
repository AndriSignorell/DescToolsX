# Detect Whether an Object Looks Like a Confusion/Coincidence Matrix

Checks if `x` behaves like a rater-by-rater contingency table: square 2D
numeric (integer-like) counts (or, optionally, proportions),
non-negative, finite, and (optionally) with matching row/column names.

## Usage

``` r
isConfusionTable(
  x,
  requireDimnames = TRUE,
  requireSameLevels = TRUE,
  integerTol = sqrt(.Machine$double.eps),
  acceptProportions = TRUE,
  requireSquare = TRUE
)
```

## Arguments

- x:

  object to check, typically a `table`, `matrix`, or numeric
  `data.frame`

- requireDimnames:

  logical; if `TRUE`, both row and column names must be present.
  Defaults to `TRUE`.

- requireSameLevels:

  logical; if `TRUE` and dimnames are present, row and column names must
  be the same set (order ignored). Defaults to `TRUE`.

- integerTol:

  numeric tolerance for integer-like counts; defaults to
  `sqrt(.Machine$double.eps)`

- acceptProportions:

  logical; if `TRUE`, proportion tables are accepted when all entries
  are in \\\[0, 1\]\\ and their sum is approximately

  1.  Defaults to `TRUE`.

- requireSquare:

  logical; whether to require a square table; defaults to `TRUE`

## Value

`TRUE` if `x` looks like a confusion or coincidence matrix, otherwise
`FALSE`

## Examples

``` r
tab <- table(sample(letters[1:3], 100, TRUE),
             sample(letters[1:3], 100, TRUE))
isConfusionTable(tab)               # TRUE
#> [1] TRUE

M <- as.matrix(tab)
isConfusionTable(M)                 # TRUE (dimnames present)
#> [1] TRUE
isConfusionTable(unname(M), requireDimnames = FALSE)  # TRUE without names
#> [1] TRUE

df <- as.data.frame.matrix(tab)
isConfusionTable(df)                # TRUE (numeric data.frame)
#> [1] TRUE

# Two-column raw ratings are NOT a confusion table:
ratings <- cbind(r1 = sample(0:1, 50, TRUE), r2 = sample(0:1, 50, TRUE))
isConfusionTable(ratings)           # FALSE (not square)
#> [1] FALSE

```
