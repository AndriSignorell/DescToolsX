# Identify Highly Correlated Variables

Identifies variables in a correlation matrix that should be removed due
to high pairwise correlations above a specified cutoff. The algorithm
uses a greedy approach similar to `caret::findCorrelation()`, but
extends it with flexible scoring methods and multiple output formats.

## Usage

``` r
findCorrX(
  x,
  cutoff = 0.9,
  method = c("mean", "max", "median"),
  output = c("index", "names", "logical", "report"),
  verbose = FALSE
)
```

## Arguments

- x:

  a symmetric correlation matrix

- cutoff:

  numeric threshold in (0, 1). Pairs with absolute correlation above
  this value are considered too highly correlated.

- method:

  character string specifying how variable importance is scored. One of
  `"mean"`, `"max"`, or `"median"`. Default is `"mean"`.

- output:

  character string specifying the return format:

  - `"index"` for indices of variables to remove (default)

  - `"names"` for column names of variables to remove

  - `"logical"` for a logical vector indicating removed variables

  - `"report"` for a detailed list of removed and retained variables and
    the decision log

- verbose:

  logical; if `TRUE`, progress information is printed

## Value

depending on `output`:

- `"index"`:

  integer vector of column indices to remove

- `"names"`:

  character vector of column names

- `"logical"`:

  logical vector with one element per column

- `"report"`:

  list with elements `removed`, `kept`, and `log`

## Details

The function iteratively examines pairs of variables with correlations
above `cutoff`. For each such pair, the variable with the higher overall
correlation (based on `method`) is removed.

The scoring is computed once at the beginning and kept fixed throughout,
ensuring deterministic and efficient behavior. Because the columns are
then processed in decreasing score order, the variable removed from a
pair is always the earlier - i.e. the higher-scoring - one.

This is a greedy heuristic and does not guarantee a globally optimal
solution.

## See also

Other assoc.continuous: [`corPart()`](corPart.md),
[`corPolychor()`](corPolychor.md), [`hoeffdingD()`](hoeffdingD.md),
[`keepSig()`](keepSig.md), [`pearsonCor()`](pearsonCor.md),
[`spearmanCor()`](spearmanCor.md)

## Examples

``` r
set.seed(123)
m <- matrix(rnorm(100), ncol = 5)
colnames(m) <- paste0("V", 1:5)
cmat <- cor(m)

findCorrX(cmat, cutoff = 0.8)
#> integer(0)
findCorrX(cmat, cutoff = 0.8, method = "max", output = "names")
#> character(0)
```
