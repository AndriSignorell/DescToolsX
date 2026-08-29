# Concordant and Discordant Pairs

Counts, for all \\n(n-1)/2\\ pairs of observations, how many are
concordant, how many discordant, and how the remaining ties are
distributed. This is the quantity every rank-based association measure
for ordinal data is built on.

## Usage

``` r
conDisPairs(x, y = NULL)
```

## Arguments

- x:

  a numeric vector, an ordered factor, or a contingency table

- y:

  optional second numeric vector or ordered factor. If provided, vector
  mode is used.

  An [`ordered`](https://rdrr.io/r/base/factor.html) factor is converted
  to its level codes, which is exactly the ordinal information the
  counts rest on. An *unordered* factor is refused: its levels have no
  order, and converting it anyway would silently impose the alphabetical
  one and report concordances that are an artefact of the level names.

## Value

a named numeric vector with elements:

- `C`:

  number of concordant pairs

- `D`:

  number of discordant pairs

- `Ties_X`:

  pairs tied in `x` only

- `Ties_Y`:

  pairs tied in `y` only

- `Ties_XY`:

  pairs tied in both

If fewer than two observations remain, all five elements are `NA`.

## Details

Two observations \\(x_i, y_i)\\ and \\(x_j, y_j)\\ are **concordant** if
they are ordered the same way in both variables - one is larger in \\x\\
*and* larger in \\y\\ - and **discordant** if the orderings disagree. If
either variable ties the pair, it is neither, and is counted among the
ties instead.

The five counts partition the pairs exhaustively and without overlap:

\$\$C + D + T_X + T_Y + T\_{XY} = \frac{n(n-1)}{2}\$\$

`Ties_X` counts pairs tied in \\x\\ but *not* in \\y\\, `Ties_Y` the
reverse, and `Ties_XY` the pairs tied in both. The counts are therefore
*exclusive*; the inclusive marginal counts that the classic tau-b
formula uses are `Ties_X + Ties_XY` and `Ties_Y + Ties_XY`. That
separation is what the ordinal measures need: Goodman-Kruskal's gamma
ignores all ties, Kendall's tau-b corrects for `Ties_X` and `Ties_Y`
separately, Somers' \\D\\ for one of them only, and the \\c\\ statistic
scores half a point for a tie in \\y\\. All of them are therefore a
short formula on top of this one function - see
[`ordAssocs`](ordAssocs.md).

### Two input modes

**Vector mode** (`x` and `y` given) works on the raw observations and
keeps the full resolution of the data. Missing values are removed
pairwise.

**Table mode** (`x` a matrix or table) works on a cross tabulation. Use
it when the data are already tabulated, or when the variables have few
distinct values: the count then depends on the table's dimensions rather
than on the number of observations, so a million observations in a 4x5
table cost the same as a hundred.

Both modes return the same five numbers for the same data.

### Why this is fast

Counted naively, the definition is a double loop over all pairs and
costs \\O(n^2)\\ - a hundred thousand observations are five billion
comparisons, which is where a straightforward implementation stops being
usable.

Vector mode sorts the observations by \\x\\ and processes equal \\x\\
values in blocks. A one-dimensional Fenwick tree (binary indexed tree)
over the compressed ranks of \\y\\ counts smaller and larger preceding
values in \\O(\log n)\\ time per observation. The resulting complexity
is \\O(n \log n)\\ time and \\O(n)\\ memory. The difference is not a
constant factor: it is what makes six-figure sample sizes a matter of a
moment instead of a coffee break.

Table mode uses the cumulative-sum identity over the table and is \\O(r
c)\\ in the table's dimensions, independent of \\n\\.

Both are implemented in C++ and return exact integer counts - no
approximation, no sampling.

## References

Goodman, L. A., Kruskal, W. H. (1954) Measures of association for cross
classifications. *Journal of the American Statistical Association*,
**49**, 732-764.

Agresti, A. (2010) *Analysis of Ordinal Categorical Data* (2nd ed.).
Wiley.

## See also

[ordAssocs](ordAssocs.md), [gkGamma](ordAssocs.md),
[kendallTauA](ordAssocs.md), [kendallTauB](ordAssocs.md),
[stuartTauC](ordAssocs.md), [somersDelta](ordAssocs.md)

Other assoc.ordinal: [`cStat()`](cStat.md), [`kendallW()`](kendallW.md),
[`ordAssocs()`](ordAssocs.md)

## Examples

``` r
# vector input
x <- c(1, 2, 3, 1, 2)
y <- c(2, 1, 3, 2, 1)
conDisPairs(x, y)
#>       C       D  Ties_X  Ties_Y Ties_XY 
#>       4       4       0       0       2 

# the five counts partition all n(n-1)/2 pairs
sum(conDisPairs(x, y)) == choose(length(x), 2)
#> [1] TRUE

# table input gives the same answer
tab <- table(x, y)
conDisPairs(tab)
#>       C       D  Ties_X  Ties_Y Ties_XY 
#>       4       4       0       0       2 

# the ordinal measures are short formulas on top of it
p <- conDisPairs(x, y)
unname((p["C"] - p["D"]) / (p["C"] + p["D"]))   # Goodman-Kruskal's gamma
#> [1] 0
gkGamma(x, y)
#> [1] 0

# \donttest{
# vector mode stays usable where a pairwise double loop would not:
# 200'000 observations are 2 * 10^10 pairs
set.seed(1)
n <- 2e5
system.time(conDisPairs(rnorm(n), rnorm(n)))
#>    user  system elapsed 
#>   0.089   0.002   0.091 
# }
```
