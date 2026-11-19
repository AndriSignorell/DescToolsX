# Chained Price Index

Price index over several periods for a panel of items in long format.
For every pair of adjacent periods a bilateral index is computed on the
items observed in both periods (matched items), and these links are
multiplied up to the chained index. Items may thus enter and leave the
basket over time, and the weights are updated with every link.

## Usage

``` r
chainedIndex(
  p,
  id,
  period,
  q = NULL,
  estimator = c("laspeyres", "paasche", "fisher", "tornqvist", "walsh",
    "marshall-edgeworth", "drobisch", "sato-vartia", "geo-laspeyres", "geo-paasche",
    "palgrave", "jevons", "carli", "dutot"),
  base = NULL,
  na.rm = FALSE
)
```

## Arguments

- p:

  numeric vector of prices, one value per item and period

- id:

  vector identifying the item of each price

- period:

  vector identifying the period of each price, of any sortable type
  (numeric, Date, factor, ...)

- q:

  numeric vector of quantities belonging to `p`, or `NULL` for the
  unweighted estimators

- estimator:

  the index formula used for the links, see
  [`priceIndex()`](priceIndex.md)

- base:

  the period in which the index is set to 1. Defaults to the first
  period.

- na.rm:

  logical, should records with a missing price or quantity be removed?
  Defaults to `FALSE`, in which case a link involving a missing value is
  `NA`.

## Value

A data frame with one row per period and the columns

- `period`:

  the period

- `index`:

  the chained index, 1 in the base period

- `link`:

  the bilateral index against the previous period, `NA` in the first
  period

- `n`:

  the number of matched items the link is based on

## Details

The periods are taken in the sort order of `period` (level order for a
factor). A link between two periods without any common item is not
defined; it is returned as `NA` with a warning, and so is the index
beyond that break as seen from the base period.

New and disappearing items do not contribute to the link in which they
enter or leave. Whether an item is the same over time is decided by `id`
alone, so the choice of the item level matters: at the level of the
single package a switch from an original to a cheaper generic is a
change of the basket, at the level of the substance (with unit values as
prices) it is a price decrease.

## References

Balk, B. M. (2008). *Price and Quantity Index Numbers: Models for
Measuring Aggregate Change and Difference*. Cambridge University Press.

Diewert, W. E. (1976). Exact and superlative index numbers. *Journal of
Econometrics*, 4(2), 115–145.

ILO, IMF, OECD, UNECE, Eurostat and World Bank (2004). *Consumer Price
Index Manual: Theory and Practice*. International Labour Office, Geneva.

## See also

[`priceIndex()`](priceIndex.md) for the bilateral indices and their
formulas

Other index.price: [`priceIndex()`](priceIndex.md)

## Examples

``` r
# item "d" enters in 2021, item "c" leaves after 2021
d <- data.frame(
  year = c(2020, 2020, 2020, 2021, 2021, 2021, 2021, 2022, 2022, 2022),
  id   = c("a", "b", "c", "a", "b", "c", "d", "a", "b", "d"),
  p    = c(2.0, 5.0, 10.0, 2.2, 5.5, 9.0, 20.0, 2.1, 5.6, 16.0),
  q    = c(100, 40, 10, 90, 45, 14, 5, 95, 40, 12)
)

chainedIndex(d$p, d$id, d$year, d$q, estimator = "tornqvist")
#>   period    index      link  n
#> 1   2020 1.000000        NA NA
#> 2   2021 1.054558 1.0545582  3
#> 3   2022 0.989307 0.9381246  3
##   period    index      link  n
## 1   2020 1.000000        NA NA
## 2   2021 1.054558 1.0545582  3
## 3   2022 0.989307 0.9381246  3

# unweighted, rebased to 2022
chainedIndex(d$p, d$id, d$year, estimator = "jevons", base = 2022)
#>   period    index     link  n
#> 1   2020 1.057029       NA NA
#> 2   2021 1.087500 1.028828  3
#> 3   2022 1.000000 0.919540  3
```
