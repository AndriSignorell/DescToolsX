# A Class for Dealing with the Yearmonth Format

Representing year and month information as an integer in YYYYMM format
is compact and efficient. Calendar arithmetic must nevertheless preserve
the date structure: for example, subtracting two months from 201201
should return 201111 rather than 201199. [`addMonths()`](AddMonths.md)
provides this arithmetic for objects of class `"ym"`.

## Usage

``` r
as.ym(x)

# S3 method for class 'ym'
as.Date(x, d = 1, ...)

# S3 method for class 'ym'
print(x, ...)
```

## Arguments

- x:

  a vector of integers, representing the dates in the format YYYYMM, to
  which a number of months has to be added. YYYY must lie in the range
  of 1000-3000, MM in 1-12. Values outside that range become `NA`.

- d:

  the day to be used for converting a yearmonth to a date. Default is

  1.  Combinations that do not exist (e.g. 30 February) yield `NA`.

- ...:

  further arguments; currently unused

## Value

- `as.ym()`:

  an integer vector of class `"ym"`

- `as.Date.ym()`:

  a vector of class `"Date"`

- [`addMonths.ym()`](AddMonths.md):

  an integer vector of class `"ym"`

- `print.ym()`:

  invisibly, `x`

## Details

All parameters are recycled if necessary, following the usual arithmetic
recycling rules; a warning is issued when the longer argument is not a
multiple of the length of the shorter one.

## Note

Based on code by Roland Rapold, adapted to conform to package standards.

## Examples

``` r

month(as.ym(202408))
#> [1] 8
year(as.ym(202408))
#> [1] 2024

year(as.Date("2024-12-05"))
#> [1] 2024
year(as.ym(202412))
#> [1] 2024

month(as.Date("2024-12-05"), fmt = "mm")
#> [1] Dec
#> 12 Levels: Jan < Feb < Mar < Apr < May < Jun < Jul < Aug < Sep < ... < Dec
month(as.ym(202412), fmt = "mm")
#> [1] Dec
#> 12 Levels: Jan < Feb < Mar < Apr < May < Jun < Jul < Aug < Sep < ... < Dec

# arithmetic stays in the ym domain, so it can be chained
addMonths(as.ym(201511), 5)
#> [1] 201604
as.ym(201511) + 5 - 2
#> [1] 201602

addMonths(as.ym(c(201511, 201302)), c(5, 15))
#> [1] 201604 201405
addMonths(as.ym(c(201511, 201302)), c(5, -4))
#> [1] 201604 201210

# out-of-range input is flagged, not silently mangled
as.ym(c(201513, 999901))
#> [1] NA NA
```
