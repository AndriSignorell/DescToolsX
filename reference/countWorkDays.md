# Count Work Days Between Two Dates

Returns the number of work days between two dates taking into account
the provided holiday dates.

## Usage

``` r
countWorkDays(from, to, holiday = NULL, nonworkdays = c("Sat", "Sun"))
```

## Arguments

- from:

  initial dates

- to:

  final dates

- holiday:

  a vector of dates (or strings coercible with
  [`as.Date`](https://rdrr.io/r/base/as.Date.html)) to exclude

- nonworkdays:

  a character vector containing the English three-letter weekday
  abbreviations to be treated as non-work days, i.e. a subset of
  `c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")`. Default is
  `c("Sat", "Sun")`.

## Value

an integer vector

## Details

The function is vectorised so that multiple initial and final dates can
be supplied. The date vectors are recycled if their lengths differ.

Both endpoints are counted, so `countWorkDays(d, d)` is 1 for a working
day and 0 otherwise. A `to` that lies before `from` yields 0.

Holidays outside the interval are ignored, as are holidays falling on a
non-work day; duplicated holiday dates are counted once.

## See also

Other date.time: [`addMonths()`](AddMonths.md),
[`date-time-predicates`](date-time-predicates.md),
[`date_functions`](date_functions.md), [`generation()`](generation.md),
[`time-conversions`](time-conversions.md), [`zodiac()`](zodiac.md)

## Examples

``` r

from <- as.Date("2019-01-01") + rep(0, 10)
to   <- as.Date("2019-01-15") + seq(0, 9)

countWorkDays(from, to)
#>  [1] 11 12 13 14 14 14 15 16 17 18

# which weekdays are involved
x <- seq(min(from), max(to), by = "days")
head(data.frame(date = x, day = weekday(x, fmt = "dd", lang = "en")))
#>         date day
#> 1 2019-01-01 Tue
#> 2 2019-01-02 Wed
#> 3 2019-01-03 Thu
#> 4 2019-01-04 Fri
#> 5 2019-01-05 Sat
#> 6 2019-01-06 Sun

# two holidays inside the period reduce the count by two
countWorkDays(from = min(x), to = max(x))
#> [1] 18
countWorkDays(from = min(x), to = max(x),
              holiday = c("2019-01-07", "2019-01-08"))
#> [1] 16

# a single day
countWorkDays(as.Date("2019-01-05"), as.Date("2019-01-05"))  # Saturday: 0
#> [1] 0
```
