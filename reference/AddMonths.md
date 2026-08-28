# Add Months to a Date

Adds or subtracts whole calendar months while ensuring that the result
is always a valid date.Negative values of `n` subtract months.

## Usage

``` r
addMonths(x, n, ...)

# Default S3 method
addMonths(x, n, ...)

# S3 method for class 'ym'
addMonths(x, n, ...)
```

## Arguments

- x:

  a `Date` object or an object coercible to one with
  [`as.Date`](https://rdrr.io/r/base/as.Date.html)

- n:

  numeric vector containing finite whole numbers of months or missing
  values

- ...:

  further arguments passed to
  [`as.Date`](https://rdrr.io/r/base/as.Date.html), such as `origin`,
  `format`, or `tz`

## Value

a vector of class `Date` having the length of the longer of `x` and `n`

## Details

Naively adding months to a date can produce invalid results. For
example, adding one month to `as.Date("2013-01-31")` returns
`"2013-02-28"` rather than a date in March.

The arguments `x` and `n` are recycled to their common maximum length
using
[`bedrock::recycle()`](https://andrisignorell.github.io/bedrock/reference/recycle.html).
Partial recycling does not produce a warning.

If either argument has length zero, an empty `Date` vector is returned.
Missing values in either argument produce missing values in the
corresponding result.

Inputs supplied as `POSIXct` are converted to `Date`, and their
time-of-day information is discarded. By default, `as.Date.POSIXct`
performs this conversion in UTC. Supply `tz` through `...` if another
time zone should determine the calendar date.

## Note

Based on code by Roland Rapold and Antonio, adapted to conform to
package standards.

## References

<https://stackoverflow.com/questions/14169620/add-a-month-to-a-date>

## See also

[`as.ym()`](as_ym.md), [`year()`](date_functions.md),
[`month()`](date_functions.md)

Other date.time: [`countWorkDays()`](countWorkDays.md),
[`date-time-predicates`](date-time-predicates.md),
[`date_functions`](date_functions.md), [`generation()`](generation.md),
[`time-conversions`](time-conversions.md), [`zodiac()`](zodiac.md)

## Examples

``` r
# character input is converted to Date
addMonths("2013-01-31", 1)
#> [1] "2013-02-28"

# negative values subtract months
addMonths(as.Date("2013-03-31"), -1)
#> [1] "2013-02-28"

# x and n are recycled
addMonths(
  c("2013-01-31", "2013-03-31", "2013-10-31", "2013-12-31"),
  c(1, -1)
)
#> [1] "2013-02-28" "2013-02-28" "2013-11-30" "2013-11-30"

# missing values propagate
addMonths(c("2013-01-31", NA), 1)
#> [1] "2013-02-28" NA          

# POSIXct input is converted to Date
x <- as.POSIXct(c("2015-01-31", "2015-08-31"), tz = "UTC")
addMonths(x, c(1, 3), tz = "UTC")
#> [1] "2015-02-28" "2015-11-30"

# numeric dates require an origin
x <- as.numeric(as.Date(c("2015-01-31", "2015-08-31")))
addMonths(x, c(1, 3), origin = as.Date("1970-01-01"))
#> [1] "2015-02-28" "2015-11-30"
```
