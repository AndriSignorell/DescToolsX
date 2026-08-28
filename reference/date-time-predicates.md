# Date and Time Class Predicates

Check whether an object contains a date component, a time component, or
both, and determine whether its observed times of day vary.

## Usage

``` r
isDate(x)

isTime(x)

isDateTime(x)

hasVaryingTime(x, na.rm = FALSE)
```

## Arguments

- x:

  an object to examine

- na.rm:

  logical; if `TRUE`, missing values are removed before the time
  components are compared (`hasVaryingTime` only)

## Value

for `isDate`, `isTime`, and `isDateTime`, a single logical value.
`hasVaryingTime` likewise returns a single logical value, or `NA` if `x`
contains missing values and `na.rm = FALSE`.

## Details

The predicates inspect the class of `x`; character strings are not
parsed or converted. In addition to the base R classes `"Date"`,
`"POSIXct"`, and `"POSIXlt"`, the legacy classes `"dates"`, `"times"`,
and `"chron"` are supported.

`isDate` returns `TRUE` for objects containing a date component,
including date-time objects. `isTime` returns `TRUE` for objects
containing a time component, including date-time objects. `isDateTime`
identifies classes containing both components, and is by construction
equivalent to `isDate(x) && isTime(x)`.

The chron classes need care, because `"dates"` inherits from `"times"`:
a pure date object therefore carries the `"times"` class without having
a time component. `isTime` accounts for this, so that `"dates"` counts
as date-only, `"times"` as time-only, and `"chron"` as both.

`hasVaryingTime` compares the observed times of day while ignoring
differences in the date component. It returns `FALSE` for objects
without a time component and when fewer than two non-missing times are
available.

For date-time objects the comparison uses local clock time, which is
usually what matters when deciding whether times need to be displayed at
all. Where a daylight saving transition falls inside `x`, clock time and
elapsed time come apart: two instants an hour apart can share the same
clock time in the repeated hour of an autumn transition, and are then
reported as not varying.

## See also

[`inherits`](https://rdrr.io/r/base/class.html),
[`DateTimeClasses`](https://rdrr.io/r/base/DateTimeClasses.html)

Other date.time: [`addMonths()`](AddMonths.md),
[`countWorkDays()`](countWorkDays.md),
[`date_functions`](date_functions.md), [`generation()`](generation.md),
[`time-conversions`](time-conversions.md), [`zodiac()`](zodiac.md)

## Examples

``` r
d <- as.Date(c("2024-01-01", "2024-01-02"))
dt <- as.POSIXct(
  c("2024-01-01 08:00:00", "2024-01-02 09:30:00"),
  tz = "UTC"
)

isDate(d)
#> [1] TRUE
isTime(d)
#> [1] FALSE
isDateTime(d)
#> [1] FALSE

isDate(dt)
#> [1] TRUE
isTime(dt)
#> [1] TRUE
isDateTime(dt)
#> [1] TRUE

# character strings are not interpreted as dates
isDate("2024-01-01")
#> [1] FALSE

hasVaryingTime(dt)
#> [1] TRUE

sameTime <- as.POSIXct(
  c("2024-01-01 08:00:00", "2024-01-02 08:00:00"),
  tz = "UTC"
)
hasVaryingTime(sameTime)
#> [1] FALSE

# missing values propagate unless they are removed
withNA <- dt
withNA[2] <- NA
hasVaryingTime(withNA)
#> [1] NA
hasVaryingTime(withNA, na.rm = TRUE)
#> [1] FALSE
```
