# Basic Date Functions

Convenience wrappers for extracting date/time components and performing
common date calculations - a readable alternative to
[`format()`](https://rdrr.io/r/base/format.html) and its cryptic format
codes.

## Usage

``` r
year(x, ...)

# S3 method for class 'ym'
year(x, ...)

# Default S3 method
year(x, ...)

month(
  x,
  fmt = c("m", "mm", "mmm"),
  lang = .getOption("lang"),
  stringsAsFactors = TRUE
)

# S3 method for class 'ym'
month(
  x,
  fmt = c("m", "mm", "mmm"),
  lang = .getOption("lang"),
  stringsAsFactors = TRUE
)

# Default S3 method
month(
  x,
  fmt = c("m", "mm", "mmm"),
  lang = .getOption("lang"),
  stringsAsFactors = TRUE
)

week(x, method = c("iso", "us"))

day(x)

day(x) <- value

weekday(
  x,
  fmt = c("d", "dd", "ddd"),
  lang = .getOption("lang"),
  stringsAsFactors = TRUE
)

quarter(x)

today()

now()

hour(x)

minute(x)

second(x)

timezone(x)

yearMonth(x)

yearWeek(x, method = c("iso", "us"))

yearDay(x)

diffDays360(startDate, endDate, method = c("eu", "us"))

lastDayOfMonth(x)

yearDays(x)

monthDays(x)

isWeekend(x)

isLeapYear(x)
```

## Arguments

- x:

  a `Date`, `POSIXct`, `POSIXlt`, or `ym` object to evaluate

- ...:

  further arguments passed to methods. `year()` is generic and carries
  them for the sake of S3 consistency; none of the methods currently
  uses them.

- fmt:

  format code controlling the output of `month()` and `weekday()`.  
  For `month()`: `"m"` = integer (default), `"mm"` = abbreviated name,
  `"mmm"` = full name.  
  For `weekday()`: `"d"` = integer (default), `"dd"` = abbreviated name,
  `"ddd"` = full name.

- lang:

  language for names returned by `month()` and `weekday()`. Either
  `"local"` (current system locale, the default) or `"en"` (English).
  Falls back to the `"lang"` option if set; otherwise `"local"` is used.

- stringsAsFactors:

  logical; if `TRUE` (default), character results from `month()` and
  `weekday()` are returned as ordered factors whose levels follow
  calendar order

- method:

  calculation convention. For `diffDays360()` either `"eu"` (European,
  default) or `"us"` (US); for `week()` and `yearWeek()` either `"iso"`
  (ISO 8601, default) or `"us"`. The two sets are not interchangeable.

- value:

  replacement value for the `day<-` assignment function

- startDate, endDate:

  start and end dates for `diffDays360()`

## Value

a vector whose type depends on the function: numeric for integer-valued
components, an ordered factor or character vector when `fmt` requests
names, logical for `isWeekend()` and `isLeapYear()`, `Date` for
`today()` and `lastDayOfMonth()`, and `POSIXct` for `now()`

## Date component extractors

|  |  |  |
|----|----|----|
| **Function** | **Returns** | **Range / Notes** |
| `year` | Year of a date or `ym` object | `yyyy` |
| `quarter` | Quarter of the year | 1-4 |
| `month` | Month of the year (numeric, abbreviated, or full name) | 1-12; S3 dispatch for `ym` |
| `week` | Week of the year | ISO 8601 or US convention |
| `day` | Day of the month (readable/writable) | 1-31 |
| `weekday` | Day of the week (numeric, abbreviated, or full name) | 1 = Mon ... 7 = Sun |
| `yearDay` | Day of the year | 1-366 |
| `yearWeek` | Compact year-week integer | `yyyyww` (ISO or US) |
| `yearMonth` | Compact year-month integer | `yyyymm` |

## Time component extractors (POSIXct/POSIXlt)

|  |  |
|----|----|
| **Function** | **Returns** |
| `hour` | Hour (0-23) |
| `minute` | Minute (0-59) |
| `second` | Second (0-60) |
| `timezone` | Time zone string |
| `now` | Current date and time ([`Sys.time()`](https://rdrr.io/r/base/Sys.time.html)) |
| `today` | Current date ([`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html)) |

## Logical tests

|              |                                           |
|--------------|-------------------------------------------|
| **Function** | **Returns**                               |
| `isWeekend`  | `TRUE` if `x` falls on Saturday or Sunday |
| `isLeapYear` | `TRUE` if the year of `x` is a leap year  |

## Date arithmetic

|  |  |
|----|----|
| **Function** | **Description** |
| `diffDays360` | Days between two dates using the 360-day calendar convention |
| `lastDayOfMonth` | Last calendar day of the month of `x` |
| `yearDays` | Total number of days in the year of `x` (365 or 366) |
| `monthDays` | Number of days in the month of `x` (28-31) |

## Language for month and weekday names

`month()` and `weekday()` respect the `"lang"` option. Set
`options(lang = "en")` to always get English names, or
`options(lang = "local")` for the current system locale. When the option
is absent, `"local"` is used as default.

## See also

[`strptime`](https://rdrr.io/r/base/strptime.html),
[`DateTimeClasses`](https://rdrr.io/r/base/DateTimeClasses.html),
[`as.POSIXlt`](https://rdrr.io/r/base/as.POSIXlt.html),
[`countWorkDays`](countWorkDays.md)

Other date.time: [`addMonths()`](AddMonths.md),
[`countWorkDays()`](countWorkDays.md),
[`date-time-predicates`](date-time-predicates.md),
[`generation()`](generation.md),
[`time-conversions`](time-conversions.md), [`zodiac()`](zodiac.md)

## Examples

``` r
x <- today()   # equivalent to Sys.Date()

year(x)
#> [1] 2026
quarter(x)
#> [1] 3

# month: numeric, abbreviated, full name
month(x)
#> [1] 8
month(x, fmt = "mm",  lang = "en")
#> [1] Aug
#> 12 Levels: Jan < Feb < Mar < Apr < May < Jun < Jul < Aug < Sep < ... < Dec
month(x, fmt = "mm",  lang = "local")
#> [1] Aug
#> 12 Levels: Jan < Feb < Mar < Apr < May < Jun < Jul < Aug < Sep < ... < Dec
month(x, fmt = "mmm", lang = "en")
#> [1] August
#> 12 Levels: January < February < March < April < May < June < ... < December
month(x, fmt = "mmm", lang = "local")
#> [1] August
#> 12 Levels: January < February < March < April < May < June < ... < December

week(x)
#> [1] 36
week(x, method = "us")
#> [1] 35

# day is both readable and writable
day(x)
#> [1] 31
day(x) <- 20
x
#> [1] "2026-08-20"

# weekday: numeric, abbreviated, full name
weekday(x)
#> [1] 4
weekday(x, fmt = "dd",  lang = "en")
#> [1] Thu
#> Levels: Mon < Tue < Wed < Thu < Fri < Sat < Sun
weekday(x, fmt = "ddd", lang = "local")
#> [1] Thursday
#> 7 Levels: Monday < Tuesday < Wednesday < Thursday < Friday < ... < Sunday

yearDay(x)
#> [1] 232
yearWeek(x)
#> [1] 202634
yearMonth(x)
#> [1] 202608

isWeekend(x)
#> [1] FALSE
isLeapYear(x)
#> [1] FALSE
isLeapYear(2000L)
#> [1] TRUE

# month names for a weekly time sequence
month(seq(Sys.Date(), Sys.Date() + 150, by = "weeks"), fmt = "mm")
#>  [1] Aug Sep Sep Sep Sep Oct Oct Oct Oct Nov Nov Nov Nov Nov Dec Dec Dec Dec Jan
#> [20] Jan Jan Jan
#> 12 Levels: Jan < Feb < Mar < Apr < May < Jun < Jul < Aug < Sep < ... < Dec

# last day of month for several dates
lastDayOfMonth(as.Date(c("2014-10-12", "2013-01-31", "2011-12-05")))
#> [1] "2014-10-31" "2013-01-31" "2011-12-31"

# days in month / year
monthDays(x)
#> [1] 31
yearDays(x)
#> [1] 365

# 360-day calendar difference. The two conventions agree here ...
diffDays360(as.Date("2023-01-31"), as.Date("2023-03-31"))
#> [1] 60
diffDays360(as.Date("2023-01-31"), as.Date("2023-03-31"), method = "us")
#> [1] 60

# ... but not here, which is the point of having both
diffDays360(as.Date("2023-01-31"), as.Date("2023-02-28"))
#> [1] 28
diffDays360(as.Date("2023-01-31"), as.Date("2023-02-28"), method = "us")
#> [1] 28

```
