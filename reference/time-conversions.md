# Convert h:m:s To/From seconds

`hmsToSec` - Converts a vector of h:m:s to seconds.

## Usage

``` r
hmsToMinute(x)

hmsToSec(x)

secToHms(x, digits = NULL)
```

## Arguments

- x:

  date-time object for `hmsToMinute()`, vector of times in h:m:s format
  for `hmsToSec()`, or numeric vector of seconds for `secToHms()`

- digits:

  the number of digits to use for potential fractions of seconds

## Value

depending on the function:

- `hmsToMinute()`:

  numeric vector of times in minutes

- `hmsToSec()`:

  numeric vector of times in seconds

- `secToHms()`:

  character vector of times in h:m:s format

\#'

## Details

`secToHms` - Converts a vector of seconds to h:m:s.

## Note

Based on code by Tyler Rinker, adapted to conform to package standards.

## See also

`times`

Other date.time: [`addMonths()`](AddMonths.md),
[`countWorkDays()`](countWorkDays.md),
[`date-time-predicates`](date-time-predicates.md),
[`date_functions`](date_functions.md), [`generation()`](generation.md),
[`zodiac()`](zodiac.md)

## Examples

``` r

hmsToSec(c("02:00:03", "04:03:01"))
#> [1]  7203 14581
hmsToSec(secToHms(c(222, 1234, 55)))
#> [1]  222 1234   55
secToHms(c(256, 3456, 56565))
#> [1] "00:04:16" "00:57:36" "15:42:45"

```
