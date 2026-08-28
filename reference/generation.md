# Generation by Birth Year

Yields the generation of a person based on the year of birth.

## Usage

``` r
generation(year)
```

## Arguments

- year:

  year of birth

## Value

ordered factor with levels
`c("Babyboomer", "Gen X", "Millennial", "Gen Z", "Gen Alpha")`  
Values which fall outside the range of breaks are coded as `NA`, as are
`NaN` and `NA` values.

## Details

The generations are defined as:  

|                    |                                          |
|--------------------|------------------------------------------|
| **birth year** ` ` | **label**                                |
| `1946-1964` ` `    | Babyboomer                               |
| `1965-1979`        | Generation X                             |
| `1980-1995`        | Generation Y - also known as Millennials |
| `1996-2010`        | Generation Z                             |
| `2011 and later`   | Generation Alpha                         |

The last class is left open at the top. The table formerly gave it as
1946-2025 while the code used `Inf`, so a birth year of 2026 was
documented as `NA` and returned as `"Gen Alpha"`. Naming the successor
generation is not settled enough to hard-code an upper bound.

## See also

[`cutAge`](cutAge.md)

Other date.time: [`addMonths()`](AddMonths.md),
[`countWorkDays()`](countWorkDays.md),
[`date-time-predicates`](date-time-predicates.md),
[`date_functions`](date_functions.md),
[`time-conversions`](time-conversions.md), [`zodiac()`](zodiac.md)

## Examples

``` r

generation(c(1946, 1964, 1972, 2001, 2003, 2018, 2026))
#> [1] Babyboomer Babyboomer Gen X      Gen Z      Gen Z      Gen Alpha  Gen Alpha 
#> Levels: Babyboomer < Gen X < Millennial < Gen Z < Gen Alpha
```
