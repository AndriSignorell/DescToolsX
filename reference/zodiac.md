# Calculate the Zodiac of a Date

Calculate the sign of zodiac of a date.

## Usage

``` r
zodiac(x, lang = c("en", "de"), stringsAsFactors = TRUE)
```

## Arguments

- x:

  the date to transform, a `Date` or anything that
  [`month()`](date_functions.md) and [`day()`](date_functions.md) accept

- lang:

  language of the zodiac names, either English (`"en"`) or German
  (`"de"`)

- stringsAsFactors:

  logical. If set to `TRUE` (default) the result will consist of a
  factor with zodiac signs as levels.

## Value

a character vector or factor containing the zodiac signs

## Details

The really relevant things can sometimes hardly be found. You just
discovered such a function... ;-)

The following rule to determine zodiac symbols is implemented:

    Dec. 22 - Jan. 19 : Capricorn | Jan. 20 - Feb. 17 : Aquarius |
    Feb. 18 - Mar. 19 : Pisces | March 20 - April 19 : Aries | April 20 - May 19 :
    Taurus | May 20 - June 20 : Gemini | June 21 - July 21 : Cancer | July 22 - Aug.
    22 : Leo | Aug 23 - Sept. 21 : Virgo | Sept. 22 - Oct. 22 : Libra | Oct. 23 -
    Nov. 21 : Scorpio | Nov. 22 - Dec. 21 : Sagittarius 

The boundaries are fixed calendar dates; the astronomical dates of the
sun's entry into a sign shift by up to a day from year to year.

## Note

Based on code from Markus Naepflin, adapted to conform to package
standards.

## See also

Other date.time: [`addMonths()`](AddMonths.md),
[`countWorkDays()`](countWorkDays.md),
[`date-time-predicates`](date-time-predicates.md),
[`date_functions`](date_functions.md), [`generation()`](generation.md),
[`time-conversions`](time-conversions.md)

## Examples

``` r

zodiac(as.Date(c("1937-07-28", "1936-06-01", "1966-02-25",
                 "1964-11-17", "1972-04-25")), lang="de")
#> [1] Loewe     Zwillinge Fische    Skorpion  Stier    
#> 12 Levels: Steinbock Wassermann Fische Widder Stier Zwillinge Krebs ... Schuetze

# the boundary days
zodiac(as.Date(c("2015-01-19", "2015-01-20", "2015-12-21", "2015-12-22")))
#> [1] Capricorn   Aquarius    Sagittarius Capricorn  
#> 12 Levels: Capricorn Aquarius Pisces Aries Taurus Gemini Cancer Leo ... Sagittarius

set.seed(1)
d <- sample(seq(as.Date("2015-01-01"), as.Date("2015-12-31"), 1), 120)
z <- zodiac(d)
desc(z)
#> ────────────────────────────────────────────────────────────────────────────── 
#> z (factor)
#> 
#>   length      n    NAs unique levels  dupes
#>      120    120      0     12     12      y
#>          100.0%   0.0%                     
#> 
#>           level  freq   perc  cumfreq  cumperc
#> 1      Aquarius    14  11.7%       14    11.7%
#> 2         Aries    12  10.0%       26    21.7%
#> 3           Leo    12  10.0%       38    31.7%
#> 4         Libra    12  10.0%       50    41.7%
#> 5       Scorpio    11   9.2%       61    50.8%
#> 6        Taurus    10   8.3%       71    59.2%
#> 7        Gemini    10   8.3%       81    67.5%
#> 8   Sagittarius    10   8.3%       91    75.8%
#> 9        Pisces     9   7.5%      100    83.3%
#> 10        Virgo     8   6.7%      108    90.0%
#> 11    Capricorn     6   5.0%      114    95.0%
#> 12       Cancer     6   5.0%      120   100.0%
#> 

```
