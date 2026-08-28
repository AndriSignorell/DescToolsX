# Frequency Table for a Single Variable

Calculates absolute and relative frequencies of a vector `x`. Continuous
(numeric) variables will be cut using the same logic as used by the
function [`hist`](https://rdrr.io/r/graphics/hist.html). Categorical
variables will be aggregated by
[`table`](https://rdrr.io/r/base/table.html). The result will contain
single and cumulative frequencies for both, absolute values and
percentages.

## Usage

``` r
freq(
  x,
  breaks = hist(x, plot = FALSE)$breaks,
  include.lowest = TRUE,
  ord = c("level", "desc", "asc", "name"),
  useNA = c("no", "ifany", "always"),
  ...
)

# S3 method for class 'Freq'
print(x, digits = NULL, ...)
```

## Arguments

- x:

  variable to be described; may be any atomic type

- breaks:

  either a numeric vector of two or more cut points or a single number
  (greater than or equal to 2) giving the number of intervals into which
  x is to be cut. Default taken from the function
  [`hist()`](https://rdrr.io/r/graphics/hist.html). If set to `FALSE`, a
  numeric `x` will not be classed at all, but tabulated directly by its
  distinct values. This is useful for discrete numeric variables such as
  counts or scores. The argument is ignored if x is not of numeric type.

- include.lowest:

  logical, indicating if an `x[i]` equal to the lowest (or highest, for
  `right = FALSE`) `"breaks"` value should be included. Ignored if x is
  not of numeric type or if `breaks = FALSE`.

- ord:

  how should the result be ordered? Default is `"level"`, other choices
  are 'by frequency' (`"desc"` or `"asc"`) or 'by name of the levels'
  (`"name"`). The argument can be abbreviated. This is ignored if x is
  numeric and classed (`breaks` not `FALSE`).

- useNA:

  one of `"no"`, `"ifany"`, or `"always"`. Defines whether to include
  extra `NA` levels in the table. Defaults to `"no"` which is the
  [`table()`](https://rdrr.io/r/base/table.html) default too.

- ...:

  further arguments are passed to the function
  [`cut()`](https://rdrr.io/r/base/cut.html). Use `dig.lab` to control
  the format of numeric group names. Use the argument `right` to define
  if the intervals should be closed on the right (and open on the left)
  or vice versa.  
  In `print.Freq` the dots are not used.

- digits:

  number of digits used to display relative frequencies

## Value

an object of class `"Freq"` inheriting from `data.frame`, with columns:

- `level`:

  levels or intervals of the tabulated variable

- `freq`:

  absolute frequencies

- `perc`:

  relative frequencies

- `cumfreq`:

  cumulative absolute frequencies

- `cumperc`:

  cumulative relative frequencies

## Details

By default only the valid cases are considered for the frequencies, say
`NA` values are excluded. (This is in accordance with the default
behavior of the R function `table`, which seemed a reasonable
reference.) If the `NA`s should be included you can set the `useNA`
argument to either `"ifany"` or `"always"`.

For numeric variables, if `breaks` is specified as a single number, the
range of the data is divided into breaks pieces of equal length, and
then the outer limits are moved away by 0.1\\ extreme values both fall
within the break intervals. (If `x` is a constant vector, equal-length
intervals are created that cover the single value.) See
[`cut`](https://rdrr.io/r/base/cut.html).

With `breaks = FALSE` no classing takes place and the distinct values of
a numeric `x` are tabulated directly (in ascending order of the values).
In this case the `ord` argument applies as for categorical variables.

## See also

[`cut`](https://rdrr.io/r/base/cut.html),
[`hist`](https://rdrr.io/r/graphics/hist.html),
[`cumsum`](https://rdrr.io/r/base/cumsum.html),
[`table`](https://rdrr.io/r/base/table.html),
[`prop.table`](https://rdrr.io/r/base/proportions.html)

Other frequency: [`expFreq()`](expFreq.md), [`freq2D()`](freq2D.md),
[`percTable()`](percTable.md), [`tOne()`](tOne.md)

## Examples

``` r

data(Pizza)

# result is a data.frame
d.freq <- freq(Pizza$price)
d.freq
#>         level  freq   perc  cumfreq  cumperc
#> 1      [0,10]     4   0.3%        4     0.3%
#> 2     (10,20]    96   8.0%      100     8.4%
#> 3     (20,30]   183  15.3%      283    23.6%
#> 4     (30,40]   147  12.3%      430    35.9%
#> 5     (40,50]   263  22.0%      693    57.9%
#> 6     (50,60]   169  14.1%      862    72.0%
#> 7     (60,70]   119   9.9%      981    82.0%
#> 8     (70,80]   109   9.1%     1090    91.1%
#> 9     (80,90]    68   5.7%     1158    96.7%
#> 10   (90,100]    22   1.8%     1180    98.6%
#> 11  (100,110]     7   0.6%     1187    99.2%
#> 12  (110,120]     6   0.5%     1193    99.7%
#> 13  (120,130]     3   0.3%     1196    99.9%
#> 14  (130,140]     1   0.1%     1197   100.0%

# it is printed by default with 3 digits for the percent values,
# but the number of digits can be defined in the print function
print(d.freq, digits=5)
#>         level  freq       perc  cumfreq     cumperc
#> 1      [0,10]     4   0.33417%        4    0.33417%
#> 2     (10,20]    96   8.02005%      100    8.35422%
#> 3     (20,30]   183  15.28822%      283   23.64244%
#> 4     (30,40]   147  12.28070%      430   35.92314%
#> 5     (40,50]   263  21.97160%      693   57.89474%
#> 6     (50,60]   169  14.11863%      862   72.01337%
#> 7     (60,70]   119   9.94152%      981   81.95489%
#> 8     (70,80]   109   9.10610%     1090   91.06099%
#> 9     (80,90]    68   5.68087%     1158   96.74185%
#> 10   (90,100]    22   1.83793%     1180   98.57978%
#> 11  (100,110]     7   0.58480%     1187   99.16458%
#> 12  (110,120]     6   0.50125%     1193   99.66583%
#> 13  (120,130]     3   0.25063%     1196   99.91646%
#> 14  (130,140]     1   0.08354%     1197  100.00000%

# sorted by frequency
freq(Pizza$driver, ord="desc")
#>        level  freq   perc  cumfreq  cumperc
#> 1  Carpenter   272  22.6%      272    22.6%
#> 2     Carter   234  19.4%      506    42.0%
#> 3     Taylor   204  16.9%      710    59.0%
#> 4     Hunter   156  13.0%      866    71.9%
#> 5     Miller   125  10.4%      991    82.3%
#> 6     Farmer   117   9.7%     1108    92.0%
#> 7    Butcher    96   8.0%     1204   100.0%

# sorted by name using all the observations, say including NAs
freq(Pizza$driver, ord="name", useNA="ifany")
#>        level  freq   perc  cumfreq  cumperc
#> 1       <NA>     5   0.4%        5     0.4%
#> 2    Butcher    96   7.9%      101     8.4%
#> 3  Carpenter   272  22.5%      373    30.9%
#> 4     Carter   234  19.4%      607    50.2%
#> 5     Farmer   117   9.7%      724    59.9%
#> 6     Hunter   156  12.9%      880    72.8%
#> 7     Miller   125  10.3%     1005    83.1%
#> 8     Taylor   204  16.9%     1209   100.0%

# percentages and cumulative frequencies for a vector of count data
freq(as.table(c(2,4,12,8)))
#>    level  freq   perc  cumfreq  cumperc
#> 1      A     2   7.7%        2     7.7%
#> 2      B     4  15.4%        6    23.1%
#> 3      C    12  46.2%       18    69.2%
#> 4      D     8  30.8%       26   100.0%

# tabulate a discrete numeric variable directly, without classing
freq(Pizza$count, breaks = FALSE)
#>    level  freq   perc  cumfreq  cumperc
#> 1      1   108   9.0%      108     9.0%
#> 2      2   259  21.6%      367    30.7%
#> 3      3   300  25.1%      667    55.7%
#> 4      4   240  20.1%      907    75.8%
#> 5      5   152  12.7%     1059    88.5%
#> 6      6    97   8.1%     1156    96.6%
#> 7      7    34   2.8%     1190    99.4%
#> 8      8     7   0.6%     1197   100.0%

# ... which also allows ordering by frequency
freq(Pizza$count, breaks = FALSE, ord = "desc")
#>    level  freq   perc  cumfreq  cumperc
#> 1      3   300  25.1%      300    25.1%
#> 2      2   259  21.6%      559    46.7%
#> 3      4   240  20.1%      799    66.8%
#> 4      5   152  12.7%      951    79.4%
#> 5      1   108   9.0%     1059    88.5%
#> 6      6    97   8.1%     1156    96.6%
#> 7      7    34   2.8%     1190    99.4%
#> 8      8     7   0.6%     1197   100.0%
```
