# Percentage Table

Creates a 2-way contingency table along with percentages, marginal, and
conditional distributions. All the frequencies are nested into one
single table.

## Usage

``` r
percTable(...)

# Default S3 method
percTable(x, y = NULL, ...)

# S3 method for class 'formula'
percTable(formula, data, subset, na.action, ...)

# S3 method for class 'table'
percTable(
  x,
  freq = TRUE,
  prop = c("rows", "cols", "total"),
  expected = FALSE,
  ...
)

# S3 method for class 'PercTable'
print(
  x,
  margins = NULL,
  col.vars = NULL,
  row.vars = NULL,
  justify = NULL,
  blockSep = NULL,
  ...
)

# S3 method for class 'matrix'
percTable(
  x,
  freq = TRUE,
  prop = c("rows", "cols", "total"),
  expected = FALSE,
  ...
)
```

## Arguments

- ...:

  further arguments passed to `print.PercTable()`

- x:

  a table, a matrix, or a vector to be tabulated

- y:

  an optional second vector to be tabulated against `x`

- formula:

  a formula of the form `lhs ~ rhs` where `lhs` will be tabled versus
  rhs (`table(lhs, rhs)`)

- data:

  an optional matrix or data frame (or similar: see
  [`model.frame`](https://rdrr.io/r/stats/model.frame.html)) containing
  the variables in the formula `formula`. By default the variables are
  taken from `environment(formula)`.

- subset:

  an optional vector specifying a subset of observations to be used

- na.action:

  a function which indicates what should happen when the data contain
  NAs. Defaults to `getOption("na.action")`.

- freq:

  logical. Should absolute frequencies be included? Defaults to `TRUE`.

- prop:

  character vector specifying the proportions to display, using
  `"rows"`, `"cols"`, `"total"`, or `"none"`

- expected:

  logical; whether to include expected counts under independence

- margins:

  vector specifying the margins to include. Use `1` or `"rows"` for row
  margins, `2` or `"cols"` for column margins, or both; `NULL` includes
  none.

- col.vars:

  a vector of column variables (see Details). If this is left to `NULL`
  the table structure will be preserved.

- row.vars:

  a vector of row variables (see Details)

- justify:

  either `"left"` or `"right"` for defining the alignment of the table
  cells

- blockSep:

  logical, defining if an empty row should be introduced between the
  table rows. Default is FALSE, if only a table with one single
  description (either frequencies or percents) should be returned and
  `TRUE` in any other case.

## Value

an object of class `"PercTable"` containing the requested frequency and
percentage tables

## Details

PercTable prints a 2-dimensional table. The absolute and relative
frequencies are nested into one flat table by means of `ftable`.
`row.vars`, resp. `col.vars` can be used to define the structure of the
table. `row.vars` can either be the names of the dimensions (included
percentages are named `"idx"`) or numbers (1:3, where 1 is the first
dimension of the table, 2 the second and 3 the percentages).  
Use
[`sortX()`](https://andrisignorell.github.io/bedrock/reference/sortX.html)
if you want to have your table sorted by rows.  
  
The style in which numbers are formatted is selected by
[`style()`](https://andrisignorell.github.io/pharos/reference/style.html)
from the DescToolsX options. Absolute frequencies will use
`style("abs.sty")` and `style("per.sty")` will do it for the
percentages. The options can be changed with `style(abs, digits=5)`
which is basically a `"style"`-object containing any format information
used in
[`fm`](https://andrisignorell.github.io/pharos/reference/fm.html).

`margins` adds the marginal distributions. In the frequency table these
are the usual row/column sums; in the percentage tables the margin holds
the *marginal distribution*, i.e. the row resp. column sums of the
frequency table divided by the grand total. A margin is only shown where
it carries information: the sum column of the row percentages is
\\100\\\\ by construction and the sum row of the row percentages is not
a distribution at all, so whichever of the two is uninformative is
printed as `"."`.

## References

Agresti, Alan (2007) *Introduction to categorical data analysis*. NY:
John Wiley and Sons, Section 2.4.5  

## See also

[table](https://rdrr.io/r/base/table.html),
[ftable](https://rdrr.io/r/stats/ftable.html),
[proportions](https://rdrr.io/r/base/proportions.html),
[addmargins](https://rdrr.io/r/stats/addmargins.html),
[setDescToolsXOption](setDescToolsXOption.md),
[pharos::style](https://andrisignorell.github.io/pharos/reference/style.html)  
There are similar functions in sfsmisc::printTable2 and package vcd
vcd::table2d_summary, both lacking some of the flexibility we needed
here.

Other frequency: [`expFreq()`](expFreq.md), [`freq()`](freq.md),
[`freq2D()`](freq2D.md), [`tOne()`](tOne.md)

## Examples

``` r

tab <- as.table(apply(HairEyeColor, c(1,2), sum))

percTable(tab, col.vars=2)
#>                Brown    Blue   Hazel   Green
#>                                             
#> Black freq        68      20      15       5
#>       perc     11.5%    3.4%    2.5%    0.8%
#>       p.row    63.0%   18.5%   13.9%    4.6%
#>       p.col    30.9%    9.3%   16.1%    7.8%
#> 
#> Brown freq       119      84      54      29
#>       perc     20.1%   14.2%    9.1%    4.9%
#>       p.row    41.6%   29.4%   18.9%   10.1%
#>       p.col    54.1%   39.1%   58.1%   45.3%
#> 
#> Red   freq        26      17      14      14
#>       perc      4.4%    2.9%    2.4%    2.4%
#>       p.row    36.6%   23.9%   19.7%   19.7%
#>       p.col    11.8%    7.9%   15.1%   21.9%
#> 
#> Blond freq         7      94      10      16
#>       perc      1.2%   15.9%    1.7%    2.7%
#>       p.row     5.5%   74.0%    7.9%   12.6%
#>       p.col     3.2%   43.7%   10.8%   25.0% 

percTable(tab, col.vars=2, margins=c(1,2))
#>                 Brown     Blue    Hazel    Green      Sum
#>                                                          
#> Black freq         68       20       15        5      108
#>       perc      11.5%     3.4%     2.5%     0.8%    18.2%
#>       p.row     63.0%    18.5%    13.9%     4.6%        .
#>       p.col     30.9%     9.3%    16.1%     7.8%        .
#> 
#> Brown freq        119       84       54       29      286
#>       perc      20.1%    14.2%     9.1%     4.9%    48.3%
#>       p.row     41.6%    29.4%    18.9%    10.1%        .
#>       p.col     54.1%    39.1%    58.1%    45.3%        .
#> 
#> Red   freq         26       17       14       14       71
#>       perc       4.4%     2.9%     2.4%     2.4%    12.0%
#>       p.row     36.6%    23.9%    19.7%    19.7%        .
#>       p.col     11.8%     7.9%    15.1%    21.9%        .
#> 
#> Blond freq          7       94       10       16      127
#>       perc       1.2%    15.9%     1.7%     2.7%    21.5%
#>       p.row      5.5%    74.0%     7.9%    12.6%        .
#>       p.col      3.2%    43.7%    10.8%    25.0%        .
#> 
#> Sum   freq        220      215       93       64      592
#>       perc      37.2%    36.3%    15.7%    10.8%   100.0%
#>       p.row         .        .        .        .        .
#>       p.col         .        .        .        .        . 
percTable(tab, col.vars=2, margins=2)
#>                Brown    Blue   Hazel   Green     Sum
#>                                                     
#> Black freq        68      20      15       5     108
#>       perc     11.5%    3.4%    2.5%    0.8%   18.2%
#>       p.row    63.0%   18.5%   13.9%    4.6%       .
#>       p.col    30.9%    9.3%   16.1%    7.8%       .
#> 
#> Brown freq       119      84      54      29     286
#>       perc     20.1%   14.2%    9.1%    4.9%   48.3%
#>       p.row    41.6%   29.4%   18.9%   10.1%       .
#>       p.col    54.1%   39.1%   58.1%   45.3%       .
#> 
#> Red   freq        26      17      14      14      71
#>       perc      4.4%    2.9%    2.4%    2.4%   12.0%
#>       p.row    36.6%   23.9%   19.7%   19.7%       .
#>       p.col    11.8%    7.9%   15.1%   21.9%       .
#> 
#> Blond freq         7      94      10      16     127
#>       perc      1.2%   15.9%    1.7%    2.7%   21.5%
#>       p.row     5.5%   74.0%    7.9%   12.6%       .
#>       p.col     3.2%   43.7%   10.8%   25.0%       . 
percTable(tab, col.vars=2, margins=1)
#>                Brown    Blue   Hazel   Green
#>                                             
#> Black freq        68      20      15       5
#>       perc     11.5%    3.4%    2.5%    0.8%
#>       p.row    63.0%   18.5%   13.9%    4.6%
#>       p.col    30.9%    9.3%   16.1%    7.8%
#> 
#> Brown freq       119      84      54      29
#>       perc     20.1%   14.2%    9.1%    4.9%
#>       p.row    41.6%   29.4%   18.9%   10.1%
#>       p.col    54.1%   39.1%   58.1%   45.3%
#> 
#> Red   freq        26      17      14      14
#>       perc      4.4%    2.9%    2.4%    2.4%
#>       p.row    36.6%   23.9%   19.7%   19.7%
#>       p.col    11.8%    7.9%   15.1%   21.9%
#> 
#> Blond freq         7      94      10      16
#>       perc      1.2%   15.9%    1.7%    2.7%
#>       p.row     5.5%   74.0%    7.9%   12.6%
#>       p.col     3.2%   43.7%   10.8%   25.0%
#> 
#> Sum   freq       220     215      93      64
#>       perc     37.2%   36.3%   15.7%   10.8%
#>       p.row        .       .       .       .
#>       p.col        .       .       .       . 
percTable(tab, col.vars=2, margins=NULL)
#>                Brown    Blue   Hazel   Green
#>                                             
#> Black freq        68      20      15       5
#>       perc     11.5%    3.4%    2.5%    0.8%
#>       p.row    63.0%   18.5%   13.9%    4.6%
#>       p.col    30.9%    9.3%   16.1%    7.8%
#> 
#> Brown freq       119      84      54      29
#>       perc     20.1%   14.2%    9.1%    4.9%
#>       p.row    41.6%   29.4%   18.9%   10.1%
#>       p.col    54.1%   39.1%   58.1%   45.3%
#> 
#> Red   freq        26      17      14      14
#>       perc      4.4%    2.9%    2.4%    2.4%
#>       p.row    36.6%   23.9%   19.7%   19.7%
#>       p.col    11.8%    7.9%   15.1%   21.9%
#> 
#> Blond freq         7      94      10      16
#>       perc      1.2%   15.9%    1.7%    2.7%
#>       p.row     5.5%   74.0%    7.9%   12.6%
#>       p.col     3.2%   43.7%   10.8%   25.0% 

percTable(tab, col.vars=2, prop="none")
#>       Eye Brown  Blue Hazel Green
#> Hair                             
#> Black        68    20    15     5
#> Brown       119    84    54    29
#> Red          26    17    14    14
#> Blond         7    94    10    16 

# just the percentages without absolute values
percTable(tab, col.vars=2, prop=c("total","rows"), freq=FALSE)
#>                Brown    Blue   Hazel   Green
#>                                             
#> Black perc     11.5%    3.4%    2.5%    0.8%
#>       p.row    63.0%   18.5%   13.9%    4.6%
#> 
#> Brown perc     20.1%   14.2%    9.1%    4.9%
#>       p.row    41.6%   29.4%   18.9%   10.1%
#> 
#> Red   perc      4.4%    2.9%    2.4%    2.4%
#>       p.row    36.6%   23.9%   19.7%   19.7%
#> 
#> Blond perc      1.2%   15.9%    1.7%    2.7%
#>       p.row     5.5%   74.0%    7.9%   12.6% 

# just the row percentages
percTable(tab, freq= FALSE, prop="rows")
#>       Eye   Brown    Blue   Hazel   Green
#> Hair                                     
#> Black       63.0%   18.5%   13.9%    4.6%
#> Brown       41.6%   29.4%   18.9%   10.1%
#> Red         36.6%   23.9%   19.7%   19.7%
#> Blond        5.5%   74.0%    7.9%   12.6% 

# just the expected frequencies
percTable(tab, prop="none", expected = TRUE)
#>                 Brown  Blue Hazel Green
#>                                        
#> Black freq         68    20    15     5
#>       expected     40    39    17    12
#> 
#> Brown freq        119    84    54    29
#>       expected    106   104    45    31
#> 
#> Red   freq         26    17    14    14
#>       expected     26    26    11     8
#> 
#> Blond freq          7    94    10    16
#>       expected     47    46    20    14 


# rearrange output such that freq are inserted as columns instead of rows
percTable(tab, col.vars=c(3,2))
#>           freq                            perc                           p.row                           p.col                        
#>          Brown    Blue   Hazel   Green   Brown    Blue   Hazel   Green   Brown    Blue   Hazel   Green   Brown    Blue   Hazel   Green
#>                                                                                                                                       
#> Black       68      20      15       5   11.5%    3.4%    2.5%    0.8%   63.0%   18.5%   13.9%    4.6%   30.9%    9.3%   16.1%    7.8%
#> 
#> Brown      119      84      54      29   20.1%   14.2%    9.1%    4.9%   41.6%   29.4%   18.9%   10.1%   54.1%   39.1%   58.1%   45.3%
#> 
#> Red         26      17      14      14    4.4%    2.9%    2.4%    2.4%   36.6%   23.9%   19.7%   19.7%   11.8%    7.9%   15.1%   21.9%
#> 
#> Blond        7      94      10      16    1.2%   15.9%    1.7%    2.7%    5.5%   74.0%    7.9%   12.6%    3.2%   43.7%   10.8%   25.0% 

# putting the areas in rows
percTable(tab, col.vars=c(3,1), prop="total", margins=c(1,2))
#>            freq                                         perc                                    
#>           Black    Brown      Red    Blond      Sum    Black    Brown      Red    Blond      Sum
#>                                                                                                 
#> Brown        68      119       26        7      220    11.5%    20.1%     4.4%     1.2%    37.2%
#> 
#> Blue         20       84       17       94      215     3.4%    14.2%     2.9%    15.9%    36.3%
#> 
#> Hazel        15       54       14       10       93     2.5%     9.1%     2.4%     1.7%    15.7%
#> 
#> Green         5       29       14       16       64     0.8%     4.9%     2.4%     2.7%    10.8%
#> 
#> Sum         108      286       71      127      592    18.2%    48.3%    12.0%    21.5%   100.0% 

# formula interface with subset
percTable(driver ~ area, data=Pizza, subset=wine_delivered==0)
#>                    Brent  Camden Westminster
#>                                             
#> Butcher   freq        65       1          18
#>           perc      6.4%    0.1%        1.8%
#>           p.row    77.4%    1.2%       21.4%
#>           p.col    15.2%    0.3%        5.8%
#> 
#> Carpenter freq        27      14         170
#>           perc      2.6%    1.4%       16.7%
#>           p.row    12.8%    6.6%       80.6%
#>           p.col     6.3%    4.9%       55.2%
#> 
#> Carter    freq       161      42           4
#>           perc     15.8%    4.1%        0.4%
#>           p.row    77.8%   20.3%        1.9%
#>           p.col    37.7%   14.7%        1.3%
#> 
#> Farmer    freq        19      72           9
#>           perc      1.9%    7.1%        0.9%
#>           p.row    19.0%   72.0%        9.0%
#>           p.col     4.4%   25.2%        2.9%
#> 
#> Hunter    freq       113       4          22
#>           perc     11.1%    0.4%        2.2%
#>           p.row    81.3%    2.9%       15.8%
#>           p.col    26.5%    1.4%        7.1%
#> 
#> Miller    freq         6      35          67
#>           perc      0.6%    3.4%        6.6%
#>           p.row     5.6%   32.4%       62.0%
#>           p.col     1.4%   12.2%       21.8%
#> 
#> Taylor    freq        36     118          18
#>           perc      3.5%   11.6%        1.8%
#>           p.row    20.9%   68.6%       10.5%
#>           p.col     8.4%   41.3%        5.8% 

# sort the table by rows, order first column (Zurich), then third, then row.names (0)
percTable(sortX(tab, ord=c(1,3,0)))
#>                Brown    Blue   Hazel   Green
#>                                             
#> Blond freq         7      94      10      16
#>       perc      1.2%   15.9%    1.7%    2.7%
#>       p.row     5.5%   74.0%    7.9%   12.6%
#>       p.col     3.2%   43.7%   10.8%   25.0%
#> 
#> Red   freq        26      17      14      14
#>       perc      4.4%    2.9%    2.4%    2.4%
#>       p.row    36.6%   23.9%   19.7%   19.7%
#>       p.col    11.8%    7.9%   15.1%   21.9%
#> 
#> Black freq        68      20      15       5
#>       perc     11.5%    3.4%    2.5%    0.8%
#>       p.row    63.0%   18.5%   13.9%    4.6%
#>       p.col    30.9%    9.3%   16.1%    7.8%
#> 
#> Brown freq       119      84      54      29
#>       perc     20.1%   14.2%    9.1%    4.9%
#>       p.row    41.6%   29.4%   18.9%   10.1%
#>       p.col    54.1%   39.1%   58.1%   45.3% 

# reverse the row variables, so that absolute frequencies and percents
# are not nested together
percTable(tab, row.vars=c(3, 1))
#>                Brown    Blue   Hazel   Green
#>                                             
#> freq  Black       68      20      15       5
#>       Brown      119      84      54      29
#>       Red         26      17      14      14
#>       Blond        7      94      10      16
#> 
#> perc  Black    11.5%    3.4%    2.5%    0.8%
#>       Brown    20.1%   14.2%    9.1%    4.9%
#>       Red       4.4%    2.9%    2.4%    2.4%
#>       Blond     1.2%   15.9%    1.7%    2.7%
#> 
#> p.row Black    63.0%   18.5%   13.9%    4.6%
#>       Brown    41.6%   29.4%   18.9%   10.1%
#>       Red      36.6%   23.9%   19.7%   19.7%
#>       Blond     5.5%   74.0%    7.9%   12.6%
#> 
#> p.col Black    30.9%    9.3%   16.1%    7.8%
#>       Brown    54.1%   39.1%   58.1%   45.3%
#>       Red      11.8%    7.9%   15.1%   21.9%
#>       Blond     3.2%   43.7%   10.8%   25.0% 

# the vector interface
percTable(x=Pizza$driver, y=Pizza$area)
#>                    Brent  Camden Westminster
#>                                             
#> Butcher   freq        72       1          22
#>           perc      6.0%    0.1%        1.8%
#>           p.row    75.8%    1.1%       23.2%
#>           p.col    15.2%    0.3%        5.8%
#> 
#> Carpenter freq        29      19         221
#>           perc      2.4%    1.6%       18.5%
#>           p.row    10.8%    7.1%       82.2%
#>           p.col     6.1%    5.6%       58.2%
#> 
#> Carter    freq       177      47           5
#>           perc     14.8%    3.9%        0.4%
#>           p.row    77.3%   20.5%        2.2%
#>           p.col    37.4%   13.8%        1.3%
#> 
#> Farmer    freq        19      87          11
#>           perc      1.6%    7.3%        0.9%
#>           p.row    16.2%   74.4%        9.4%
#>           p.col     4.0%   25.5%        2.9%
#> 
#> Hunter    freq       128       4          24
#>           perc     10.7%    0.3%        2.0%
#>           p.row    82.1%    2.6%       15.4%
#>           p.col    27.1%    1.2%        6.3%
#> 
#> Miller    freq         6      41          77
#>           perc      0.5%    3.4%        6.4%
#>           p.row     4.8%   33.1%       62.1%
#>           p.col     1.3%   12.0%       20.3%
#> 
#> Taylor    freq        42     142          20
#>           perc      3.5%   11.9%        1.7%
#>           p.row    20.6%   69.6%        9.8%
#>           p.col     8.9%   41.6%        5.3% 
percTable(x=Pizza$driver, y=Pizza$area, prop="rows", 
margins=c("rows","cols"))
#>                     Brent   Camden Westminster      Sum
#>                                                        
#> Butcher   freq         72        1          22       95
#>           p.row     75.8%     1.1%       23.2%     8.0%
#> 
#> Carpenter freq         29       19         221      269
#>           p.row     10.8%     7.1%       82.2%    22.5%
#> 
#> Carter    freq        177       47           5      229
#>           p.row     77.3%    20.5%        2.2%    19.2%
#> 
#> Farmer    freq         19       87          11      117
#>           p.row     16.2%    74.4%        9.4%     9.8%
#> 
#> Hunter    freq        128        4          24      156
#>           p.row     82.1%     2.6%       15.4%    13.1%
#> 
#> Miller    freq          6       41          77      124
#>           p.row      4.8%    33.1%       62.1%    10.4%
#> 
#> Taylor    freq         42      142          20      204
#>           p.row     20.6%    69.6%        9.8%    17.1%
#> 
#> Sum       freq        473      341         380     1194
#>           p.row     39.6%    28.6%       31.8%   100.0% 
```
