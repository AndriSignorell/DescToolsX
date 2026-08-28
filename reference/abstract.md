# Display Compact Abstract of a Data Frame

Compactly display the content and structure of a `data.frame`, including
variable labels. [`str()`](https://rdrr.io/r/utils/str.html) is
optimized for lists and its output is relatively technical, when it
comes to e.g. attributes.
[`summary()`](https://rdrr.io/r/base/summary.html) on the other hand
already calculates some basic statistics.

## Usage

``` r
abstract(
  x,
  sep = ", ",
  zeroForm = ".",
  maxLevels = 5,
  maxVars = Inf,
  truncate = TRUE
)

# S3 method for class 'Abstract'
print(x, width = NULL, truncate = NULL, print.gap = 2, ...)
```

## Arguments

- x:

  a `data.frame` to be described

- sep:

  the separator for concatenating the levels of a factor

- zeroForm:

  a symbol to be used when a variable has zero NAs

- maxLevels:

  integer; maximum number of factor levels to display. Default is 5. Set
  this to `Inf` if all levels are needed.

- maxVars:

  integer; maximum number of variables (rows) to display. Default is
  `Inf`, meaning all variables.

- truncate:

  logical; whether level names exceeding the column width should be
  truncated. Default is `TRUE`.

- width:

  console width. If `NULL`, defaults to
  [options("width")](https://rdrr.io/r/base/options.html).

- print.gap:

  integer; number of spaces between columns

- ...:

  further arguments passed to the `print` method

## Value

a data frame of class `Abstract` with columns:

- `Nr`:

  column number

- `Class`:

  column class

- `ColName`:

  column name

- `NAs`:

  number of missing values

- `Levels`:

  factor levels, if applicable

- `Label`:

  descriptive column label

When printing, the `Label` column is hidden if no labels are set.

## Details

The levels of a factor and describing variable labels (as created by
[`label()`](https://andrisignorell.github.io/bedrock/reference/Label.html))
will be wrapped within the columns.

The first 4 columns are printed with the needed fix width, the last 2
(Levels and Labels) are wrapped within the column. The width is
calculated depending on the width of the screen as given by
`getOption("width")`.

`toWord` has an interface for the class `Abstract`.

## See also

[`utils::str()`](https://rdrr.io/r/utils/str.html),
[`base::summary()`](https://rdrr.io/r/base/summary.html),
[`columnWrap()`](https://andrisignorell.github.io/bedrock/reference/columnWrap.html),
[`desc()`](Desc.md)

Other data.inspection: [`outlier()`](outlier.md)

## Examples

``` r

d.mydata <- CO2

# let's use describing labels
label(d.mydata) <- "CO2 contains data from an experiment on the cold
tolerance of the grass species Echinochloa crus-galli."

label(d.mydata$Plant) <- "an ordered factor with levels Qn1 < Qn2 < Qn3 < ... < Mc1
giving a unique identifier for each plant."

label(d.mydata$Type) <- "a factor with levels Quebec Mississippi giving the
origin of the plant"

abstract(d.mydata)
#> ────────────────────────────────────────────────────────────────────────────── 
#> d.mydata :
#>   CO2 contains data from an experiment on the cold tolerance of the
#>   grass species Echinochloa crus-galli.
#> 
#> data frame:  84 obs. of  5 variables
#>      84 complete cases (100.0%)
#> 
#>   Nr  Class  ColName    NAs  Levels                Label                
#>   1   ord    Plant      .    (12): 1-Qn1, 2-Qn2,   an ordered factor    
#>                              3-Qn3, 4-Qc1, 5-Qc3,  with levels Qn1 < Qn2
#>                              ...                   < Qn3 < ... < Mc1    
#>                                                    giving a unique      
#>                                                    identifier for each  
#>                                                    plant.               
#>   2   fac    Type       .    (2): 1-Quebec,        a factor with levels 
#>                              2-Mississippi         Quebec Mississippi   
#>                                                    giving the origin of 
#>                                                    the plant            
#>   3   fac    Treatment  .    (2): 1-nonchilled,    -                    
#>                              2-chilled                                  
#>   4   num    conc       .                          -                    
#>   5   num    uptake     .                          -                    
#> 
```
