# Describe a Factor

Compute descriptive statistics for a factor or character vector. The
plot method displays absolute and relative frequencies in horizontal bar
plots.

## Usage

``` r
# S3 method for class 'factor'
desc(
  x,
  maxrows = NULL,
  ord = NULL,
  conf.level = 0.95,
  main = NULL,
  verbose = NULL,
  plotit = NULL,
  digits = NULL,
  ...
)

# S3 method for class 'character'
desc(
  x,
  maxrows = NULL,
  ord = NULL,
  conf.level = 0.95,
  main = NULL,
  verbose = NULL,
  plotit = NULL,
  digits = NULL,
  ...
)

# S3 method for class 'Desc.factor'
print(x, digits = NULL, ...)
```

## Arguments

- x:

  factor, ordered factor, or character vector to describe

- maxrows:

  numeric; defines the maximum number of rows in a frequency table to be
  reported. For factors with many levels it is often not interesting to
  see all of them. Default is set to 12 most frequent ones (resp. the
  first ones if `ord` is set to `"levels"` or `"names"`).

  For a numeric argument x `maxrows` is the minimum number of unique
  values needed for a numeric variable to be treated as continuous. If
  left to its default `NULL`, x will be regarded as continuous if it has
  more than 12 single values. In this case the list of extreme values
  will be displayed and the frequency table else.

  If `maxrows` is \< 1 it will be interpreted as percentage. In this
  case just as many rows, as the `maxrows` most frequent levels will be
  shown. Say, if `maxrows` is set to `0.8`, then the number of rows is
  fixed so, that the highest cumulative relative frequency is the first
  one going beyond 0.8.

  Setting `maxrows` to `Inf` will unconditionally report all values and
  also produce a plot with type "h" instead of a histogram.

- ord:

  character out of `"name"` (alphabetical order), `"level"`, `"asc"` (by
  frequencies ascending), `"desc"` (by frequencies descending) defining
  the order for a frequency table as used for factors, numerics with few
  unique values and logicals. Factors (and character vectors) are by
  default ordered by their descending frequencies, ordered factors by
  their natural order.

- conf.level:

  confidence level of the interval (default 0.95). If set to `NA`, no
  confidence interval is calculated.

- main:

  character string, `NULL`, or `NA`, defining the main title. By default
  (`main = NULL`) the title will be composed as: (\<class(es)\>). If
  `NA`, no title is printed.

- verbose:

  integer controlling verbosity of table output. One of `1` (minimal),
  `2` (default), `3` (extensive). Applies to tables only.

- plotit:

  logical. Should a plot be created? The plot type depends on the
  classes of the variables. Default can be defined by the option
  `plotit`, if it does not exist then it's set to `TRUE`.

- digits:

  number of digits used to format relative frequencies; the default can
  be set with `setDescToolsXOption(digits = x)`

- ...:

  further arguments passed to methods

## Value

an object of class `c("Desc.factor", "Desc")` containing counts,
frequency information, and metadata

## Details

This function produces a rich description of a **factor**, containing
length, number of NAs, number of levels and detailed frequencies of all
levels. The order of the frequency table can be chosen between
descending/ascending frequency, labels or levels. For ordered factors
the order default is `"level"`. Character vectors are treated as
unordered factors. `desc.character()` converts `x` to a factor and then
uses the factor method. Ordered factors use their intrinsic level order
rather than descending frequency order.

## See also

[`plotCatDist`](https://andrisignorell.github.io/pharos/reference/plotCatDist.html)
for graphical display

Other desc: [`desc()`](Desc.md), [`desc.Date()`](Desc.Date.md),
[`desc.nn`](Desc.nn.md), [`desc.nq`](desc.nq.md),
[`desc.numeric()`](desc.numeric.md), [`desc.qn`](desc.qn.md),
[`desc.qq`](desc.qq.md), [`desc.ts()`](desc.ts.md),
[`print.Desc.qq()`](desc.table.md)
