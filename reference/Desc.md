# Describe Data

Produce summaries of various types of variables. Descriptive statistics
and plots are chosen automatically depending on the class of `x`. The
intention is to provide a fast but rich summary with minimal typing.

Compute descriptive statistics for a dichotomous variable. The plot
method displays absolute and relative frequencies in horizontal bar
plots.

## Usage

``` r
desc(x, ...)

# S3 method for class 'list'
desc(x, ...)

# S3 method for class 'Desc.list'
print(x, ...)

# S3 method for class 'data.frame'
desc(x, ...)

# S3 method for class 'Desc'
print(x, ...)

# S3 method for class 'Desc'
plot(x, ...)

# S3 method for class 'Desc.AllNA'
print(x, ...)

# S3 method for class 'Desc.AllNA'
plot(x, ...)

# S3 method for class 'Desc.factor'
plot(x, ...)

# S3 method for class 'formula'
desc(
  formula,
  data,
  subset,
  na.action = na.pass,
  main = NULL,
  verbose = NULL,
  plotit = NULL,
  ...
)

# S3 method for class 'logical'
desc(
  x,
  ord = "level",
  conf.level = 0.95,
  include_x = TRUE,
  main = NULL,
  verbose = NULL,
  plotit = NULL,
  digits = NULL,
  ...
)

# S3 method for class 'Desc.logical'
print(x, digits = NULL, ...)

# S3 method for class 'Desc.logical'
plot(x, ...)

# S3 method for class 'Desc.numeric'
print(x, digits = NULL, ...)

# S3 method for class 'Desc.numeric'
plot(x, main = x$meta$main, ...)
```

## Arguments

- x:

  a dichotomous numeric, integer, factor, character, or logical vector

- ...:

  further arguments passed to methods

- formula:

  formula of the form `lhs ~ rhs`, where `lhs` gives the response values
  and `rhs` the corresponding groups or explanatory variables

- data:

  optional matrix or data frame (or similar; see
  [`model.frame`](https://rdrr.io/r/stats/model.frame.html)) containing
  the variables in the formula. By default the variables are taken from
  `environment(formula)`.

- subset:

  optional vector specifying a subset of observations to be used in the
  analysis

- na.action:

  function indicating what should happen when the data contain `NA`s.
  Defaults to `getOption("na.action")`.

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

- ord:

  order of the levels

- conf.level:

  confidence level of the interval (default 0.95). If set to `NA`, no
  confidence interval is calculated.

- include_x:

  logical; if `TRUE`, the original vector is retained in the result

- digits:

  number of digits used to format relative frequencies; the default can
  be set with `setDescToolsXOption(digits = x)`

## Value

an object of class `"Desc"` with a subclass determined by the input,
such as `"Desc.numeric"` or `"Desc.qn"`

an object of class `c("Desc.logical", "Desc")` with components:

- `afrq`:

  absolute frequencies

- `rfrq`:

  matrix of binomial estimates with columns:

  `est`

  :   point estimate of the binomial proportion

  `lci`

  :   lower confidence interval bound

  `uci`

  :   upper confidence interval bound

## Details

`desc()` is an S3 generic that computes basic descriptive statistics
depending on the class of its input. The result is an object of class
`"desc"` with a more specific subclass such as `"desc.numeric"`,
`"desc.factor"` or `"desc.data.frame"`.

For numeric vectors, summary statistics such as mean and standard
deviation are computed. For factors, frequency tables are returned. For
data frames, `desc()` is applied column-wise.

`desc` is a **generic function**. It dispatches to the method of the
class of its first argument.

Typing `?desc` + TAB at the prompt lists all available methods. You
usually call `desc(x)`, but direct calls like `desc.numeric(x)` are also
possible.

**Univariate descriptions**

- Numeric variables: [`desc.numeric`](desc.numeric.md)

- Factors and character vectors: [`desc.factor`](Desc.factor.md)

- Boolean variables: `desc.logical`

- Contingency tables: [`desc.table`](desc.table.md)

- Dates: [`desc.Date`](Desc.Date.md)

- Time series: [`desc.ts`](desc.ts.md)

**Bivariate descriptions**

- numeric ~ numeric: [`desc.nn`](Desc.nn.md)

- numeric ~ qualitative: [`desc.nq`](desc.nq.md)

- qualitative ~ numeric: [`desc.qn`](desc.qn.md)

- qualitative ~ qualitative: [`desc.qq`](desc.qq.md) (wrapper around
  [`desc.table`](desc.table.md))

**Design** The `desc` system separates:

- computation (internal `.desc_*` functions)

- printing (`print.Desc.*`)

- visualization (`plot.Desc.*`)

Description of a **dichotomous variable**. This can either be a logical
vector, a factor with two levels or a numeric variable with only two
unique values. The confidence levels for the relative frequencies are
calculated by
[`binomCI()`](https://andrisignorell.github.io/lumen/reference/binomCI.html),
method `"Wilson"` on a confidence level defined by `conf.level`.

Dichotomous variables can be condensed into a compact graphical
representation. The method calculates frequencies and binomial
confidence intervals and can display them as a dot plot with error bars.

## See also

[`summary`](https://rdrr.io/r/base/summary.html),
[`plot`](https://rdrr.io/r/graphics/plot.default.html)

[`plotPropCI`](https://andrisignorell.github.io/pharos/reference/plotPropCI.html)
for graphical display

Other desc: [`desc.Date()`](Desc.Date.md),
[`desc.factor()`](Desc.factor.md), [`desc.nn`](Desc.nn.md),
[`desc.nq`](desc.nq.md), [`desc.numeric()`](desc.numeric.md),
[`desc.qn`](desc.qn.md), [`desc.qq`](desc.qq.md),
[`desc.ts()`](desc.ts.md), [`print.Desc.qq()`](desc.table.md)
