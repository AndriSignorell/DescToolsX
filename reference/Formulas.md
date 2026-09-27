# Formula Interfaces - Common Arguments

Common arguments and conventions for formula interfaces in DescToolsX.

## Arguments

- formula:

  formula describing the design, see section **Supported forms**. Which
  forms a function accepts is documented on its own help page.

- data:

  optional matrix or data frame (or similar; see
  [`stats::model.frame()`](https://rdrr.io/r/stats/model.frame.html))
  containing the variables in the formula. A matrix is converted to a
  data frame. If omitted or `NULL`, variables are taken from
  `environment(formula)`.

- subset:

  optional expression specifying a subset of observations to be used in
  the analysis. It is evaluated in `data` first, then in
  `environment(formula)`, and applied before missing values are handled.

- na.action:

  function specifying how missing values are handled, passed to
  [`bedrock::resolveFormula()`](https://andrisignorell.github.io/bedrock/reference/resolveFormula.html).
  The resolver's default
  [`stats::na.pass()`](https://rdrr.io/r/stats/na.fail.html) keeps
  incomplete cases in the model frame, so the function itself can count
  and report them (as [`desc()`](Desc.md) does) before removing them.
  [`stats::na.omit()`](https://rdrr.io/r/stats/na.fail.html) drops them
  beforehand.

## Details

Formula interfaces in DescToolsX are resolved consistently by
[`bedrock::resolveFormula()`](https://andrisignorell.github.io/bedrock/reference/resolveFormula.html).
The resolver builds the
[`stats::model.frame()`](https://rdrr.io/r/stats/model.frame.html) once,
applying `data`, `subset` and `na.action`, and classifies the design as
one-sample, two-sample independent, two-sample dependent, n-sample
independent, n-sample dependent, or numeric-numeric. Each function
states which designs it accepts; any other form is rejected with an
error naming the design found.

Besides the model frame, the resolver returns the row positions of the
retained observations in the original data. Functions that align
external vectors with the model frame (e.g. an ordering variable after
`subset` and `na.action`) use these instead of recomputing them.

## Supported forms

- `y ~ 1`:

  one sample

- `Pair(x, y) ~ 1`:

  two dependent (paired) samples, see
  [`stats::Pair()`](https://rdrr.io/r/stats/Pair.html)

- `y ~ group`:

  two or more independent samples, `group` being categorical

- `y ~ treatment | block`:

  two or more dependent samples in a block design

- `y ~ x`:

  two numeric variables (response and predictor)

**[`desc()`](Desc.md)** accepts `y ~ x` for any combination of numeric
and categorical variables and dispatches on the types of the two columns
of the model frame:

|             |             |                           |
|-------------|-------------|---------------------------|
| **y**       | **x**       | **method**                |
| numeric     | numeric     | [`desc.nn()`](Desc.nn.md) |
| numeric     | categorical | [`desc.nq()`](desc.nq.md) |
| categorical | numeric     | [`desc.qn()`](desc.qn.md) |
| categorical | categorical | [`desc.qq()`](desc.qq.md) |

## See also

[`bedrock::resolveFormula()`](https://andrisignorell.github.io/bedrock/reference/resolveFormula.html),
[`stats::formula()`](https://rdrr.io/r/stats/formula.html),
[`stats::model.frame()`](https://rdrr.io/r/stats/model.frame.html),
[`stats::Pair()`](https://rdrr.io/r/stats/Pair.html),
[`desc()`](Desc.md)
