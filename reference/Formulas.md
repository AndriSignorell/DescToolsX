# Formula Interfaces - Common Arguments

Common arguments and conventions for formula interfaces in DescToolsX.

## Arguments

- formula:

  formula describing the design. Depending on the function, supported
  forms include `y ~ 1`, `Pair(x, y) ~ 1`, `y ~ group`, `y ~ predictor`,
  and `y ~ treatment | block`

- data:

  optional matrix or data frame (or similar; see
  [`model.frame`](https://rdrr.io/r/stats/model.frame.html)) containing
  the variables in the formula. If omitted, variables are taken from
  `environment(formula)`

- subset:

  optional expression specifying a subset of observations to be used in
  the analysis

- na.action:

  function specifying how missing values are handled; passed to
  [`resolveFormula`](https://andrisignorell.github.io/bedrock/reference/resolveFormula.html)

## Details

Formula interfaces in DescToolsX are resolved consistently by
[`resolveFormula`](https://andrisignorell.github.io/bedrock/reference/resolveFormula.html).
The resolver constructs the
[`model.frame`](https://rdrr.io/r/stats/model.frame.html) and classifies
the resulting design as one-sample, two-sample independent, two-sample
dependent, n-sample independent, n-sample dependent, or numeric-numeric.

Individual functions may support only a subset of these designs. The
accepted forms are documented on the corresponding function's help page.
Data lookup, subsetting, and missing-value handling are delegated to
[`resolveFormula()`](https://andrisignorell.github.io/bedrock/reference/resolveFormula.html).

## See also

[`resolveFormula`](https://andrisignorell.github.io/bedrock/reference/resolveFormula.html),
[`formula`](https://rdrr.io/r/stats/formula.html),
[`model.frame`](https://rdrr.io/r/stats/model.frame.html),
[`Pair`](https://rdrr.io/r/stats/Pair.html)
