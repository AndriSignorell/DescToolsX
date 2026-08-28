# Formula Interface - Common Arguments

Common formula-based interface shared by multiple functions.

## Arguments

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

## Details

Formula interfaces are evaluated using
[`model.frame`](https://rdrr.io/r/stats/model.frame.html), following
standard R conventions. The left-hand side of the formula must contain
the response variable. The right-hand side typically specifies a
grouping or explanatory variable. Only formulas with a single response
and at least one explanatory variable are supported.

See also:

- [`formula`](https://rdrr.io/r/stats/formula.html)

- [`model.frame`](https://rdrr.io/r/stats/model.frame.html)

- [`terms`](https://rdrr.io/r/stats/terms.html)
