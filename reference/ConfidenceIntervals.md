# Confidence Interval Interface - Common Arguments

Arguments shared by the confidence interval functions in this package,
documented once. Individual functions describe only which values they
accept and refer here for what the values mean.

## Arguments

- conf.level:

  confidence level of the interval. If set to `NA` (the default), only
  the point estimate is returned.

- sides:

  character string specifying the sidedness of the confidence interval.
  Must be one of `"two.sided"` (default), `"left"` or `"right"`. For a
  one-sided interval, the value names the side with the finite bound.
  The initial letter is sufficient.

- method:

  character string specifying the interval method. The available methods
  and the default differ between functions; see the individual help page
  for the choices.

- ...:

  additional arguments for the bootstrap, such as the number of
  resamples `R` and the interval type `type`.

## Details

### Return value

With `conf.level = NA` the functions return the point estimate as an
unnamed scalar. With a confidence level they return a named numeric
vector with the elements `est`, `lci` and `uci`, in that order. Reading
the result by name rather than by position is the safer habit, since
some functions return further elements.

### One-sided intervals

`sides` names the side carrying the *finite* bound:

- `"left"`:

  the lower bound is finite, the upper one is open.

- `"right"`:

  the upper bound is finite, the lower one is open.

Thus `sides = "left"` corresponds to `alternative = "greater"` in
[`t.test`](https://rdrr.io/r/stats/t.test.html), and `sides = "right"`
to `alternative = "less"`. This is also the convention used by the
corresponding functions in DescTools.

The open side is reported at the boundary of the parameter space, not at
infinity - most of the statistics here are bounded, so this is the
ordinary case rather than an exception. A correlation opens to \\\pm
1\\, an association measure in \\\[0, 1\]\\ to 0 or 1, Cramer's \\V\\ to
1, Pearson's \\C\\ to \\\sqrt{(m-1)/m}\\. Where the parameter really is
unbounded, \\\pm\infty\\ is reported: a relative risk opens upwards to
`Inf` but downwards only to 0, a location estimator opens to `-Inf` and
`Inf`, Cronbach's \\\alpha\\ to `-Inf` and 1. An interval never claims a
value the statistic cannot take.

A one-sided bound at level \\\gamma\\ is the corresponding end of the
two-sided interval at level \\2\gamma - 1\\: a 95\\ lower end of the
two-sided 90\\ interval requires `conf.level` above 0.5 and is refused
below it, where the adjusted level would not be positive.

### Choice of method

The available options depend on the statistic and on what is known about
its distribution. Classical intervals rely on asymptotic normality or on
an analytic variance formula, and are fast and deterministic where such
a formula exists. Bootstrap intervals, requested with `"boot"`, need no
closed-form variance and are therefore available for statistics that
have none - at the price of being random and slower.

Bootstrap intervals are partly computed with the boot package (see
[`boot`](https://rdrr.io/pkg/boot/man/boot.html) and
[`boot.ci`](https://rdrr.io/pkg/boot/man/boot.ci.html)). The number of
resamples `R` and the interval type - `"perc"`, `"bca"` and others - are
passed through `...`.

`"bca"` corrects for bias and skewness and is the better choice for a
smooth statistic whose parameter lies well inside its range. It is the
weaker choice near a boundary: both of its ingredients degrade where the
parameter sits at the edge of the parameter space, which for an
association measure under independence is the ordinary situation rather
than a pathology. `"perc"` is the more robust default there.

## Random number generation

Requesting a bootstrap confidence interval draws a seed from R's global
random number generator and therefore advances it. Call
[`set.seed`](https://rdrr.io/r/base/Random.html) beforehand for
reproducible intervals. This applies to the bootstrap methods only;
classical intervals are deterministic.

## See also

[`boot`](https://rdrr.io/pkg/boot/man/boot.html),
[`boot.ci`](https://rdrr.io/pkg/boot/man/boot.ci.html),
[`confint`](https://rdrr.io/r/stats/confint.html)
