
# DescToolsX — Design Rules & Architecture

Version: 0.4  
Maintainer: Andri Signorell

---

# 1. Project Philosophy

DescToolsX is the conceptual successor to DescTools and follows these guiding principles:

- API-consistent and predictable
- Methodologically transparent
- Defensive-programming-oriented
- CRAN-clean
- Clearly structured, with engine, interface, and resolver separated

Overall style goal: DescToolsX should feel like `survival`, `boot`, or `stats` — a statistical framework, not a historically grown toolbox.

**Core principle:**  
Consistency > Perfection  
Base-R compatibility > stylistic purity

---

# 2. Architecture

## 2.1 Separation of Layers

Each method family follows this structure:

```text
Interface → Resolver → Engine → Recycle Layer
```

Example for `binomCI`:

```text
binomCI()
 ├── .resolveMethod()
 ├── .recycleApply()
 └── .binomCI_engine()
      ├── .binomCI.wilson()
      ├── .binomCI.jeffreys()
      └── ...
```

## 2.2 Recycling Framework

- Vectorization is handled exclusively via `.recycleApply()`
- No implicit recycling inside engines
- Engines operate on scalar cases
- The interface guarantees consistent lengths

### Vectorization Contract

- The interface guarantees vectorization
- Engines must never expect vectors
- Recycling is centralized

## 2.3 No Metaprogramming Hacks

Forbidden:

- `eval(parse())`
- unnecessary string evaluation
- dynamic function construction

Allowed:

- clean function dispatch tables
- explicit `switch` / lookup tables
- clearly documented resolvers

## 2.4 Imports vs. Depends

- `Depends` only for the R version
- Packages via `Imports`
- No unnecessary namespace pollution

---

# 3. Naming Rules

## 3.1 Functions and API

### 3.1.1 General

| Layer | Style | Example |
|---|---|---|
| Exported functions | lowerCamelCase | `fitModel()` |
| Internal functions | `.lowerCamelCase` | `.computeWeights()` |
| Helper functions | `.prefix` | `.checkInput()` |
| Engines | `.familyEngine` | `.binomCI_engine()` |
| Classes | UpperCamelCase | `LinearModel` |
| User-visible strings | kebab-case | `"log-scale"` |

S3 methods never receive an X suffix and remain base-compliant:

```r
print.PercTable
plot.Desc.numeric
lines.Lc
```

### 3.1.2 Naming Across R and C++ (Rcpp Integration)

**Goal:**  
Clear, consistent, and immediately recognizable separation between:

- R code, which is user/API-oriented
- C++ code, which is the algorithmic implementation / performance layer

#### R: User-facing and internal

| Layer | Style |
|---|---|
| API | lowerCamelCase |
| Internal | .lowerCamelCase |
| Dispatch | .familyDispatch |
| Engine, optional R-side | .familyEngine |
| Helper | .prefix |

#### C++ / Rcpp

| Layer | Style |
|---|---|
| C++ functions | snake_case |
| Files | snake_case.cpp |

R wrappers act as bridges and also use snake_case:

```r
between_num <- function(...) {
  .Call(between_num_engine, ...)
}
```

## 3.2 Function Classification and X Suffix

The X suffix applies exclusively to **statistical summary measures** whose names collide with a function in `base`, `stats`, `graphics`, or `utils`.

**Statistical summary measures, collision-prone → lowercase + X:**

```text
meanX       medianX     sdX         varX
madX        iqrwX       rangeX      coefvarX
gmeanX      hmeanX      skewX       kurtX
maeX        mseX        rmseX       mapeX
smapeX      quantileX   percentRankX
```

Rules:

- No exceptions
- No all-caps spelling: `madX`, not `MADX`

**Rule: When exactly is X used?**

X is used when:

1. A function conceptually represents a statistical summary measure
2. AND the name collides with an existing base-R function

Examples:

```text
mean → meanX
var  → varX
rank → rankX
```

Do not use X for:

- Tests
- CI functions
- Transformations without collision

**Confidence intervals → `baseNameCI`, no X unless there is a collision:**

```text
meanCI      medianCI    varCI       sdCI
quantileCI  binomCI     poissonCI   rateCI
```

**Named tests and procedures → lowerCamelCase, no X:**

Tests generally do not collide with base and represent named statistical procedures. Therefore, the X suffix is omitted.

```r
# correct
jarqueBeraTest()
bartelsRankTest()
shapiroFranciaTest()

# incorrect
ShapiroFranciaTest()
AndersonDarlingTest()
```

**Transformations and utilities → lowercase, no X**, except when colliding with base:

```text
winsorize    roundTo    cutAge    cutQ
sortX        rankX      sampleX   # with X because of base collision
```

**Plot functions → lowerCamelCase with plot prefix, no X:**

```text
plotQQ       plotECDF     plotCorr
plotPairs    plotViolin   plotBar
```

**Special rule: established abbreviations**

Well-known statistical abbreviations may retain uppercase letters:

- CI (confidence interval)
- QQ (quantile-quantile)
- ECDF

Examples:

```text
meanCI
plotQQ
plotECDF
```

## 3.3 Argument Names

**(A) Preserve Base-R argument names unchanged** — these intentionally keep the dot:

```text
na.rm       conf.level      xlab    ylab    xlim    ylim
```

**(B) New arguments → lowerCamelCase:**

```text
groupSize   numBootstrap    showLegend    maxIter
```

**(C) Do not introduce new dot-separated names** — the dot is reserved exclusively for legacy/base-R conventions:

```r
# incorrect
group.size
num.bootstrap
```

**(D) Consistent terminology throughout the package** — one concept, one name:

```text
data      for data.frames / matrices
x         for vector input
y         second vector
groups    not grp or grouping
weights   not w or weightVec
data      not mixed with df or dataset
```

See also the core vocabulary.

**(E) Boolean arguments**

Boolean arguments begin with:

- is*
- has*
- show*
- use*

Examples:

```text
showLegend
useWeights
isSorted
```

Not allowed:

```r
legend = TRUE   # ambiguous
weights = TRUE  # semantic overload
```

**(F) Control terminology**

Do not use numeric terms such as `tol`, `eps`, or `precision` if the argument actually controls user-facing behavior, such as formatting or display boundaries.

Prefer semantic names such as:

```text
threshold
cutoff
limit
```

**(G) Output Representation, Return-control arguments**

When a function supports multiple mutually exclusive output
representations, use an `output` argument with enumerated character
options rather than Boolean switches.

Preferred pattern:

```r
output = c("value", "index")
```

Examples:

```r
closest(x, 3.1, output = "value")
closest(x, 3.1, output = "index")
```

This pattern is preferred because it:

- scales naturally when additional output representations are added
- produces clearer error messages via `match.arg()`
- avoids Boolean API proliferation
- improves readability and discoverability
- makes the API self-documenting

Avoid Boolean arguments for mutually exclusive output formats.

Incorrect:

```r
idx          = TRUE
returnIndex  = TRUE
returnList   = TRUE
asMatrix     = TRUE
```

Boolean `return*`, `keep*`, or `drop*` arguments remain appropriate
when they control independent optional output components rather than
exclusive output modes.

Examples:

```r
returnNames      = TRUE
returnCall       = FALSE
keepAttributes   = TRUE
dropUnusedLevels = FALSE
```

Conceptual distinction:

| Type | Purpose | Example |
|---|---|---|
| `output` | select one output representation | `output = "index"` |
| `return*` | optionally add information | `returnNames = TRUE` |
| `keep*` / `drop*` | modify retained structure | `dropNA = TRUE` |



## 3.4 Return Values

Output is part of the API and must be strictly consistent:

- always lowerCamelCase
- no dots
- descriptive names

```r
# correct
list(
  pValue        = 0.03,
  testStatistic = 2.1,
  confInt       = c(0.1, 0.5),
  nObs          = 120
)

# incorrect
list(p.value = 0.03, test.statistic = 2.1)
```

**Standardized statistic names**

Where possible, use:

- pValue
- testStatistic
- df (kept base-R compatible)
- estimate



### Design Rule: Missing-value handling (`NA` policy)

Functions should follow standard R semantics for missing values:

- If `na.rm = FALSE` (default) and the input contains `NA` values, the function should generally return `NA` rather than throw an error.
- If `na.rm = TRUE`, missing values are removed before computation.
- Functions should only error when missing values make computation structurally impossible (e.g. all observations removed, incompatible dimensions after omission, singular systems caused by complete missingness, etc.).
- This behaviour should mirror the expectations established by base R summary/statistical functions such as `mean()`, `sd()`, `median()`, and related estimators.

#### Examples

```r
mean(c(1, NA))
# NA

mean(c(1, NA), na.rm = TRUE)
# 1
```

#### Recommended implementation pattern

#### Univariate input

```r
if (na.rm)
  x <- x[!is.na(x)]

if (anyNA(x))
  return(NA_real_)
```

#### Multivariate input

```r
if (na.rm)
  x <- x[complete.cases(x), , drop = FALSE]

if (anyNA(x))
  return(NA_real_)
```

This rule prioritises predictable R-like behaviour over defensive failure for ordinary missing-data situations.




## 3.5 Method Strings (User Interface)

External options:

- lowercase
- hyphen-separated, kebab-case
- no spaces
- no cryptic abbreviations
- terminology close to the literature

```text
"wald"            "wald-cc"        "wilson"
"wilson-cc"       "wilson-mod"     "jeffreys"
"clopper-pearson" "agresti-coull"  "mid-p"
```

Internal naming:

- underscore `_`
- private functions with `.` prefix

```text
.binomCI.wilson_mod
.binomCI.clopper_pearson
```

## 3.6 Namespace Usage for Base-R Functions

Functions from the standard attached packages (`base`, `stats`, `graphics`, `grDevices`, `utils`, `methods`) are called **without** explicit namespace qualification:

```r
# correct
plot(...)
lines(...)
density(...)

# incorrect
graphics::plot(...)
stats::density(...)
```

Explicit qualification (`pkg::fun`) is used only for functions from non-base packages or in case of name conflicts.

## 3.7 C++ Functions

C++ functions use snake_case with a `_cpp` suffix and are registered via:

```cpp
// [[Rcpp::export]]
```

They are not exported in the NAMESPACE and therefore are not accessible to users via `::`.

The `_cpp` suffix conventionally marks them as internal implementation details.

Examples:

```text
kurt_cpp
kurt_weighted_cpp
conDisPairsTab_cpp
```

---

# 4. Argument Order in Functions

## 4.1 Statistical Functions

Exported statistical functions follow this order:

1. **Data** — `x`, `y`, `n`, possibly matrix or formula inputs
2. **Estimator definition** — `estimator`, `model`, `type`, `unit`, `weights`
3. **Inference / CI control** — `conf.level`, `sides`, `method`
4. **Data handling** — `na.rm`, `subset`
5. **`...`**

Core principle: CI construction depends on the estimator, never the other way around.

Example `skewX`:

```r
skewX(
  x,
  # Estimator
  estimator = 3,
  weights   = NULL,
  # Inference
  conf.level = NA,
  sides      = c("two.sided", "left", "right"),
  method     = c("classic", "boot"),
  # Data handling
  na.rm = FALSE,
  ...
)
```

**`conf.level` default:**

- Functions that return a CI together with the result, e.g. `skewX`, `ICC`: `conf.level = NA`, no CI by default
- Dedicated CI functions, e.g. `meanCI`, `binomCI`: `conf.level = 0.95`

## 4.2 Plot Functions

Plot functions follow this order:

```text
DATA
LABELS
AXES
STRUCTURE
STYLE
FEATURES
FRAMEWORK
...
```

| Group | Typical arguments |
|---|---|
| DATA | `x`, possibly `y` |
| LABELS | `main`, `xlab`, `ylab` |
| AXES | `xlim`, `ylim` |
| STRUCTURE | `cluster`, `order`, `groups`, `gap`, `items` |
| STYLE | `col`, `lwd`, `pch`, `bg`, `grid`, `box` |
| FEATURES | `legend`, `text`, `connlines`, `labels` |
| FRAMEWORK | `stamp` |

STRUCTURE arguments must never contain graphical style parameters. Colors always belong to STYLE.

---

# 5. Programming

## 5.1 Default Handling

- All valid options appear in the `formals()` default
- First entry = default
- No hardcoded default strings in the body
- No duplicate source of truth

## 5.2 Method Resolver

DescToolsX uses `.resolveMethod()` instead of `match.arg()` where extended resolution is needed, such as alias mapping, multiple selections, or hidden options.

`match.arg()` remains allowed for simple cases.

`.resolveMethod()` must:

- support partial matching
- support alias mapping
- be deterministic, with no guessing in ambiguous cases

Pattern with `.resolveMethod()`:

```r
if (missing(method)) {
  method <- formals(sys.function())$method[[1]]
} else {
  method <- .resolveMethod(method, several.ok = TRUE)
}
```

## 5.3 Defensive Programming

Exported functions must:

- perform type checks
- perform length checks
- validate scalar returns
- avoid silent NA cascades
- explicitly document boundary handling

## 5.4 Error Messages

- Clear and concrete
- No humor
- No internal terminology
- Always include argument names

Example:

```text
"Argument 'x' must be numeric and non-empty."
```

## 5.5 Backward Compatibility

- Old method names are accepted as aliases
- Internally, they are immediately mapped to canonical names
- No breaking changes without an alias layer

## 5.6 Confidence Interval Output Convention

All CI functions use the following column names:

- `estimate`
- `lci` (lower confidence interval bound)
- `uci` (upper confidence interval bound)

This convention is binding and must not be changed without a major version bump.

---

# 6. Handling `...`

## 6.1 Graphical Parameters in Plot Functions

Graphical parameters are passed via `...` and applied through `.applyParFromDots()`.

Plot functions do not manually process graphical parameters from `...`.

Which arguments are explicit and which are passed via `...` is **context-dependent**:

- the 2–3 most important visual parameters for the given function are explicit, so users see them in quick tips
- less central parameters such as `cex`, `cex.axis`, `las`, `mar`, `oma` go through `...`

Example `plotDot`:

- explicit: `col`, `pch`
- through `...`: `cex`, `las`

**Explicit vs. implicit graphical parameters**

- Key parameters of the respective function may be explicit
- Generic base parameters always go through `...`

Example:

```text
plotDot():
- explicit: col, pch
- through ...: cex, lwd, las
```

## 6.2 Bootstrap Arguments

Bootstrap arguments (`method`, `R`, `parallel`, etc.) always go through 
`...` and are internally extracted via `.extractBootArgs()`. This
function is available internally in several packages in the file
utils-ecosys:

```r
dots      <- list(...)
boot_args <- .extractBootArgs(dots)

boot::boot(..., R = boot_args$R, parallel = boot_args$parallel)
```

Forbidden:

- direct use of `getDotsArg()`
- direct access to `...` in the function body
- argument parsing inside `apply` / `replicate`

## 6.3 Flexible Argument Pattern for Graphical Elements

Arguments such as `xax`, `yax`, `grid`, and `legend` follow a unified flexible pattern:

| Value | Meaning |
|---|---|
| `TRUE` | Draw element with package defaults |
| `FALSE` | Suppress element |
| `NULL` / `NA` | Use package option |
| `list(...)` | Draw element with custom parameters |

Implementation via `.callIf()`:

```r
.callIf(graphics::grid, grid, defaults = th$grid)
```

DescToolsX does **not** use legacy base-graphics string flags such as `xaxt = "n"`.

Instead:

```r
yax = FALSE
```

---

# 7. Color Conventions

## 7.1 Argument Name

Always use `col`, never `cols` — also for palettes or multiple colors:

```r
col = "red"
col = c("red", "blue", "green")
col = colorRampPalette(...)(20)
```

## 7.2 Related Color Arguments

If different graphical elements require different colors, follow base R:

```r
col     # main color
border  # border color for polygons/boxes
bg      # symbol fill color
```

---

# 8. Statistical Functions

## 8.1 CI Functions

Dedicated CI functions (`meanCI`, `binomCI`, etc.) use `conf.level = 0.95` as default.

Functions that optionally include a CI use `conf.level = NA`.

## 8.2 Numerical Behavior

- Handle extreme cases explicitly
- No implicit corrections
- Document randomized procedures, e.g. Witting
- Mention RNG dependence

## 8.3 Random Number Generation (RNG) Policy

Functions in this package may rely on random number generation
provided by R's global RNG state.

Design principles:
  1) No internal seeding
     Functions do not call set.seed() internally. The RNG state is
     entirely controlled by the user.

  2) No side effects
     Functions do not modify or restore the global RNG state.

  3) Reproducibility by user control
     Users are expected to ensure reproducibility by calling
     set.seed() prior to invoking functions that involve randomness.

  4) Deterministic behaviour under fixed seed
     Given identical inputs and RNG state, functions produce
     reproducible results.

  5) Explicit documentation
     All functions involving randomness document this behaviour
     and refer users to set.seed() for reproducibility.
 
Example:
    set.seed(42)
    idx <- splitTrainTest(x)

---

# 9. Plot Functions

## 9.1 Graphics State Management

All plot functions are wrapped in `.withGraphicsState()`:

```r
.withGraphicsState({
  .applyParFromDots(...)
  # plotting code
}, stamp = stamp)
```

No direct `par()` calls outside `.applyParFromDots()`.

## 9.2 Helper Functions

Plot functions use these internal helpers:

```text
.withGraphicsState()
.applyParFromDots()
.resolveNames()
.normalizeDotData()
.adjustLeftMarginForLabels()
.callIf()
.drawAxis()
```

## 9.3 Theme System

Plot functions use `.theme()` for centralized style defaults:

```r
th <- .theme(
  grid = list(col = "grey", lwd = 1, lty = "dotted")
)
```

Theme subsetting: plot functions do not modify the theme globally, but select the relevant subset locally:

```r
defaults = th$grid[!startsWith(names(th$grid), "group.")]
```

Theme values may define STYLE only, never STRUCTURE.

## 9.4 `stamp`

`stamp` is controlled via a global option and is exposed as an explicit argument only when the user must be able to override the global default:

```r
.withGraphicsState(expr, stamp = .getOption("stamp", NULL))
```

---

# 10. Verbose Concept

> [TODO: Insert verbose concept]

---

# 11. Documentation

## 11.1 Description vs. Details

| Section | Contains | Does not contain |
|---|---|---|
| `@description` | What the method is; statistical purpose; conceptual foundation | Comparisons; asymptotic behavior; recommendations; limitations |
| `@details` | Relations to other methods; asymptotics; power; assumptions; limitations; comparisons | The primary definition of the method |

Description remains concise and self-contained.

Comparisons between methods always belong in Details. If a comparison seems short enough for Description, that is a signal that it should be moved to Details.

## 11.1a Roxygen Topic Naming (`@name`, `@rdname`)

### Purpose

Roxygen topic names define:

- the filename of the Rd documentation
- the grouping of related functions
- the anchor structure for pkgdown and help pages

They are **not function names** and therefore follow string-style conventions, not API naming rules.

### Naming Style

All topic names MUST use:

```text
kebab-case
```

lowercase + hyphen.

Example:

```r
@name extreme-value-moments
@rdname extreme-value-moments
```

### Allowed Binding Element

- Hyphen (`-`) → mandatory

### Forbidden Styles

The following MUST NOT be used:

| Style | Example | Reason |
|---|---|---|
| underscore | extreme_value_moments | conflicts with C++ naming |
| dot | extreme.value.moments | legacy base R style |
| camelCase | extremeValueMoments | reserved for functions |
| mixed styles | extreme-valueMoments | inconsistent |

### Structural Guidelines

Topic names should follow a semantic grouping pattern:

```text
<domain>-<concept>[-<detail>]
```

Examples:

```text
extreme-value-moments
extreme-value-distribution
extreme-value-quantiles
robust-statistics-location
robust-statistics-scale
```

### Grouping Across Functions

Multiple functions can share the same documentation topic:

```r
@name extreme-value-moments
@rdname extreme-value-moments

gumbel <- function(...) {}

@rdname extreme-value-moments

gev <- function(...) {}
```

This creates a single unified help page.

### Design Principles

- Topic names describe conceptual domains, not implementations
- Names must be:
  - short
  - descriptive
  - reusable across functions
- Prefer stable vocabulary to enable consistent grouping

### Summary

| Element | Rule |
|---|---|
| Case | lowercase |
| Separator | hyphen (`-`) |
| Style | kebab-case |
| Scope | conceptual grouping, not function naming |

## 11.2 Required Sections

All exported functions must contain:

**`@param`** — for every argument: type, meaning, constraints, and default behavior. Precise enough to prevent misuse.

**`@return`** — structure and type of the returned object. For complex outputs such as lists, the most important components are described.

**`@examples`** — minimal, reproducible, no external data, primary use case. Where useful: a second example for a non-default or edge case.

**`@examples`** should be deterministic → use `set.seed()` where needed.

## 11.3 Error Handling in Documentation

Document where relevant:

- handling of missing values (`NA`, `NaN`)
- behavior at boundary values
- violation of assumptions
- warnings or errors triggered

Goal: document behavior important for correct and predictable use — not a complete enumeration of all possible errors.

## 11.4 Authorship

The package maintainer is the default author of all functions and is not explicitly mentioned in individual functions.

| Contributor type | Where to mention |
|---|---|
| Package maintainer | Only in `DESCRIPTION` |
| External contributor, significant code | `@note` of the function |
| External contributor, minor contribution | `DESCRIPTION` or `NEWS`, not per function |

**`@note` wording, graded:**

Minor contribution:

```r
#' @note
#' Parts of the code contributed by [Name].
```

Adapted contribution:

```r
#' @note
#' Based on code by [Name], adapted to conform to package standards.
```

Substantial contribution:

```r
#' @note
#' Substantially based on code by [Name], with major extensions
#' and improvements by the package author.
```

When in doubt, choose the more generous wording.

`@note` is a courtesy attribution, not a legal statement. License compatibility of external code components must be checked.

## 11.5 References

Every function implementing a named method, test, or estimator cites the primary methodological source in `@references`.

**APA-based format:**

```r
#' @references
#' Author, A. B., & Author, C. D. (Year). Title of article.
#'   \emph{Journal Name}, \emph{volume}(issue), pages.
#'   \doi{10.xxxx/xxxxx}
#'
#' Author, A. B. (Year). \emph{Title of Book}. Publisher.
```

**What to cite:**

- paper or book where the method was first formally derived
- more accessible secondary source if the primary source is difficult to access, after the primary source
- software or algorithm paper if the implementation follows a specific algorithm

**What not to cite:**

- textbooks as primary sources, unless the method originated there
- R packages or online resources as primary methodological references if a theoretical original source exists

Exception:

If the implementation is explicitly based on a package, for example an adopted algorithm, that package may additionally be referenced.

If several references are listed, the theoretical source comes first. Methods compared in Details are all referenced.

### Distinction: Implementation vs. Application

References are used **only when the function itself** represents a methodological innovation or a concrete implementation of a published method.

References are **not used** when:

- the function merely wraps existing base-R functionality
- a known method is only applied, but not implemented
- the mathematical definition is trivial or generally known, e.g. mean, variance, dummy coding
- the reference adds no direct value for understanding the function

Examples:

```text
❌ dummy() → no reference, uses contr.*
❌ wrapper around mean(), sd() → no reference
✅ custom bootstrap algorithm → reference
✅ implemented statistical test → reference
```

## 11.6 Mathematical Notation

- Use `\eqn{}` for inline math
- Use `\deqn{}` only when truly necessary
- Parameters are italicized
- Literature is referenced cleanly

---

# 12. Family and Concepts

## 12.1 Overview

The package uses `@family` and `@concept` for function organization with strict separation of roles:

- `@family` defines the primary classification, used for navigation
- `@concept` provides additional semantic tags, used for search, context, and cross-linking

## 12.2 `@family`

**Exactly one family per function:**

```r
@family topic.<categoryName>
```

Naming convention:

```text
topic. prefix + camelCase suffix
```

Examples:

```text
topic.hypothesisTests
topic.nonparametricTests
topic.distributions
topic.timeSeriesTests
topic.goodnessOfFit
topic.contingencyTests
```

**Decision rule:**  
Where would a user look for this function first?

## 12.3 `@concept`

Each function should typically have 2–4 `@concept` tags.

Rules:

- Concepts are not mutually exclusive
- Concepts are descriptive, not hierarchical
- No redundancy with the family
- lowercase kebab-case: `goodness-of-fit`, `heavy-tailed`

```r
@concept goodness-of-fit
@concept normality
@concept rank-based
@concept extreme value theory
```

## 12.4 Families vs. Concepts

| Feature | `@family` | `@concept` |
|---|---|---|
| Cardinality | exactly one | several |
| Purpose | navigation | semantic tagging |
| Structure | hierarchical (`topic.*`) | flat |
| Stability | high | flexible |
| Example | `topic.nonparametricTests` | `rank-based`, `paired` |

## 12.5 Special Case: Distributions

All distribution functions (`d` / `p` / `q` / `r`) are grouped under one single family:

```r
@family topic.distributions
```

Specific properties are expressed via `@concept`:

```r
@concept continuous distribution
@concept extreme value theory
@concept GEV
@concept dpqr
```

**Rationale:**  
Distributions follow a uniform API structure (`dpqr`), and users search by distribution name, not by category.

## 12.6 Hypothesis Tests

Tests are heterogeneous and therefore use several families, according to the natural way users search for them:

```text
topic.goodnessOfFit
topic.nonparametricTests
topic.contingencyTests
topic.timeSeriesTests
```

## 12.7 `@seealso`

`@seealso` is reserved for close functional relationships: functions that are direct alternatives, or helper functions typically used together.

---

# 13. DescToolsX — Core Vocabulary

## 13.1 Data Structure

| Meaning | Name | Comment |
|---|---|---|
| Vector, primary input | `x` | Standard |
| Second vector | `y` | Standard |
| Data frame / matrix | `data` | as in `lm`, `ggplot` |
| Formula | `formula` | Base-R compatible |

## 13.2 Grouping and Structure

| Meaning | Name |
|---|---|
| Grouping variable | `groups` |
| Strata | `strata` |
| Cluster | `cluster` |

## 13.3 Weights and Subsetting

| Meaning | Name |
|---|---|
| Weights | `weights` |
| Subset | `subset` |

## 13.4 Statistics / Inference

| Meaning | Name |
|---|---|
| Confidence level | `conf.level` |
| p-value | `pValue` |
| Test statistic | `testStatistic` |
| Degrees of freedom | `df` |
| Estimator | `estimate` |

## 13.5 Method Control

| Meaning | Name |
|---|---|
| Method | `method` |
| Internal engine | `engine` |
| Type | `type` |

## 13.6 Simulation / Bootstrap

| Meaning | Name |
|---|---|
| Iterations | `R` |

## 13.7 Order / Range

| Meaning | Name |
|---|---|
| Sorting / ordering | `order` |
| Value range | `range` |
| Decreasing | `decreasing` |

## 13.8 Missing Values

| Meaning | Name |
|---|---|
| Remove NA | `na.rm` |
| NA position | `na.last` |

## 13.9 Plotting

| Meaning | Name |
|---|---|
| Color | `col` |
| Symbol | `pch` |
| Line width | `lwd` |
| Line type | `lty` |

## 13.10 Boolean Flags

| Type | Prefix |
|---|---|
| Display | `show*` |
| Use | `use*` |
| State | `is*` |
| Possession / availability | `has*` |
| Return structure | `return*` |

## 13.11 Intervals

| Meaning | Name |
|---|---|
| Left boundary closed | `leftClosed` |
| Right boundary closed | `rightClosed` |

## 13.12 Output

| Meaning | Name |
|---|---|
| p-value | `pValue` |
| Statistic | `testStatistic` |
| Lower CI | `lci` |
| Upper CI | `uci` |
| Confidence interval | `confInt` |

## 13.13 Forbidden Names

Do not use:

```text
df     for data
dat
grp
w
level
alpha
color
```
