
# Package Suite — Design Rules & Architecture

Scope: DescToolsX · lumen · aurora · bedrock · alloy  
Version: 0.5  
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

Kategorie,Stil,Beispiele
Datensätze,PascalCase,"Kunden, Messdaten, RawData"
Modell-Objekte,lowerCamelCase,"fit, modKunden, regOut"
Funktionen,lowerCamelCase,"fitMod(), calcSummary(), plotResults()"


S3 methods never receive an X suffix and remain base-compliant:

```r
print.PercTable
plot.Desc.numeric
lines.Lc
```

#### 3.1.1.1 Exporting S3 Methods Callable From Other Packages

**Problem:** roxygen2 detects functions named `generic.class` (e.g.
`plot.Desc.table`) as S3 methods via naming convention. For such a
function, `@export` and `@exportS3Method` are **not additive** - roxygen2
picks one registration mode and writes only `S3method(generic, class)` to
`NAMESPACE`, never an additional `export(generic.class)` entry, no matter
which combination of the two tags is tried. This is true even when both
tags are present on the same function.

**Symptom:** `plot(obj)` works fine within the defining package (and for
anyone calling it via plain `UseMethod` dispatch, since R's S3 dispatch
searches the registered `S3method()` table regardless of export status).
But another package that calls the method **by name**, unqualified (e.g.
`plot.Desc.table(x, ...)` inside a wrapper like `plot.Desc.qq`), fails
with `could not find function "plot.Desc.table"` - even after
`@importFrom otherPkg plot.Desc.table` - because the symbol was never
actually exported, only registered as an S3 method.

**Fix:** add an explicit `@rawNamespace` line to force the export, on top
of `@exportS3Method` for the dispatch registration:

```r
#' @exportS3Method
#' @rawNamespace export(plot.Desc.table)
plot.Desc.table <- function(x, ...) { ... }
```

This produces both `NAMESPACE` lines:

```
S3method(plot, Desc.table)
export(plot.Desc.table)
```

**When this applies:** any `plot.Desc.*`/`print.Desc.*`/etc. method that
(a) lives in `aurora` (or another helper package) rather than in
`DescToolsX` where the class itself is defined, **and** (b) is called by
unqualified name from `DescToolsX` code rather than purely through
`UseMethod` dispatch. Methods only ever reached via `plot(obj)` dispatch
do not need the `@rawNamespace` line - plain `@export` (or
`@exportS3Method`) is sufficient there.

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
gmeanX      hmeanX      skew       kurt
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

Boolean arguments follow established R conventions (paired, ordered, verbose, 
exact, ...). 
Prefixes such as is*, has*, show* are permitted but not required.


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



**(H) Argument Naming **

Use argument names that communicate the **role** of the object, 
not merely its storage type.

### General Principles

- Use short, conventional names where their meaning is universally understood.
- Prefer consistency across the package over local optimization.
- Follow established R conventions whenever they are clear and widely adopted.

### Recommended Names

| Object type | Preferred name |
|-------------|----------------|
| Numeric vector, factor, table, generic object | `x` |
| Second object of same type | `y` |
| Matrix | `m` |
| Data frame | `data` |
| Formula | `formula` |
| Model fit object (`lm`, `glm`, `coxph`, etc.) | `fit` |
| User-supplied function | `FUN` |
| Internal function object | `fun` |

### Examples

Good:

```r
meanCI(x)
corCI(x, y)

fit <- lm(y ~ x)

coefCI(fit)
pseudoR2(fit)
vif(fit)

model.frame(formula, data)
```

Avoid:

```r
coefCI(x)
vif(mod)
bpTest(lm_fit)
pseudoR2(model_object)
```

### Rationale

For raw data, `x` and `y` are concise and familiar. For fitted models, `fit` immediately signals that a model object is expected and aligns with common usage throughout the R ecosystem:

```r
fit <- lm(y ~ x)

coef(fit)
predict(fit)
residuals(fit)
fitted(fit)
```

Using `fit` consistently improves readability and reduces ambiguity compared with alternatives such as `x`, `mod`, `model`, `lm_fit`, or `glm_fit`.


**(I) Argument matching & signature design **

R's partial argument matching cannot be disabled, but signatures can be
designed so it never bites.

**Rules**

1. **No prefix families in signatures.** Never expose two or more formals
   sharing a common prefix that is itself a plausible argument name
   (e.g. `cex.axis` + `cex.names` → `cex=` errors with
   "matches multiple formal arguments", as in `barplot()`).
   Worse than the error is the silent case: a prefix matching exactly
   *one* formal the caller did not intend.

2. **Route par-parameters through the dots.** Graphics parameters
   (`cex`, `lwd`, `col`, ...) are not formals; they pass via `...`
   to `.applyParFromDots()` / `plot()` / `points()`. The dots never
   partially match, so no collisions arise.

3. **List arguments instead of prefixed formals.** Substructure goes
   into a single list argument (`qqline = list(col=, lty=)`), never
   into `qqline.col`, `qqline.lty`, ... formals.

4. **Critical options go after the dots.** (unsure whether to retain: observe) 
   Formals placed after `...`
   require exact names — partial matching is off for them:

```r
   f <- function(x, ..., conf.level = 0.95)
```

   Trade-off: a typo (`conf.lvl=`) silently falls into the dots instead
   of matching. Acceptable for a documented API; consider validating
   unused dots where feasible.

**Development setting**

Run tests with `options(warnPartialMatchArgs = TRUE)` to surface any
partial matches the API still permits.


## 3.4 Inherited Argument Names — Exemption List

Argument names inherited from base-R functions or established third-party
packages are exempt from the lowerCamelCase rule and are kept exactly as
they appear in the source package. Do not rename them.

### Base R / stats

These names are passed through via `...` or explicitly forwarded and must
not be renamed:

```text
na.rm           # almost all stats functions
na.action       # lm, glm, model.frame
na.last         # sort, order
conf.level      # t.test, prop.test, binom.test
sig.level       # power.*
lower.tail      # p* / q* distribution functions
log.p           # p* / q* distribution functions
var.equal       # t.test
paired          # t.test
ties.method     # rank
decreasing      # sort
use.names       # sapply, setNames
fixed           # grep, sub, gsub
perl            # grep, sub, gsub
ignore.case     # grep, grepl, sub, gsub
strict.width    # format
dig.lab         # cut, formatC
right           # cut
include.lowest  # cut
ordered_result  # cut
list.len        # str
all.x           # merge
all.y           # merge
by.x            # merge
by.y            # merge
ncp             # d/p/q/r distribution functions
df              # d/p/q/r distribution functions (degrees of freedom)
```

### abind package

Arguments forwarded to `abind::abind()` retain their original names:

```text
rev.along
new.names
force.array
make.names
use.anon.names
use.first.dimnames
hier.names
use.dnns
```

### Rule

If an argument is forwarded — either via `...` or explicitly — to a
base-R or established third-party function, its original name is always
preserved, regardless of naming style.

New arguments introduced by DescToolsX itself always follow lowerCamelCase
(Section 3.3).


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


### Grouping argument by function family

The grouping argument name follows the base-R function being extended:

| Context | Argument name |
|---------|--------------|
| Extensions of `kruskal.test`, `oneway.test` | `g` |
| Extensions of `t.test`, `wilcox.test` | `x`, `y` |
| All other contexts (desc, plot, reshape, utility) | `groups` |

Examples: `jonckheereTerpstraTest`, `siegelTukeyTest`, `mosesTest` → `g`;
`toWide`, `plotDot`, `resolveGroups` → `groups`.



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

Example `skew`:

```r
skew(
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

- Functions that return a CI together with the result, e.g. `skew`, `ICC`: `conf.level = NA`, no CI by default
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


## 4.3 General Utility Functions

General utility functions (vector operations, string functions, data
manipulation) follow this order:

1. **Primary input** — `x`, `y`, `data`, `formula`, `fit`
2. **Secondary input** — `breaks`, `groups`, `weights`, `subset`
3. **Method control** — `method`, `type`, `output`
4. **Numeric tuning** — `R`, `maxIter`, `threshold`
5. **Data handling** — `na.rm`
6. **`...`**

Core principle: input before control, control before tuning.

Example `closest`:

```r
closest(
  x,
  val,
  # Method
  output = c("value", "index"),
  # Data handling
  na.rm = FALSE,
  ...
)
```

## 4.4 Toggle Arguments (`callIf`-Pattern)

Toggle arguments are always placed **after `...`**, since they are
secondary to the core function and require exact name matching:

```r
plotProbDist(
  breaks, FUN,
  # Labels
  main  = "",
  xlab  = NULL,
  ylab  = "density",
  # Axes
  xlim  = NULL,
  # Style
  col     = NULL,
  density = 7,
  # Features (callIf)
  ...,
  areaLabels  = NULL,
  breakLabels = NULL,
  grid        = FALSE,
  box         = .useTheme
)
```

Toggle arguments after `...` are intentional API design: they require
exact name matching and are documented as such. This prevents accidental
partial matching against graphical parameters passed via `...`.

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

## cex policy

`cex` means symbol size, nothing else.

- Functions that draw symbols (`plotXY`, `plotDot`, `plotQQ`, ...) declare
  `cex` as a documented formal placed after the dots, resolved against the
  theme (`cex = NULL` -> `.theme()`), and pass it explicitly to `points()`.
- All other plot functions do not know `cex`. It is never routed to `par()`
  (`.applyParFromDots()` excludes it by default): setting `par(cex=)` scales
  the text line height and thereby the margins (`mai = mar * line height`),
  silently changing the plot layout.
- Fine-grained text sizing uses the specific parameters (`cex.axis`,
  `cex.lab`, `cex.main`), which pass through the dots to `par()` without
  side effects.
- Global scaling (e.g. presentation mode) is a theme concern, declared once
  via options and resolved in `.theme()` — never per call site via `cex`.


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

For `grid` and `box` specifically, use the dedicated dispatchers
`.drawGrid()`/`.drawBox()` (see 9.2) rather than calling `.callIf()`
directly - they wrap the same pattern and additionally understand the
`.useTheme` sentinel (9.3), which raw `.callIf()` does not.

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

All plot functions are wrapped in `.withGraphicsState()`, with `stamp`
passed as an argument to the wrapper, not drawn manually inside it:

```r
.withGraphicsState({

  .applyParFromDots(
    ...,
    defaults = list(mar = c(bottom = 5, left = lmar, top = .marTop(main), right = 3.1))
  )

  # plotting code

}, stamp = stamp)
```

No direct `par()` calls outside `.applyParFromDots()`.

`.withGraphicsState()` only stamps if its body completes without error
(it sets an internal `ok` flag to `TRUE` after `eval.parent()` returns,
and the `on.exit()` stamp call checks that flag) - a plot that errors
partway through is never stamped. This is by design, not a bug to work
around: don't add manual stamp suppression for the error case.

**`oma`/`omi` must never be added to a `par()` save/restore list.**
`.withGraphicsState()` saves a fixed set of graphical parameters before
drawing and restores them afterward (`op <- par(keep); ...;
withr::defer(par(op))`). `"oma"`/`"omi"` must not appear in `keep` (or in
any similar save/restore list elsewhere) - merely reading them via
`par(c(...))` and writing the *same*, unchanged value back via
`par(op)` breaks an active `par(mfrow = ...)`/`layout()` grid. Confirmed
empirically:

```r
par(mfrow = c(1, 2))
op <- par(c("oma", "omi"))
barplot(1:3)
par(op)          # writes back the unchanged oma/omi - no-op in theory
barplot(4:6)     # ...but this opens a NEW page instead of filling panel 2
```

`oma`/`omi` describe the page-wide outer margin around the *entire*
`mfrow`/`layout` grid, not a per-panel setting; re-setting them via
`par()`, even to a value identical to the current one, appears to
invalidate R's internal page/layout progress on at least some graphics
devices. None of the current plot functions set `oma`/`omi` themselves
(via `defaults` in `.applyParFromDots()` or otherwise), so there is
nothing that legitimately needs saving/restoring here - if that ever
changes, the interaction with an active `mfrow`/`layout` needs to be
re-verified before adding `oma`/`omi` back to any such list.

**Stamp is currently drawn on every panel, not only the last.** With a
`par(mfrow = c(1, 2))` call, the corner stamp from `.withGraphicsState()`
is drawn after each panel - so it currently appears once per panel, not
once per page. A `.isLastPanel()` helper exists (9.2) but is **not**
wired into the stamp call as of this writing; do not assume it is in
effect, and do not document or rely on once-per-page stamping until this
is actually implemented.

## 9.2 Helper Functions

Plot functions use these internal helpers, as established in
`plotXY.R`/`plotBox.R` (reference implementations) and ported to
`plotAssoc.R`/`plotHeatmap.R`:

```text
.withGraphicsState()
.applyParFromDots()
.resolveTitle()
.marTop()
.marginLines()
.drawGrid()
.drawBox()
.useTheme           (sentinel value, not a function - see 9.3)
```

`.isLastPanel()` also exists in `utils-plot.R` (intended to detect the
last panel of an `mfrow`/`layout` grid for once-per-page stamping) but
is currently **not** called from anywhere, including
`.withGraphicsState()` - see 9.1's note on stamp-per-panel behavior.
Treat it as available-but-unused until it is actually wired in; don't
assume it is already affecting stamp placement.

`bedrock::callIf()` is used for one-off graphical elements that follow
the flexible TRUE/FALSE/NA/list pattern (see 6.3) but are not already
covered by `.drawGrid()`/`.drawBox()` - e.g. a legend
(`bedrock::callIf(graphics::legend, legend, defaults = ...)`).

`callIf()` does **not** understand the `.useTheme` sentinel - never pass
a raw, unresolved `.useTheme`-valued argument into `callIf()` directly.
Resolve it first (e.g. via `.resolveToggle()`, or by branching on
`identical(x, .useTheme)` as in `resolveCol()`, 9.3) before handing the
result to `callIf()`.

### 9.2.1 Standard Set of FRAMEWORK/FEATURES Arguments

Every plot function exposes `grid`, `box`, and `stamp` wherever the
element is geometrically meaningful for that plot type - these are not
optional extras added case-by-case, but the expected baseline API for
any new plot function. Each follows the flexible
`TRUE`/`FALSE`/`NA`/`list(...)` pattern (6.3), defaults to `.useTheme`,
and is wired through `.drawGrid()`/`.drawBox()` or
`.withGraphicsState(..., stamp = stamp)` rather than a bespoke
implementation:

```r
grid  = .useTheme,   # FEATURES
box   = .useTheme,   # STYLE
stamp = .useTheme,   # FRAMEWORK
```

"Wherever meaningful" excludes the element only when the underlying
drawing function makes it structurally impossible or redundant to offer
a toggle - not merely inconvenient to wire up:

- a panel that delegates to a base-R function with no native
  suppression path for its own frame (e.g. `spineplot()`'s
  unconditional `box()`) has no working `box` toggle, and the argument
  is documented as having no effect there rather than omitted from the
  function signature entirely (see `plot.Desc.table`'s `@param box` for
  the documented-no-op pattern in a dispatching method);
- a plot type with no rectangular coordinate frame at all (e.g.
  `plotAssoc()`, which draws cell rectangles directly with no
  `box()`/frame concept) legitimately has no `box` argument;
- `stamp` is omitted only on the inner leg of a delegation chain
  pattern, never on a function callable directly by the end user (9.5).

When in doubt, add the argument and document precisely what it does (or
doesn't) affect, rather than leaving it out silently.

## 9.3 Theme System and the `.useTheme` Sentinel

Plot functions resolve style defaults from the active theme via
`getTheme()`, not via a function call per parameter. The default value of
a STYLE argument is the sentinel `.useTheme`, never a literal color or a
computed value, so the call site can tell "use the theme" apart from "the
user explicitly chose this":

```r
plotHeatmap <- function(x,
                        col  = .useTheme,
                        box  = .useTheme,
                        stamp = .useTheme,
                        ...)
```

Inside the function, resolve `.useTheme` against `getTheme()` (for a
single, unambiguous default) or against multiple panel-specific defaults
when one shared default would not be meaningful (e.g. a dispatching
`plot.Desc.*` method covering several different panel types - a grey fill
ramp and a diverging residual palette are not interchangeable defaults
for the same `col` argument):

```r
resolveCol <- function(default) {
  if (identical(col, .useTheme)) default else col
}

colSpineDefault <- colorRampPalette(c("grey30", "grey90"))(ncolTab)
# ...
spineplot(tab, col = resolveCol(colSpineDefault), ...)
```

Theme subsetting: plot functions do not modify the theme globally, but
select the relevant subset locally:

```r
defaults = th$grid[!startsWith(names(th$grid), "group.")]
```

Theme values may define STYLE only, never STRUCTURE.

## 9.4 `main` / Title Resolution

The `main` argument follows a three-state contract, resolved via
`.resolveTitle(main, default = ...)`:

| Value | Meaning |
|---|---|
| `NULL` (default) | derive a title from the call (see below) |
| `""` / `NA` / `FALSE` | suppress the title; the top margin is compacted accordingly (`.marTop(main)` returns a smaller value) |
| any other string | used as-is |

The derived default follows "substitute magic": `match.call()` is
captured early (before the data argument is reassigned/transformed), and
the default title is built from the deparsed expression(s) the caller
actually wrote, not from variable names inside the function:

```r
mc <- match.call()

.withGraphicsState({
  main <- .resolveTitle(main, default = deparse(mc$x))   # single-argument functions
  # or, for a y ~ x pair:
  main <- .resolveTitle(main, default = paste(deparse(mc$y), "~", deparse(mc$x)))
  ...
})
```

`mc <- match.call()` is placed *before* `.withGraphicsState()`, at the
same point in every function, for consistency and so it captures the
original argument expressions before any internal reassignment
(transpose, reorder, etc.).

Dispatching `plot.Desc.*` methods that have no `y ~ x` formula pair at
their level (e.g. a table built outside a formula) have no reliable
source for separate "x" and "y" names; do not invent one. Use whatever
single name is actually available (e.g. `x$meta$xname`), optionally with
a panel-type suffix for context, rather than fabricating a placeholder
like `"y"`.

## 9.5 `stamp`

`stamp` is an explicit, documented argument on every plot function for
which a corner stamp is meaningful, defaulting to `.useTheme`:

```r
stamp = .useTheme
```

It is passed straight through to `.withGraphicsState(expr, stamp = stamp)`
- never drawn manually inside the function body.

**Nested calls:** when a `plot.Desc.*` dispatch method delegates to
another plot function that itself draws a stamp (e.g.
`plot.Desc.table()` calling `plotMosaic()`/`plotAssoc()`/`plotHeatmap()`),
the inner call passes `stamp = NA` to suppress its own stamp, so the
stamp is drawn exactly once, by the outermost `.withGraphicsState()`
after all selected panels have been drawn:

```r
plotHeatmap(tab, ..., stamp = NA)   # inside plot.Desc.table()
```

Functions that are always called directly by the end user (never
delegated to) do not need this - only the inner leg of a
delegation chain does.

## 9.6 Left-Margin Auto-Sizing

The left margin (`mar[2]`) is sized automatically from the longest of the
labels actually drawn on that axis - the y-axis label and, where the
panel draws categorical level names as axis tick labels (e.g.
`spineplot()`, `cdplot()`), those level names too - via `.marginLines()`:

```r
lmar <- max(4.1, .marginLines(tickLabels, side = 2, las = 1, pad = 1))
```

Only the axis tick-label text itself sizes the margin this way; a y-axis
*label* (`ylab`) is drawn rotated and needs roughly constant width
regardless of its string length, so it must not be mixed into the same
`strwidth()`-based comparison as the (horizontal) tick labels - doing so
overestimates the needed margin for a long `ylab` and produces an
inconsistent layout.

Functions that delegate to a base-R plotting function with its own
internal margin logic (e.g. `spineplot()`) are not exempt from this -
`spineplot()` does respect `par(mar = ...)` set beforehand via
`.applyParFromDots()`; if labels still appear clipped, the cause is
almost always that the margin was not sized from the actual labels being
drawn (e.g. a leftover fixed value), not that the underlying function
ignores `par()`.

## 9.7 Dispatching `plot.Desc.*` Methods

`plot.Desc.*` S3 methods (one per `Desc` subclass) typically live in
`aurora`, not in `DescToolsX` where the `Desc` classes themselves are
constructed, because they require `aurora`'s internal plotting helpers
(9.2) which cannot be used from outside `aurora`'s namespace. See
3.1.1.1 for the resulting export requirements - this is the most common
case where the `@rawNamespace export()` pattern is needed.

Selecting multiple panels via `which = c(...)` never implies an internal
layout decision (no `mfrow` is ever set by the function itself).
Arranging multiple panels on one device (`par(mfrow = c(2, 1))` or
similar) is left entirely to the caller.

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

## 11.1b Function Titles (`@title`)

The title is the first thing a user sees in `?functionName`, in pkgdown reference pages, and in search results. It must be:

- **concise** — one line, no trailing period
- **informative** — says what the function does, not just what it is named
- **consistent** — follows the verb/noun rule below

### Verb vs. noun rule

The choice between a verb phrase and a noun phrase depends on what the function primarily does:

| Function type | Title form | Example |
|---|---|---|
| Computes / returns a statistical measure | **Noun phrase** — name the measure | `Gini Coefficient` |
| Transforms input into output | **Verb phrase** — starts with verb | `Convert CMY to CMYK` |
| Tests a hypothesis | **Noun phrase** — name of the test | `Jarque-Bera Test for Normality` |
| Extracts something from an object | **Verb phrase** | `Extract the Response Variable from a Fitted Model` |
| Plots something | **Noun phrase** — name of the chart type | `Correlation Matrix Plot` |
| Utility / helper | **Verb phrase** | `Find Highly Correlated Variables` |

The verb/noun distinction is not arbitrary — noun phrases signal "this *is* something", verb phrases signal "this *does* something". Statistical measures, tests, and plot types are things; transformations and utilities are actions.

### Approved opening verbs for verb-phrase titles

Use these consistently rather than inventing synonyms:

```text
Add         Append      Check       Coerce
Compare     Compute     Convert     Count
Create      Detect      Extract     Find
Format      Generate    Get         List
Load        Merge       Parse       Plot
Recode      Remove      Rename      Replace
Reshape     Resolve     Set         Split
Test        Transform
```

Do not use: `Calculate` (use `Compute`), `Make` (use `Create`), `Give` (use `Return`).

### Proper nouns and eponyms

- Retain the author's name exactly as in the literature: `Cramér's V`, `Hodges-Lehmann Estimator`, `Yeo-Johnson Transformation`
- Use the possessive (`'s`) when the measure is named after a person and the possessive is standard in the literature
- Accents are mandatory where they are part of the name: `Cramér`, `Fréchet`, `Spéarman` → note: Spearman has no accent
- Hyphenated eponyms: `Breslow-Day Test`, `Cochran-Armitage Trend Test`

### Parenthetical clarifications

A parenthetical is appropriate when the function name is an abbreviation or the title alone would be ambiguous:

```text
C-Statistic (ROC AUC)
Variance Inflation Factors (VIF / GVIF)
Confidence Interval for Coefficient of Variation (CV)
```

Do not use parentheticals to add synonyms that are not needed for disambiguation.

### Scope: singular functions that return multiple results

When a function computes a collection of related measures, make the plural explicit:

```text
# too narrow — implies one measure
"Ordinal Association Measure"

# correct — signals the function returns several
"Suite of Ordinal Association Measures"
```

### What to avoid

| Anti-pattern | Example | Better |
|---|---|---|
| Repeats only the function name | `gkGamma` → "Gamma" | "Goodman-Kruskal Gamma" |
| Too vague | "Summary Statistics" | "Univariate Descriptive Statistics Summary" |
| Verb where noun fits | "Compute the Gini Coefficient" | "Gini Coefficient" |
| Noun where verb fits | "Box-Cox Lambda Optimizer" | "Estimate Optimal Box-Cox Lambda" |
| Trailing period | "Pearson Correlation." | "Pearson Correlation" |

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

The package suite uses `@family` and `@concept` for function organization across all packages (DescToolsX, lumen, aurora, bedrock, alloy). The two tags serve strictly separated roles:

- `@family` = **functional navigation** — where does a user look for this function?
- `@concept` = **subject-matter tagging** — what is this function about?

This separation is authoritative. Do not use `@concept` for navigation, and do not use `@family` for subject-matter cross-linking.

## 12.2 `@family`

**Exactly one family per function.** No function may carry two `@family` tags.

### Naming convention

```text
<domain>.<category>
```

- `<domain>` — lowercase, identifies the functional area
- `<category>` — lowercase, identifies the sub-area within that domain
- Separator: dot (`.`)
- Both parts: lowercase, no camelCase, no hyphens

```r
# correct
@family assoc.ordinal
@family test.normality
@family plot.univariate
@family ci.proportion

# incorrect
@family topic.hypothesisTests   ← old style, do not use
@family assocOrdinal            ← no separator
@family assoc-ordinal           ← hyphen, not dot
```

### Decision rule

> Where would a user look for this function first?

If a function could plausibly belong to two families, assign it to the one that reflects its **primary use**, and add the secondary area as a `@concept` tag.

### Established families by package

**DescToolsX**

| Family | Contents |
|---|---|
| `assoc.agreement` | Kappa, ICC, CCC, Cronbach, rater agreement |
| `assoc.ordinal` | Gamma, Tau-a/b/c, Somers, Stuart, C-statistic |
| `assoc.nominal` | Cramér V, Phi, Lambda, Yule, mutual information |
| `assoc.continuous` | Pearson/Spearman/Polychor, Hoeffding D, partial correlation |
| `effect.size` | Cohen D/H, Glass Delta, Eta², OR, RR |
| `location` | Mean, median, mode, geometric/harmonic mean, robust location |
| `dispersion` | SD, variance, IQR, range, MAD, CV |
| `shape` | Skewness, kurtosis |
| `quantile` | Quantiles, order statistics |
| `inequality` | Gini, Atkinson, Herfindahl, Lorenz |
| `frequency` | freq, percTable, tOne, expFreq |
| `model.metrics` | MAE, RMSE, MAPE, Brier score, prediction error |
| `model.classification` | Sensitivity, specificity, confusion matrix |
| `transform` | Box-Cox, Yeo-Johnson, logSt, scaleX |
| `impute` | Imputation, outlier detection |
| `cut` | cutQ, cutAge — binning functions |
| `date.time` | Date/time extraction and arithmetic |
| `number.theory` | GCD, LCM, divisors |
| `anova` | Eta², sphericity, error terms |
| `descriptive` | desc(), abstract() |
| `utils` | Package options, conceptMap, getConcepts |

**lumen**

| Family | Contents |
|---|---|
| `test.normality` | Anderson-Darling, Lilliefors, Jarque-Bera, Shapiro-Francia |
| `test.gof` | Benford, runs test, KPSS, Bartels, von Neumann |
| `test.variance` | Levene, Siegel-Tukey, Moses, varTest |
| `test.location` | t-test, z-test, Hotelling T², Yuen, sign test |
| `test.ksample` | Cochran Q |
| `test.posthoc` | Dunn, Dunnett, Conover, Nemenyi, Scheffé, Steel, van der Waerden |
| `test.trend` | Cochran-Armitage, Jonckheere-Terpstra, Mantel, Page |
| `test.categorical` | Barnard, Breslow-Day, Woolf, Stuart-Maxwell, Lehmacher |
| `test.regression` | Breusch-Godfrey, Hosmer-Lemeshow, Le Cessie |
| `test.correlation` | corTest, corCI, Fisher Z |
| `ci.proportion` | Proportion CIs (binomCI, multinomCI, …) |
| `ci.general` | General CIs (bootstrap, MAD, mean difference, …) |
| `power` | Power and sample size |
| `distributions` | All d/p/q/r/m functions for non-standard distributions |
| `scores` | Normal scores |

**aurora**

| Family | Contents |
|---|---|
| `plot.univariate` | plotBar, plotBox, plotDens, plotViolin, plotDot, plotECDF, plotQQ, … |
| `plot.bivariate` | plotXY, plotCor, plotAssoc, plotMosaic, plotHeatmap, plotBubble, … |
| `plot.distribution` | plotProbDist, plotFun |
| `plot.special` | plotTimeSeries, plotMiss, plotPropCI, plotTreemap, plotWeb, … |
| `plot.s3` | plot.BlandAltman, plot.Desc.qn, plot.Desc.table, plot.Lc |
| `theme` | getTheme, setTheme, resetTheme, style |
| `format` | fm, fmCI, notation |
| `color` | All color conversion, mixing, palette functions |
| `geometry` | arc, band, circle, ellipse, polygon, ring, … |
| `graphics.utils` | axisBreak, barText, errBars, stamp, textLegend, lines.lm, … |
| `string` | strAbbr, strExtract, strPad, strVal, mgsub, … |
| `html` | as.html, as.img, toHtmlTable, preview.html |
| `ci.objects` | as.CI |
| `tables` | ftable.list |

**bedrock**

| Family | Contents |
|---|---|
| `data.manipulation` | appendX, collapseTable, dummy, nf, recodeX, sortX, … |
| `data.inspection` | allDuplicated, completeColumns, flags, isDichotomous, isNA, … |
| `vector.ops` | closest, coalesceX, locf, moveAvg, naIf, winsorize, … |
| `math.utils` | crossProd, linScale, percentRank, roundTo, unirootAll, … |
| `number.theory` | digitSum, factorize, fibonacci, isPrime, primes |
| `combinatorics` | combN, combPairs, permn, randGroupSplit, sampleX |
| `string.utilities` | mGsub, mReplace, strSplitToCol, strSplitToDummy |
| `table.utils` | collapseTable, multMerge, printCharMatrix |
| `pkg.introspection` | funArgs, funCalls, funKeywords, funList, getRdLabels |
| `label.utils` | label, dataDescription, openDataObject |
| `file.utils` | buildPath, fileExistURL, findDownload, pdfManual, readDownload |
| `data.utils` | resolveFormula, resolveGroups, resolveContingency |
| `utilities` | callIf, isNA |

**alloy**

| Family | Contents |
|---|---|
| `modelling` | fitMod, predict.FitMod, print.FitMod |
| `model.comparison` | tMod, tmodSummary |
| `regression.utils` | coefCI, pseudoR2, rSqCI, vif, refLevel, varImp, … |
| `roc` | roc, bestCut, confint.roc |
| `tree` | bestTree, cParam, leafRates, node, rules, splits, plot.rpart |
| `data.split` | splitTrainTest |


## 12.3 `@concept`

Each function should have **2–4** `@concept` tags.

### Naming convention

- **Singular** — concepts describe a subject area, not a set of instances
- **kebab-case** — lowercase, hyphen-separated
- **No redundancy** with the family name — if `@family assoc.ordinal` already says it, do not add `@concept ordinal-association`

```r
# correct
@concept rank-correlation
@concept confidence-interval
@concept robust-statistic
@concept extreme-value

# incorrect
@concept ordinal_association    ← underscore
@concept Robust Statistics      ← uppercase
@concept descriptive-statistics ← too generic, banned
@concept data-manipulation      ← too generic, banned
@concept graphics               ← too generic, banned
@concept plot                   ← redundant with family
```

### Banned concepts (too generic)

These concepts were rejected because they appear in 50+ functions and provide no navigational value:

```text
descriptive-statistics
data-manipulation
graphics
plot
association-measures   (outside assoc.* families)
package-utilities
```

### Three orthogonal axes

Concepts should be drawn from three complementary axes. A well-tagged function typically has one concept from each relevant axis:

**A — Subject area / domain**
```text
agreement          correlation        regression
classification     reliability        inequality
information-theory number-theory      geometry
```

**B — Data type / scale**
```text
ordinal            nominal            binary
continuous         multivariate       time-series
```

**C — Method / technique**
```text
bootstrap          confidence-interval  effect-size
goodness-of-fit    transformation       imputation
nonparametric      robust-statistic     variance-stabilization
feature-selection  prediction-error     calibration
```

### Authoritative concept vocabulary

The following concepts are approved and in use across the suite. Use these exact spellings:

```text
agreement               annotation              anova-effect-size
association-measure     asymmetric-association  attribute
autocorrelation         bar-chart               binary
binary-association      binary-outcome          binning
bivariate               bootstrap               boxplot
calibration             categorical-agreement   categorical-test
categorization          chi-square-based        classification
color                   color-conversion        combinatorics
concentration-index     concordance             confidence-interval
confusion-matrix        correlation             dataset
date-time               demographics            density
dispersion              distribution-function   distribution-summary
distribution-visualization  dotchart            dummy-coding
effect-size             exact-test              extreme-value
factor-handling         feature-selection       finance
formatting              formula                 frequency-table
geometry                goodness-of-fit         homogeneity
html                    hypothesis-test         imputation
inequality              information-theory      internal-consistency
interrater-agreement    introspection           k-sample
label                   latent-variable         line-chart
location                location-test           machine-learning
merge                   method-comparison       missing-value
model-evaluation        modelling               multicollinearity
multiple-testing        multivariate            nominal
nonlinear-association   nonlinear-mean          nonparametric
normality-test          number-formatting       number-theory
numeric-conversion      numerical-methods       order-statistic
ordering                ordinal                 outlier-detection
palette                 parametric              post-hoc
power                   prediction              prediction-error
programming             proportion              quantile
randomness              range                   rank-correlation
rater-data              regression              regression-diagnostics
reliability             reshape                 robust-statistic
roc                     sample-size             sampling
scatterplot             shape                   standardization
string-manipulation     summary                 table
table-summary           theme                   time-series
transformation          tree                    trend-test
type-test               variance-analysis       variance-component
variance-stabilization  variance-test
```

### Special cases

**`distribution-function` vs. `distribution-summary`**

- `distribution-function` — d/p/q/r/m functions in lumen (computational)
- `distribution-summary` — plot functions and quantile summaries (descriptive)

**`programming` vs. `introspection`**

- `introspection` — `funArgs`, `funCalls`, `funKeywords`, `funList`, `getRdLabels`, `rdTitle`  
  (functions that inspect other functions or the R session)
- `programming` — `callIf`, `mergeArgs`, `extractArgs`, `buildPath` and similar  
  (functions that assist in writing packages or calling functions programmatically)

**`modelling` vs. `model-evaluation`**

- `modelling` — model fitting: `fitMod`, `predict.FitMod`, tree functions, `splitTrainTest`
- `model-evaluation` — assessing model quality: ROC, Brier, pseudo-R², VIF, tMod, RMSE, calibration

**`association-measure`**

Retained as a cross-cutting concept tag **only within `assoc.*` families**. Do not use it outside these families.

## 12.4 Families vs. Concepts — summary

| Feature | `@family` | `@concept` |
|---|---|---|
| Cardinality | exactly one | 2–4 |
| Purpose | functional navigation | subject-matter tagging |
| Structure | `domain.category` | flat, singular, kebab-case |
| Stability | high — changes require suite-wide update | flexible — can be extended |
| Redundancy | never redundant with `@concept` | never redundant with `@family` |

## 12.5 `@seealso`

`@seealso` is reserved for close functional relationships: direct alternatives, or helper functions typically used together. It is not a substitute for `@concept` cross-linking.

---

# 13. DescToolsX — Core Vocabulary

## 13.1 Data Structure

| Meaning | Name | Comment |
|---|---|---|
| Vector, primary input | `x` | Standard |
| Second vector | `y` | Standard |
| Data frame / matrix | `data` | as in `lm`, `ggplot` |
| Formula | `formula` | Base-R compatible |
| File path / connection | `file` | as in `readLines`, `saveRDS` |

## 13.2 Grouping and Structure

| Meaning | Name |
|---|---|
| Grouping variable | `groups` |
| Number of groups | `nGroups` |
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
