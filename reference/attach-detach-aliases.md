# Attach and Remove Short Aliases for Selected DescToolsX Functions

Assigns a set of convenient short-name aliases into an environment (by
default the global environment). This is an explicit opt-in: nothing is
exported under these names, so no namespace conflicts arise when the
package is merely attached with
[`library()`](https://rdrr.io/r/base/library.html).

## Usage

``` r
attachAliases(envir = .GlobalEnv, overwrite = FALSE)

detachAliases(envir = .GlobalEnv)
```

## Arguments

- envir:

  the environment into which the aliases are assigned. Defaults to
  `.GlobalEnv`. Supply a different environment (e.g. the calling frame
  via [`environment()`](https://rdrr.io/r/base/environment.html)) if you
  want script-local aliases.

- overwrite:

  logical; if `TRUE`, objects already present under an alias name are
  replaced. Default is `FALSE`, in which case such names are skipped
  with a warning.

## Value

invisibly, a character vector of the alias names that were created,
resp. removed

## Details

The short-name aliases that were previously created by `attachAliases()`
from the given environment can be removed by `attachAliases()`. Names
that do not exist, or that hold something other than the aliased
function, are left untouched.

The following aliases are created:

|           |                               |
|-----------|-------------------------------|
| **Alias** | **Function**                  |
| `or`      | [`oddsRatio()`](oddsRatio.md) |
| `rr`      | [`relRisk()`](relRisk.md)     |

Aliases are plain function objects assigned with
[`assign()`](https://rdrr.io/r/base/assign.html); they are
indistinguishable from calling the original function directly. To remove
them, call `detachAliases()` or simply `rm(or, rr, envir = .GlobalEnv)`.

An existing object of the same name is not overwritten unless
`overwrite = TRUE` is given, so a user's own `or` or `rr` cannot be
clobbered by accident.

## See also

`detachAliases`, [`oddsRatio`](oddsRatio.md), [`relRisk`](relRisk.md)

## Examples

``` r
# aliases are written into an environment; use a local one in scripts
e <- new.env()
attachAliases(envir = e)
#> DescToolsX: aliases attached (or, rr)

m <- matrix(c(10, 20, 30, 40), nrow = 2)
e$or(m)   # same as oddsRatio(m)
#> [1] 0.6666667
e$rr(m)   # same as relRisk(m)
#> [1] 0.75

detachAliases(envir = e)
#> DescToolsX: aliases removed (or, rr)
```
