
# The single source of truth for the alias set - attachAliases() and
# detachAliases() must never disagree about which names they manage.
# Referencing the function objects directly (rather than looking them up
# by string in asNamespace("DescToolsX")) keeps the package name out of
# the code and lets R CMD check see the dependency.
#' @noRd
.aliasTable <- function()
  list(or = oddsRatio,
       rr = relRisk)


#' Attach and Remove Short Aliases for Selected DescToolsX Functions
#'
#' Assigns a set of convenient short-name aliases into an environment
#' (by default the global environment).  This is an explicit opt-in: nothing
#' is exported under these names, so no namespace conflicts arise when the
#' package is merely attached with \code{library()}.
#' 
#' The short-name aliases that were previously created by
#' \code{\link{attachAliases}()} from the given environment can be removed 
#' by \code{\link{attachAliases}()}. Names that do
#' not exist, or that hold something other than the aliased function, are
#' left untouched.
#'
#' @details
#' The following aliases are created:
#' \tabular{ll}{
#'   \bold{Alias} \tab \bold{Function}     \cr
#'   \code{or}    \tab \code{oddsRatio()}  \cr
#'   \code{rr}    \tab \code{relRisk()}    \cr
#' }
#'
#' Aliases are plain function objects assigned with \code{\link{assign}()};
#' they are indistinguishable from calling the original function directly.
#' To remove them, call \code{\link{detachAliases}()} or simply
#' \code{rm(or, rr, envir = .GlobalEnv)}.
#'
#' An existing object of the same name is not overwritten unless
#' \code{overwrite = TRUE} is given, so a user's own \code{or} or
#' \code{rr} cannot be clobbered by accident.
#'
#' @name attach-detach-aliases
#' @param envir the environment into which the aliases are assigned.
#'   Defaults to \code{.GlobalEnv}.  Supply a different environment
#'   (e.g. the calling frame via \code{environment()}) if you want
#'   script-local aliases.
#' @param overwrite logical; if \code{TRUE}, objects already present under
#'   an alias name are replaced. Default is \code{FALSE}, in which case
#'   such names are skipped with a warning.
#'
#' @return invisibly, a character vector of the alias names that were
#'   created, resp. removed
#'
#' @seealso \code{\link{detachAliases}}, \code{\link{oddsRatio}},
#'   \code{\link{relRisk}}
#'
#' @examples
#' # aliases are written into an environment; use a local one in scripts
#' e <- new.env()
#' attachAliases(envir = e)
#'
#' m <- matrix(c(10, 20, 30, 40), nrow = 2)
#' e$or(m)   # same as oddsRatio(m)
#' e$rr(m)   # same as relRisk(m)
#'
#' detachAliases(envir = e)
#'
#' @family convenience
#' @concept convenience
#' @export
attachAliases <- function(envir = .GlobalEnv, overwrite = FALSE) {

  aliasFuns <- .aliasTable()

  taken <- vapply(names(aliasFuns), exists, logical(1L),
                  envir = envir, inherits = FALSE)

  if (!overwrite && any(taken)) {
    warning("not attached, name(s) already in use: ",
            paste(names(aliasFuns)[taken], collapse = ", "),
            ". Use overwrite = TRUE to replace them.")
    aliasFuns <- aliasFuns[!taken]
  }

  for (alias in names(aliasFuns))
    assign(alias, aliasFuns[[alias]], envir = envir)

  if (length(aliasFuns))
    message("DescToolsX: aliases attached (",
            paste(names(aliasFuns), collapse = ", "), ")")

  invisible(names(aliasFuns))
}


#' @rdname attach-detach-aliases
#' @export
detachAliases <- function(envir = .GlobalEnv) {

  aliasFuns <- .aliasTable()

  # Removing every name unconditionally would delete a user's own object
  # that happens to be called 'or' or 'rr' - identity is checked so that
  # only what attachAliases() put there is taken away again.
  removable <- vapply(
    names(aliasFuns),
    function(nm)
      exists(nm, envir = envir, inherits = FALSE) &&
        identical(get(nm, envir = envir, inherits = FALSE), aliasFuns[[nm]]),
    logical(1L))

  removed <- names(aliasFuns)[removable]

  if (length(removed)) {
    rm(list = removed, envir = envir)
    message("DescToolsX: aliases removed (",
            paste(removed, collapse = ", "), ")")
  }

  invisible(removed)
}
