
#' Attach Short Aliases for Selected DescToolsX Functions
#'
#' Assigns a set of convenient short-name aliases into an environment
#' (by default the global environment).  This is an explicit opt-in: nothing
#' is exported under these names, so no namespace conflicts arise when the
#' package is merely attached with \code{library()}.
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
#' @param envir The environment into which the aliases are assigned.
#'   Defaults to \code{.GlobalEnv}.  Supply a different environment
#'   (e.g. the calling frame via \code{environment()}) if you want
#'   script-local aliases.
#'
#' @return Invisibly returns a named character vector of the aliases that
#'   were created.
#'
#' @seealso \code{\link{detachAliases}}, \code{\link{oddsRatio}},
#'   \code{\link{relRisk}}
#'
#' @examples
#' attachAliases()
#'
#' m <- matrix(c(10, 20, 30, 40), nrow = 2)
#' or(m)   # same as oddsRatio(m)
#' rr(m)   # same as relRisk(m)
#'
#' detachAliases()
#'
#' @family convenience
#' @concept convenience


#' @export
attachAliases <- function(envir = .GlobalEnv) {
  
  aliases <- c(
    or = "oddsRatio",
    rr = "relRisk"
  )
  
  for (alias in names(aliases)) {
    assign(alias, get(aliases[[alias]], envir = asNamespace("DescToolsX")),
           envir = envir)
  }
  
  message("DescToolsX: aliases attached (", paste(names(aliases), collapse = ", "), ")")
  invisible(aliases)
}


#' Remove Aliases Created by attachAliases
#'
#' Removes the short-name aliases that were previously created by
#' \code{\link{attachAliases}()} from the given environment.  Aliases
#' that do not exist in the environment are silently ignored.
#'
#' @param envir The environment from which the aliases are removed.
#'   Must match the \code{envir} argument used in \code{attachAliases()}.
#'   Defaults to \code{.GlobalEnv}.
#'
#' @return Invisibly returns a character vector of the alias names that
#'   were removed.
#'
#' @seealso \code{\link{attachAliases}}
#'
#' @examples
#' attachAliases()
#' detachAliases()
#'
#' @family convenience
#' @concept convenience
#' @export
detachAliases <- function(envir = .GlobalEnv) {
  
  aliases <- c("or", "rr")
  exists_ <- aliases[vapply(aliases, exists, logical(1L), envir = envir,
                            inherits = FALSE)]
  if (length(exists_))
    rm(list = exists_, envir = envir)
  
  message("DescToolsX: aliases removed (", paste(exists_, collapse = ", "), ")")
  invisible(exists_)
}

