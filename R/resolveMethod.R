
#' Resolve method argument including sentinel ".all"
#'
#' Internal helper that resolves a character argument against the
#' formal default choices of the calling function. If \code{method}
#' equals the sentinel value \code{".all"}, all available choices are
#' returned. Otherwise \code{match.arg()} is applied.
#'
#' @param method A character value specifying one or more methods.
#' @param several.ok Logical. Should multiple matches be allowed?
#' @param fn The calling function whose formal argument definition
#'   contains the available method choices. Defaults to the parent
#'   function.
#'
#' @return A character vector of matched method names.
#'
#' @keywords internal
#' @noRd
.resolveMethod <- function(method,
                           several.ok = FALSE,
                           fn = sys.function(sys.parent())) {
  
  choices <- eval(formals(fn)$method)
  
  # Falls NULL -> Default = erster Eintrag
  if (is.null(method))
    return(choices[1])
  
  # Sentinel für alle Methoden
  if (identical(method, ".all"))
    return(choices)
  
  match.arg(method,
            choices = choices,
            several.ok = several.ok)
}



