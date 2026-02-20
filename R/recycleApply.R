
#' Apply a scalar function with strict recycling
#'
#' Internal helper that applies a non-vectorised function \code{FUN}
#' elementwise to recycled arguments. All arguments are first recycled
#' using \code{.recycle()}, then \code{FUN} is evaluated for each
#' observation.
#'
#' If all return values are length 1, the result is simplified to an
#' atomic vector. Otherwise, a list is returned.
#'
#' @param FUN A function to be applied elementwise.
#' @param ... Arguments passed to \code{FUN}. These are recycled to a
#'   common length using \code{.recycle()}.
#'
#' @return A vector (if scalar results) or a list.
#'
#' @keywords internal
#' @noRd
.recycleApply <- function(FUN, ...) {
  
  rc <- recycle(...)
  maxdim <- attr(rc, "maxdim")
  
  out <- vector("list", maxdim)
  
  for (i in seq_len(maxdim)) {
    args_i <- lapply(rc, `[[`, i)
    out[[i]] <- do.call(FUN, args_i)
  }
  
  if (all(lengths(out) == 1L))
    return(unlist(out))
  
  attr(out, "recycle") <- rc
  
  return(out)
  
}
