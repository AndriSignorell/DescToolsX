
#' Identify Highly Correlated Variables
#'
#' Identifies variables in a correlation matrix that should be removed due to
#' high pairwise correlations above a specified cutoff. The algorithm uses a
#' greedy approach similar to \code{caret::findCorrelation()}, but extends it
#' with flexible scoring methods and multiple output formats.
#'
#' @param x A symmetric correlation matrix.
#' @param cutoff Numeric threshold in (0, 1). Pairs with absolute correlation
#'   above this value are considered too highly correlated.
#' @param method Character string specifying how variable importance is scored.
#'   One of \code{"mean"}, \code{"max"}, or \code{"median"}. Default is \code{"mean"}.
#' @param output Character string specifying the return format:
#'   \itemize{
#'     \item \code{"index"}: indices of variables to remove (default)
#'     \item \code{"names"}: column names of variables to remove
#'     \item \code{"logical"}: logical vector indicating removed variables
#'     \item \code{"report"}: detailed list with removed, kept variables and decision log
#'   }
#' @param verbose Logical; if \code{TRUE}, prints progress information.
#'
#' @details
#' The function iteratively examines pairs of variables with correlations above
#' \code{cutoff}. For each such pair, the variable with the higher overall
#' correlation (based on \code{method}) is removed.
#'
#' The scoring is computed once at the beginning and kept fixed throughout,
#' ensuring deterministic and efficient behavior.
#'
#' This is a greedy heuristic and does not guarantee a globally optimal solution.
#'
#' @return Depending on \code{return}:
#' \itemize{
#'   \item \code{"index"}: integer vector of column indices to remove
#'   \item \code{"names"}: character vector of column names
#'   \item \code{"logical"}: logical vector (length = ncol(x))
#'   \item \code{"report"}: list with elements \code{removed}, \code{kept}, \code{log}
#' }
#'
#' @examples
#' set.seed(123)
#' m <- matrix(rnorm(100), ncol = 5)
#' colnames(m) <- paste0("V", 1:5)
#' cmat <- cor(m)
#'
#' findCorrX(cmat, cutoff = 0.8)
#' findCorrX(cmat, cutoff = 0.8, method = "max", return = "names")
#'



#' @family correlation
#' @concept correlation
#' @concept data-inspection
#' @concept regression
#'
#'
#' @export
findCorrX <- function(x,
                      cutoff = 0.9,
                      method = c("mean", "max", "median"),
                      output = c("index", "names", "logical", "report"),
                      verbose = FALSE) {
  
  # --- Argument handling ---
  method <- match.arg(method)
  output <- match.arg(output)
  
  if (!is.matrix(x))
    stop("x must be a matrix")
  
  if (!isTRUE(all.equal(x, t(x), tolerance = 1e-8)))
    stop("x must be a symmetric correlation matrix")
  
  if (nrow(x) < 2)
    stop("Need at least two variables")
  
  if (!is.numeric(cutoff) || length(cutoff) != 1 || cutoff <= 0 || cutoff >= 1)
    stop("cutoff must be a numeric value between 0 and 1 (exclusive).")
  
  if (output == "names" && is.null(colnames(x)))
    stop("x has no column names; use return = 'index' instead.")
  
  # --- Preprocessing ---
  x <- abs(x)
  diag(x) <- NA
  
  score_fun <- switch(method,
                      mean   = function(v) mean(v, na.rm = TRUE),
                      max    = function(v) max(v, na.rm = TRUE),
                      median = function(v) median(v, na.rm = TRUE)
  )
  
  scores <- apply(x, 2, score_fun)
  ord <- order(scores, decreasing = TRUE)
  
  x_ord <- x[ord, ord, drop = FALSE]
  removed <- rep(FALSE, nrow(x_ord))
  
  removal_log <- list()
  
  # --- Main loop ---
  for (i in seq_len(nrow(x_ord) - 1)) {
    
    if (removed[i]) next
    
    for (j in (i + 1):nrow(x_ord)) {
      
      if (removed[j]) next
      
      cij <- x_ord[i, j]
      
      if (is.na(cij)) next
      
      if (cij > cutoff) {
        
        if (verbose) {
          message(sprintf(
            "Comparing %d vs %d (corr=%.3f): score_i=%.3f, score_j=%.3f",
            ord[i], ord[j], cij, scores[ord[i]], scores[ord[j]]
          ))
        }
        
        if (scores[ord[i]] >= scores[ord[j]]) {
          removed[i] <- TRUE
          removal_log[[length(removal_log) + 1]] <-
            list(remove = ord[i], keep = ord[j], corr = cij)
          break
        } else {
          removed[j] <- TRUE
          removal_log[[length(removal_log) + 1]] <-
            list(remove = ord[j], keep = ord[i], corr = cij)
        }
      }
    }
  }
  
  removed_idx <- ord[removed]
  kept_idx <- ord[!removed]
  
  # --- Output ---
  out <- switch(output,
                index   = removed_idx,
                names   = colnames(x)[removed_idx],
                logical = {
                  res <- rep(FALSE, ncol(x))
                  res[removed_idx] <- TRUE
                  res
                },
                report  = list(
                  removed = removed_idx,
                  kept = kept_idx,
                  log = removal_log
                )
  )
  
  return(out)
}


