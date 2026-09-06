
#' Identify Highly Correlated Variables
#'
#' Identifies variables in a correlation matrix that should be removed due to
#' high pairwise correlations above a specified cutoff. The algorithm uses a
#' greedy approach similar to `caret::findCorrelation()`, but extends it
#' with flexible scoring methods and multiple output formats.
#'
#' @param x a symmetric correlation matrix
#' @param cutoff numeric threshold in (0, 1). Pairs with absolute correlation
#'   above this value are considered too highly correlated.
#' @param method character string specifying how variable importance is scored.
#'   One of `"mean"`, `"max"`, or `"median"`. Default is `"mean"`.
#' @param output character string specifying the return format:
#'   \itemize{
#'     \item `"index"` for indices of variables to remove (default)
#'     \item `"names"` for column names of variables to remove
#'     \item `"logical"` for a logical vector indicating removed variables
#'     \item `"report"` for a detailed list of removed and retained
#'       variables and the decision log
#'   }
#' @param verbose logical; if `TRUE`, progress information is printed
#'
#' @details
#' The function iteratively examines pairs of variables with correlations above
#' `cutoff`. For each such pair, the variable with the higher overall
#' correlation (based on `method`) is removed.
#'
#' The scoring is computed once at the beginning and kept fixed throughout,
#' ensuring deterministic and efficient behavior. Because the columns are
#' then processed in decreasing score order, the variable removed from a
#' pair is always the earlier - i.e. the higher-scoring - one.
#'
#' This is a greedy heuristic and does not guarantee a globally optimal solution.
#'
#' @return depending on `output`:
#' \describe{
#'   \item{`"index"`}{integer vector of column indices to remove}
#'   \item{`"names"`}{character vector of column names}
#'   \item{`"logical"`}{logical vector with one element per column}
#'   \item{`"report"`}{list with elements `removed`, `kept`,
#'     and `log`}
#' }
#'
#' @examples
#' set.seed(123)
#' m <- matrix(rnorm(100), ncol = 5)
#' colnames(m) <- paste0("V", 1:5)
#' cmat <- cor(m)
#'
#' findCorrX(cmat, cutoff = 0.8)
#' findCorrX(cmat, cutoff = 0.8, method = "max", output = "names")
#'




#' @family assoc.continuous
#' @concept correlation
#' @concept feature-selection
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
  
  # isSymmetric() on the unnamed matrix: all.equal() also compares
  # dimnames, so a matrix whose row and column names differ was rejected
  # as asymmetric even when it was numerically symmetric
  if (!isSymmetric(unname(x), tol = 1e-8))
    stop("x must be a symmetric correlation matrix")
  
  if (nrow(x) < 2)
    stop("Need at least two variables")
  
  if (!is.numeric(cutoff) || length(cutoff) != 1 || cutoff <= 0 || cutoff >= 1)
    stop("cutoff must be a numeric value between 0 and 1 (exclusive).")
  
  if (output == "names" && is.null(colnames(x)))
    stop("x has no column names; use output = 'index' instead.")
  
  # --- Preprocessing ---
  x <- abs(x)
  diag(x) <- NA
  
  scoreFun <- switch(method,
                      mean   = function(v) mean(v, na.rm = TRUE),
                      max    = function(v) max(v, na.rm = TRUE),
                      median = function(v) median(v, na.rm = TRUE)
  )
  
  scores <- apply(x, 2, scoreFun)
  ord <- order(scores, decreasing = TRUE)
  
  xOrd <- x[ord, ord, drop = FALSE]
  removed <- rep(FALSE, nrow(xOrd))
  
  removalLog <- list()
  
  # --- Main loop ---
  for (i in seq_len(nrow(xOrd) - 1)) {
    
    if (removed[i]) next
    
    for (j in seq.int(i + 1L, nrow(xOrd))) {
      
      if (removed[j]) next
      
      cij <- xOrd[i, j]
      
      if (is.na(cij)) next
      
      if (cij > cutoff) {
        
        if (verbose) {
          message(sprintf(
            "Comparing %d vs %d (corr=%.3f): score_i=%.3f, score_j=%.3f",
            ord[i], ord[j], cij, scores[ord[i]], scores[ord[j]]
          ))
        }
        
        # ord sorts by decreasing score, so for i < j the first branch
        # always held and the else was unreachable. Kept as a single
        # statement rather than a comparison that can only go one way.
        removed[i] <- TRUE
        removalLog[[length(removalLog) + 1L]] <-
          list(remove = ord[i], keep = ord[j], corr = cij)
        break
      }
    }
  }
  
  removedIdx <- ord[removed]
  keptIdx <- ord[!removed]
  
  # --- Output ---
  out <- switch(output,
                index   = removedIdx,
                names   = colnames(x)[removedIdx],
                logical = {
                  res <- rep(FALSE, ncol(x))
                  res[removedIdx] <- TRUE
                  res
                },
                report  = list(
                  removed = removedIdx,
                  kept = keptIdx,
                  log = removalLog
                )
  )
  
  return(out)
}

