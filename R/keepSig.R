
#' Keep Only Significant Values in a Symmetric Matrix
#'
#' Replaces entries in a symmetric matrix (typically a correlation matrix)
#' with \code{NA} wherever the corresponding p-value exceeds a significance
#' threshold - retaining only the statistically supported associations.
#' Designed as a pre-processing step for \code{\link[pharos]{plotWeb}} and
#' \code{\link[pharos]{plotCor}}.
#'
#' @param m a symmetric numeric matrix. Typically the output of
#'   \code{\link[stats]{cor}}, but any symmetric matrix of effect sizes or
#'   association measures is accepted.
#' @param p a matrix of p-values of the same dimension as \code{m}. If
#'   \code{NULL} (default), p-values are computed from pairwise correlation
#'   tests on the columns of \code{data} (requires \code{data}).
#' @param data an optional numeric data frame or matrix. Used to compute
#'   \code{p} when \code{p = NULL}. Ignored if \code{p} is supplied.
#' @param sig.level numeric; significance threshold. Entries where
#'   \code{p > sig.level} are replaced by \code{NA}. Default \code{0.05}.
#' @param method character; the correlation test method passed to
#'   \code{\link[stats]{cor.test}} when computing p-values from \code{data}.
#'   One of \code{"pearson"} (default), \code{"spearman"}, or
#'   \code{"kendall"}.
#' @param diag logical; if \code{TRUE} (default), the diagonal is kept
#'   as-is (typically 1 for correlation matrices). If \code{FALSE}, the
#'   diagonal is also set to \code{NA}.
#'
#' @return matrix with the same dimensions and dimnames as \code{m}, with
#' \code{NA} wherever \code{p > sig.level}
#'
#' @examples
#' # compute p-values on the fly from the raw data
#' keepSig(cor(mtcars), data = mtcars)
#'
#' # stricter threshold, and drop the diagonal as well
#' keepSig(cor(swiss), data = swiss, sig.level = 0.01, diag = FALSE)
#'
#' # the intended use is as a pre-processing step for the plots
#' pharos::plotWeb(keepSig(cor(mtcars), data = mtcars))
#'
#' # supply a pre-computed p-value matrix
#' m <- cor(mtcars)
#' p <- outer(
#'   (vars <- colnames(mtcars)), vars,
#'   Vectorize(function(v1, v2)
#'     cor.test(mtcars[[v1]], mtcars[[v2]])$p.value)
#' )
#' dimnames(p) <- list(vars, vars)
#' plotWeb(keepSig(m, p = p))
#'
#' @seealso [pharos::plotWeb], [pharos::plotCor], [lumen::corTest], [stats::cor.test]
#'   
#'
#' @family assoc.continuous
#' @concept correlation
#' @concept multiple-testing
#' @export
keepSig <- function(m, p = NULL, data = NULL,
                    sig.level  = 0.05,
                    method = c("pearson", "spearman", "kendall"),
                    diag   = TRUE) {
  
  method <- match.arg(method)
  
  # --- compute p-values if not supplied ---
  if (is.null(p)) {
    
    if (is.null(data))
      stop("supply either 'p' (a p-value matrix) or 'data' (to compute it).")
    
    data <- as.matrix(data)
    vars <- colnames(data)
    
    p <- outer(
      vars, vars,
      Vectorize(function(v1, v2) {
        if (v1 == v2) return(NA_real_)
        cor.test(data[, v1], data[, v2], method = method)$p.value
      })
    )
    dimnames(p) <- list(vars, vars)
  }
  
  if (!identical(dim(m), dim(p)))
    stop("'m' and 'p' must have the same dimensions.")
  
  if (!is.numeric(sig.level) || length(sig.level) != 1L ||
      !is.finite(sig.level) || sig.level <= 0 || sig.level > 1)
    stop("'sig.level' must be a single number in (0, 1].")

  out <- m

  # A logical index containing NA skips those positions when the
  # replacement has length 1, so cells whose p-value is NA - which is
  # exactly what the internally computed diagonal is - were left
  # untouched. That made `diag` a no-op in BOTH directions: TRUE restored
  # a diagonal that had never changed, and FALSE did not blank it, which
  # is what the documentation promises. Decide the diagonal explicitly.
  drop <- !is.na(p) & p > sig.level
  out[drop] <- NA

  if (diag)
    diag(out) <- diag(m)
  else
    diag(out) <- NA

  out
}
