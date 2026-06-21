
#' Keep Only Significant Values in a Symmetric Matrix
#'
#' Replaces entries in a symmetric matrix (typically a correlation matrix)
#' with \code{NA} wherever the corresponding p-value exceeds a significance
#' threshold – retaining only the statistically supported associations.
#' Designed as a pre-processing step for \code{\link[aurora]{plotWeb}} and
#' \code{\link[aurora]{plotCor}}.
#'
#' @param m a symmetric numeric matrix. Typically the output of
#'   \code{\link[stats]{cor}}, but any symmetric matrix of effect sizes or
#'   association measures is accepted.
#' @param p a matrix of p-values of the same dimension as \code{m}. If
#'   \code{NULL} (default), p-values are computed from pairwise correlation
#'   tests on the columns of \code{data} (requires \code{data}).
#' @param data an optional numeric data frame or matrix. Used to compute
#'   \code{p} when \code{p = NULL}. Ignored if \code{p} is supplied.
#' @param alpha numeric; significance threshold. Entries where
#'   \code{p > alpha} are replaced by \code{NA}. Default \code{0.05}.
#' @param method character; the correlation test method passed to
#'   \code{\link[stats]{cor.test}} when computing p-values from \code{data}.
#'   One of \code{"pearson"} (default), \code{"spearman"}, or
#'   \code{"kendall"}.
#' @param diag logical; if \code{TRUE} (default), the diagonal is kept
#'   as-is (typically 1 for correlation matrices). If \code{FALSE}, the
#'   diagonal is also set to \code{NA}.
#'
#' @return A matrix of the same dimension and dimnames as \code{m}, with
#'   \code{NA} wherever \code{p > alpha}.
#'
#' @examples
#' # compute p-values on the fly from the raw data
#' plotWeb(keepSig(cor(mtcars), data = mtcars))
#'
#' # stricter threshold
#' plotCor(keepSig(cor(swiss), data = swiss, alpha = 0.01))
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
#' @seealso \code{\link[aurora]{plotWeb}}, \code{\link[aurora]{plotCor}},
#'   \code{\link[stats]{cor.test}}
#'
#' @family matrix.utils
#' @concept correlation
#' @concept hypothesis-testing
#'


#' @export
keepSig <- function(m, p = NULL, data = NULL,
                    alpha  = 0.05,
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
  
  out          <- m
  out[p > alpha] <- NA
  
  if (diag)
    diag(out) <- diag(m)
  
  out
}
