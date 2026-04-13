
#' Simpson Diversity Indices
#'
#' Computes Simpson-type diversity indices, including the Gini-Simpson index,
#' the Hunter-Gaston index (bias-corrected for sample size), and the
#' Deltas-corrected Gini-Simpson index (bias-corrected for number of categories).
#'
#' The Gini-Simpson index is defined as \eqn{1 - \sum p_i^2}, where \eqn{p_i}
#' are the relative frequencies of categories.
#'
#' The Hunter-Gaston index is defined as
#' \eqn{1 - \sum n_i (n_i - 1) / (N (N - 1))}, where \eqn{n_i} are counts and
#' \eqn{N} is the total sample size.
#'
#' The Deltas correction is defined as
#' \eqn{(1 - \sum p_i^2) * k / (k - 1)}, where \eqn{k} is the number of categories.
#'
#' @param x A vector of observations (factor, character, numeric), or a
#'   vector of counts.
#' @param method Character string specifying the index to compute:
#'   \code{"gini"}, \code{"hunter"}, or \code{"deltas"}.
#' @param na.rm Logical. If \code{TRUE}, missing values are removed before
#'   computation. If \code{FALSE} and \code{x} contains \code{NA}, the result
#'   will be \code{NA}.
#'
#' @return A numeric value between 0 and 1. Returns \code{NA_real_} if input
#'   is invalid or empty.
#'
#' @details
#' All indices represent the probability that two randomly selected
#' observations belong to different categories.
#'
#' The Hunter-Gaston index corrects for finite sample size, while the
#' Deltas correction adjusts for a small number of categories.
#'
#' @examples
#' x <- c("A", "A", "B", "C", "C", "C")
#'
#' simpson(x, method = "gini")
#' simpson(x, method = "hunter")
#' simpson(x, method = "deltas")
#'
#' # Using counts directly
#' counts <- c(A = 2, B = 1, C = 3)
#' simpson(counts, method = "hunter")
#'
#' # With missing values
#' x <- c("A", "A", NA, "B")
#' simpson(x, method = "gini", na.rm = TRUE)
#'
#' @references
#' Sachs, L. (1997). \emph{Angewandte Statistik}. Springer.
#'
#' Hunter, P. R., & Gaston, M. A. (1988).
#' Numerical index of the discriminatory ability of typing systems.
#' \emph{Journal of Clinical Microbiology}, 26(11), 2465–2466.
#' https://doi.org/10.1128/jcm.26.11.2465-2466.1988
#'
#' Deltas, G. (2003).
#' The small-sample bias of the Gini coefficient:
#' Results and implications for empirical research.
#' \emph{Review of Economics and Statistics}, 85(1), 226–234.
#' https://doi.org/10.1162/rest.2003.85.1.226
#'


#' @export
simpson <- function(x, method = c("gini", "hunter", "deltas"), na.rm = FALSE) {
  
  method <- match.arg(method)
  
  # Handle NA for raw data (not counts)
  if(!is.numeric(x)) {
    if(na.rm) {
      x <- x[!is.na(x)]
    } else if(anyNA(x)) {
      return(NA_real_)
    }
  }
  
  # Determine counts
  if(is.numeric(x) && all(x >= 0)) {
    tt <- as.numeric(x)
  } else {
    tt <- as.numeric(table(x))
  }
  
  N <- sum(tt)
  
  if(length(tt) == 0 || N == 0) {
    return(NA_real_)
  }
  
  # Gini-Simpson
  if(method == "gini") {
    p <- tt / N
    return(1 - sum(p^2))
  }
  
  # Hunter-Gaston
  if(method == "hunter") {
    
    # fewer than 2 non-zero categories → no diversity
    if(sum(tt > 0) < 2) {
      return(0)
    }
    
    return(1 - sum(tt * (tt - 1)) / (N * (N - 1)))
  }
  
  # Deltas correction
  if(method == "deltas") {
    
    k <- sum(tt > 0)
    
    if(k < 2) {
      return(0)
    }
    
    p <- tt / N
    return((1 - sum(p^2)) * k / (k - 1))
  }
}