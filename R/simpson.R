
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
#' \eqn{(1 - \sum p_i^2) * k / (k - 1)}, where \eqn{k} is the number of
#' observed (non-empty) categories. The factor is the reciprocal of
#' \eqn{(k-1)/k}, the largest value the Gini-Simpson index can attain with
#' \eqn{k} categories, so the corrected index reaches 1 for a uniform
#' distribution over the observed categories.
#'
#' @param x a factor or character vector of observations, or a vector of
#'   non-negative counts. Note that a *numeric* vector is always read as
#'   counts, never as observations; wrap it in [factor()] to have it
#'   tabulated instead.
#' @param method character string specifying the index to compute:
#'   `"gini"`, `"hunter"`, or `"deltas"`
#' @param na.rm logical. If `TRUE`, missing values are removed before
#'   computation. If `FALSE` and `x` contains `NA`, the result
#'   will be `NA_real_`.
#'
#' @return a numeric scalar between 0 and 1. Returns `NA_real_` if input
#'   is invalid or empty.
#'
#' @details
#' All indices represent the probability that two randomly selected
#' observations belong to different categories.
#'
#' The Hunter-Gaston index corrects for finite sample size, while the
#' Deltas correction adjusts for a small number of observed categories. Note
#' that the finite-sample correction \eqn{N/(N-1)} applied to the
#' Gini-Simpson index reproduces the Hunter-Gaston index exactly; the two are
#' the same adjustment with \eqn{N} and \eqn{k} in the correction factor.
#'
#' A sample concentrated in a single category is perfectly homogeneous rather
#' than undefined, so `"gini"` and `"hunter"` return 0 for it. Only
#' the Deltas correction requires \eqn{k \ge 2}, since \eqn{k - 1} appears in
#' its denominator.
#'
#' When `x` is numeric, it is treated as a vector of counts. Non-integer
#' values produce a warning; the Hunter-Gaston index requires integer counts.
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
#' # a numeric vector of observations must be tabulated first, otherwise
#' # its values are read as counts
#' simpson(factor(c(1, 1, 2, 2, 3)), method = "gini")
#'
#' # With missing values
#' x <- c("A", "A", NA, "B")
#' simpson(x, method = "gini", na.rm = TRUE)
#'
#' @references
#' Sachs, L. (1997). *Angewandte Statistik*. Springer.
#'
#' Hunter, P. R., & Gaston, M. A. (1988).
#' Numerical index of the discriminatory ability of typing systems.
#' *Journal of Clinical Microbiology*, 26(11), 2465-2466.
#' https://doi.org/10.1128/jcm.26.11.2465-2466.1988
#'
#' Deltas, G. (2003).
#' The small-sample bias of the Gini coefficient:
#' Results and implications for empirical research.
#' *Review of Economics and Statistics*, 85(1), 226-234.
#' https://doi.org/10.1162/rest.2003.85.1.226
#'
#'
#' @family diversity.concentration
#' @concept concentration-index
#'
#'
#' @export
simpson <- function(x, method = c("gini", "hunter", "deltas"), na.rm = FALSE) {
  
  method <- match.arg(method)
  
  if (!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm))
    stop("'na.rm' must be a single non-missing logical value")
  
  # Coerce single-row data frame (e.g. vegan BCI[1,]) to numeric vector
  if (is.data.frame(x)) {
    x <- unlist(x, use.names = FALSE)
  }
  
  # Handle NA uniformly for all input types (fix: was skipped for numeric)
  if (na.rm) {
    x <- x[!is.na(x)]
  } else if (anyNA(x)) {
    return(NA_real_)
  }
  
  
  # Determine counts (preserve names)
  if (is.numeric(x)) {
    if (any(x < 0)) {
      stop("Counts must be non-negative")
    }
    if (any(x != floor(x))) {
      warning("Non-integer counts detected; Hunter-Gaston index requires integer counts")
    }
    tt <- x
  } else {
    tt <- table(x)
  }
  
  
  N <- sum(tt)
  
  # Empty input
  if (length(tt) == 0 || N == 0) {
    warning("Empty input: x has no observations")
    return(NA_real_)
  }
  
  # Gini-Simpson
  if (method == "gini") {
    p <- tt / N
    return(unname(1 - sum(p^2)))
  }
  
  # Hunter-Gaston
  if (method == "hunter") {
    
    # N >= 2 is what the index needs: with a single observation there is no
    # pair to draw. A single non-empty category is not a degenerate case at
    # all - the answer is 0, the same value "gini" reports for it.
    if (N < 2) {
      warning("Hunter-Gaston index requires N >= 2; returning NA")
      return(NA_real_)
    }
    
    return(unname(1 - sum(tt * (tt - 1)) / (N * (N - 1))))
  }
  
  # Deltas correction
  if (method == "deltas") {
    k <- sum(tt > 0)
    if (k < 2) {
      warning("Deltas correction requires at least 2 non-empty categories (k >= 2); returning NA")
      return(NA_real_)
    }
    
    p <- tt / N
    return(unname((1 - sum(p^2)) * k / (k - 1)))
  }
}
