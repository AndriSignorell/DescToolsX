
#' Simpson Diversity Indices
#'
#' Computes Simpson-type diversity indices: the Gini-Simpson index, the
#' Hunter-Gaston index (its unbiased, finite-sample version) and the index of
#' qualitative variation (the Gini-Simpson index rescaled to a maximum of 1).
#'
#' The Gini-Simpson index is defined as \eqn{1 - \sum p_i^2}, where \eqn{p_i}
#' are the relative frequencies of the categories.
#'
#' The Hunter-Gaston index is defined as
#' \eqn{1 - \sum n_i (n_i - 1) / (N (N - 1))}, where \eqn{n_i} are the counts
#' and \eqn{N} is the total sample size. It equals the Gini-Simpson index
#' times \eqn{N / (N - 1)}. The complement, \eqn{\sum n_i (n_i - 1) / (N (N - 1))},
#' is Simpson's unbiased \eqn{\lambda}, the probability that two observations
#' are of the *same* category.
#'
#' The index of qualitative variation (IQV) is defined as
#' \eqn{(1 - \sum p_i^2) \, K / (K - 1)}, where \eqn{K} is the number of
#' categories. Since \eqn{(K - 1)/K} is the largest value the Gini-Simpson
#' index can attain with \eqn{K} categories, the IQV reaches 1 exactly for a
#' uniform distribution over all \eqn{K} categories.
#'
#' @param x a factor or character vector of observations, or a vector of
#'   non-negative counts. Note that a *numeric* vector is always read as
#'   counts, never as observations; wrap it in [factor()] to have it
#'   tabulated instead. A data frame with a single row of numeric columns
#'   (e.g. one site of a community matrix) is read as counts.
#' @param method character string specifying the index to compute:
#'   `"gini"`, `"hunter"`, or `"iqv"`.
#' @param categories the possible categories for `method = "iqv"`: either
#'   their number (a single number) or the categories themselves (a vector,
#'   e.g. `levels(x)`). If `NULL` (default), the number of observed,
#'   non-empty categories is used. Ignored, with a warning, for the other
#'   methods.
#' @param na.rm logical. If `TRUE`, missing values are removed before
#'   computation. If `FALSE` and `x` contains `NA`, the result
#'   will be `NA_real_`.
#'
#' @return a numeric scalar between 0 and 1. `NA_real_` (with a warning)
#'   when the index is undefined: no observations, \eqn{N < 2} for
#'   `"hunter"`, \eqn{K < 2} for `"iqv"`; `NA_real_` without a warning for
#'   missing values with `na.rm = FALSE`. Invalid input (negative, infinite
#'   or, for `"hunter"`, non-integer counts; invalid `categories`) is an
#'   error.
#'
#' @details
#' `"gini"` and `"hunter"` are the probability that two randomly selected
#' observations belong to different categories, drawn with and without
#' replacement respectively. `"iqv"` is not a probability but the
#' Gini-Simpson index relative to its maximum.
#'
#' **Number of categories in the IQV.** \eqn{K} should be the number of
#' categories that were *possible*, not only those observed. With the
#' default, a sample spread evenly over 3 of 5 possible categories gets an
#' IQV of 1, although it is far from the maximum diversity the coding scheme
#' allows. Supply `categories` whenever the set of categories is known; for a
#' factor, `categories = levels(x)` uses all levels, including empty ones.
#'
#' A sample concentrated in a single category is perfectly homogeneous rather
#' than undefined, so `"gini"` returns 0 for it, and so does `"hunter"` as
#' long as \eqn{N \ge 2}. The IQV requires \eqn{K \ge 2}, since
#' \eqn{K - 1} appears in its denominator; with `categories` given, a single
#' occupied category gives an IQV of 0.
#'
#' When `x` is numeric, it is treated as a vector of counts. Relative
#' frequencies are fine for `"gini"` and `"iqv"`; the Hunter-Gaston index
#' counts pairs of observations and therefore requires integer counts
#' (non-integer counts can push it above 1).
#'
#' @examples
#' x <- c("A", "A", "B", "C", "C", "C")
#'
#' simpson(x, method = "gini")
#' simpson(x, method = "hunter")
#' simpson(x, method = "iqv")
#'
#' # the same sample, if five categories were possible
#' simpson(x, method = "iqv", categories = 5)
#' simpson(x, method = "iqv", categories = c("A", "B", "C", "D", "E"))
#'
#' # Using counts directly
#' counts <- c(A = 2, B = 1, C = 3)
#' simpson(counts, method = "hunter")
#'
#' # Hunter-Gaston = Gini-Simpson * N / (N - 1)
#' simpson(counts, method = "gini") * 6 / 5
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
#' Mueller, J. H., & Schuessler, K. F. (1961).
#' *Statistical Reasoning in Sociology*. Houghton Mifflin.
#'
#' Agresti, A., & Agresti, B. F. (1978).
#' Statistical analysis of qualitative variation.
#' *Sociological Methodology*, 9, 204-237.
#' https://doi.org/10.2307/270810
#'
#'
#' @family diversity.concentration
#' @concept concentration-index
#'
#'
#' @export
simpson <- function(x, method = c("gini", "hunter", "iqv"),
                    categories = NULL, na.rm = FALSE) {

  method <- match.arg(method)

  if (!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm))
    stop("'na.rm' must be a single non-missing logical value")

  if (!is.null(categories) && method != "iqv")
    warning("'categories' is only used with method = \"iqv\"; ignored")

  # a single row of a community matrix (e.g. vegan BCI[1, ]) is a count
  # vector; several rows would be merged silently by unlist()
  if (is.data.frame(x)) {
    if (nrow(x) != 1L)
      stop("a data frame must have exactly one row; ",
           "use apply(x, 1, simpson, ...) for several")
    # unlist() of mixed columns would give a character vector, which would
    # then be tabulated as observations instead of read as counts
    if (!all(vapply(x, is.numeric, logical(1L))))
      stop("a data frame must contain numeric counts only")
    x <- unlist(x)
  }

  # Handle NA uniformly for all input types
  if (na.rm) {
    x <- x[!is.na(x)]
  } else if (anyNA(x)) {
    return(NA_real_)
  }

  # Determine counts (preserve names)
  if (is.numeric(x)) {
    if (any(x < 0) || !all(is.finite(x)))
      stop("Counts must be finite and non-negative")
    # relative frequencies are fine for gini and iqv; hunter counts pairs,
    # and non-integer counts can push it above 1 (rep(0.5, 4) gives 1.5)
    if (method == "hunter" && any(x != floor(x)))
      stop("the Hunter-Gaston index requires integer counts")
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

  gini <- 1 - sum((tt / N)^2)

  switch(method,

    gini = unname(gini),

    hunter = {
      # N >= 2 is what the index needs: with a single observation there is
      # no pair to draw. A single non-empty category is not a degenerate case
      # at all - the answer is 0, the same value "gini" reports for it.
      if (N < 2) {
        warning("Hunter-Gaston index requires N >= 2; returning NA")
        return(NA_real_)
      }
      # = 1 - sum(n_i (n_i - 1)) / (N (N - 1)), without the products that
      # overflow for huge counts
      unname(gini * (N / (N - 1)))
    },

    iqv = {
      K <- .iqvCategories(tt, categories)
      if (K < 2) {
        warning("IQV requires at least 2 categories (K >= 2); returning NA")
        return(NA_real_)
      }
      unname(gini * K / (K - 1))
    }
  )
}



# == internal helper functions ==============================================

.iqvCategories <- function(tt, categories) {

  # number K of categories for the IQV: observed ones by default, otherwise
  # a given number or a given set of possible categories
  kObs <- sum(tt > 0)

  if (is.null(categories))
    return(kObs)

  if (is.numeric(categories) && length(categories) == 1L) {
    if (!is.finite(categories) || categories != floor(categories))
      stop("'categories' must be a whole number or a vector of categories")
    K <- categories

  } else {
    # unique() would keep NA as a category of its own
    if (anyNA(categories))
      stop("'categories' must not contain missing values")
    categories <- unique(as.character(categories))
    K <- length(categories)

    # observed categories must be among the possible ones; unnamed counts
    # cannot be checked
    obs <- names(tt)[tt > 0]
    if (!is.null(obs)) {
      miss <- setdiff(obs, categories)
      if (length(miss))
        stop("observed categories not in 'categories': ",
             paste(miss, collapse = ", "))
    }
  }

  if (K < kObs)
    stop("'categories' (", K, ") is smaller than the number of observed ",
         "categories (", kObs, ")")

  K
}
