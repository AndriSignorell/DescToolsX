
#' The (Weighted) Interquartile Range
#'
#' Compute the interquartile range of `x`, with optional weights.
#'
#' The IQR is the difference of the 0.75 and the 0.25 quantile as computed
#' by [quantileX()]. Without weights the result is identical to
#' [stats::IQR()], except that a missing value yields `NA` instead of an
#' error when `na.rm = FALSE`.
#'
#' @param x numeric vector
#' @param weights optional numeric vector of non-negative sample weights, of
#'   the same length as `x`
#' @param na.rm logical; whether to remove missing values. If `FALSE` and
#'   `x` contains missing values, the result is `NA`.
#' @param type integer selecting the quantile algorithm, see [quantileX()].
#'   The default `NULL` picks the one that suits the branch taken: `7`
#'   without weights, as in [stats::IQR()]; `2` with weights, which reads
#'   them as *relative* weights and depends only on their ratios. Pass
#'   `type = 7` explicitly to read the weights as replication counts.
#'
#' @return numeric scalar containing the interquartile range
#'
#' @examples
#' x <- c(3.7, 3.3, 3.5, 2.8)
#' w <- c(5, 5, 4, 1) / 15
#'
#' iqrX(x)
#' iqrX(x, weights = w)
#' iqrX(x, weights = w * 15)                 # same: only the ratios count
#' iqrX(x, weights = w * 15, type = 7)       # replication counts
#'
#' iqrX(c(x, NA))                            # NA, no error
#' iqrX(c(x, NA), na.rm = TRUE)
#'
#' @seealso [medianX()], [quantileX()], [stats::IQR()], [stats::quantile()]
#'
#' @family dispersion
#' @concept dispersion
#' @export
iqrX <- function(x, weights = NULL, na.rm = FALSE, type = NULL) {

  # type = NULL means "the right default for the branch taken":
  #
  #   unweighted -> 7, matching IQR() and quantile()
  #   weighted   -> 2, which reads the weights as RELATIVE and therefore
  #                  depends only on their ratios (DescTools called this
  #                  algorithm type 5, see quantileX)
  #
  # A fixed default of 7 would read the weights as replication counts, and
  # weights normalized to sum to 1 then make every quantile collapse onto
  # max(x). medianX() uses the same rule.
  if (is.null(type))
    type <- if (is.null(weights)) 7 else 2

  # Both branches through quantileX(): the unweighted one is then exactly
  # IQR() (which is diff(quantile(..., names = FALSE))), but returns NA
  # instead of stopping on missing values. names = FALSE also keeps the
  # result a bare scalar - diff() would otherwise carry the name "75%".
  diff(quantileX(x, weights = weights, probs = c(0.25, 0.75),
                 na.rm = na.rm, names = FALSE, type = type))
}
