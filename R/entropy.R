
#' Shannon Entropy
#'
#' Computes the Shannon entropy of a categorical vector, contingency table,
#' or matrix.
#'
#' Entropy is defined as:
#'
#' \deqn{
#' H(X) = - \sum_i p_i \log_b(p_i)
#' }{H(X) = -sum(p * log(p, base = b))}
#'
#' where \eqn{p_i} are empirical probabilities and
#' \eqn{b} is the logarithm base.
#'
#' @param x a table, matrix or array of counts, or a categorical vector
#'   (factor, character or logical), which is tabulated first
#' @param y an optional second variable used together with
#'   \code{x} to create a contingency table via
#'   \code{table(x, y, ...)}
#' @param base logarithm base; defaults to \code{2} (bits)
#' @param normalize logical.
#'   If \code{TRUE}, entropy is normalized to the interval
#'   \eqn{[0,1]}.
#' @param na.rm logical; if \code{TRUE}, missing counts are dropped. A
#'   categorical \code{x} is tabulated with \code{\link{table}}, which
#'   excludes \code{NA} by default in any case.
#' @param ... additional arguments passed to \code{table()}
#'
#' @return a numeric scalar containing the entropy
#'
#' @details
#' Common logarithm bases:
#'
#' \tabular{ll}{
#' base = 2 \tab entropy in bits \cr
#' base = exp(1) \tab entropy in nats \cr
#' base = 10 \tab entropy in bans
#' }
#'
#' Zero probabilities are ignored in the summation.
#'
#' \code{normalize = TRUE} divides by \eqn{\log_b k}, with \eqn{k} the
#' number of \emph{occupied} categories rather than the number of possible
#' ones. The maximum of 1 is therefore reached whenever the observed
#' categories are equally frequent, independently of how many empty levels
#' the input carries. With a single occupied category the normalized value
#' is 0.
#'
#' @examples
#' x <- c("A", "A", "B", "B", "C")
#'
#' entropy(x)
#'
#' tab <- matrix(c(10, 20,
#'                 30, 40), nrow = 2)
#'
#' entropy(tab)
#' entropy(tab, normalize = TRUE)
#'
#' # a fair coin carries exactly one bit
#' entropy(c(50, 50))
#'
#' @references
#' Shannon CE (1948). A Mathematical Theory of Communication.
#' Bell System Technical Journal, 27, 379-423.
#'
#' @family diversity.concentration
#' @concept information-theory
#' @export
entropy <- function(x,
                    y = NULL,
                    base = 2,
                    normalize = FALSE,
                    na.rm = FALSE,
                    ...) {

  if (!is.numeric(base) || length(base) != 1L || !is.finite(base) ||
      base <= 0 || base == 1)
    stop("'base' must be a single positive number other than 1")

  if (!is.null(y)) {
    x <- table(x, y, ...)

  } else if (!is.numeric(x) && !is.table(x)) {
    # A bare categorical vector is the textbook use of entropy, and the
    # first documented example passes one. as.numeric() turned it into
    # NAs with a coercion warning and the function returned NA.
    x <- table(x, ...)
  }

  p <- as.numeric(x)

  if (na.rm)
    p <- p[!is.na(p)]

  if (anyNA(p))
    return(NA_real_)

  if (any(p < 0))
    stop("'x' must contain non-negative counts")

  total <- sum(p)
  if (total == 0)
    return(NA_real_)

  p <- p / total

  # p[p > 0] rather than ifelse(): the latter still evaluated log(0) for
  # every empty cell before discarding the result
  pp <- p[p > 0]
  H <- -sum(pp * log(pp, base = base))

  if (normalize) {

    k <- length(pp)

    H <- if (k > 1) H / log(k, base = base) else 0

  }

  return(H)
}
