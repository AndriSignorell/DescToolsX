
#' Shannon Entropy
#'
#' Computes the Shannon entropy of a vector, contingency table,
#' or matrix.
#'
#' Entropy is defined as:
#'
#' \deqn{
#' H(X) = - \sum_i p_i \log_b(p_i)
#' }
#'
#' where \eqn{p_i} are empirical probabilities and
#' \eqn{b} is the logarithm base.
#'
#' @param x A vector, table, matrix, or array of counts.
#' @param y An optional second variable used together with
#'   \code{x} to create a contingency table via
#'   \code{table(x, y, ...)}.
#' @param base Logarithm base.
#'   Defaults to \code{2} (bits).
#' @param normalize Logical.
#'   If \code{TRUE}, entropy is normalized to the interval
#'   \eqn{[0,1]}.
#' @param ... Additional arguments passed to \code{table()}.
#'
#' @return
#' A numeric scalar containing the entropy.
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
#' @examples
#' x <- c("A", "A", "B", "B", "C")
#'
#' entropy(x)
#'
#' tab <- matrix(
#'   c(10, 20,
#'     30, 40),
#'   nrow = 2
#' )
#'
#' entropy(tab)
#'
#' entropy(tab, normalize = TRUE)
#'
#' @references
#' Shannon CE (1948). A Mathematical Theory of Communication.
#' Bell System Technical Journal, 27, 379-423.
#'


#' @export
entropy <- function(x,
                    y = NULL,
                    base = 2,
                    normalize = FALSE,
                    ...) {
  
  if (!is.null(y))
    x <- table(x, y, ...)
  
  p <- as.numeric(x)
  p <- p / sum(p)
  
  H <- -sum(
    ifelse(p > 0,
           p * log(p, base = base),
           0)
  )
  
  if (normalize) {
    
    k <- sum(p > 0)
    
    if (k > 1)
      H <- H / log(k, base = base)
    else
      H <- 0
    
  }
  
  return(H)
}

