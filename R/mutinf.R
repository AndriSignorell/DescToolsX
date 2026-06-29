
#' Mutual Information
#'
#' Computes the mutual information (MI) between two variables
#' from a contingency table.
#'
#' Mutual information quantifies the amount of information
#' obtained about one variable through observing the other.
#'
#' It is defined as:
#'
#' \deqn{
#' I(X;Y) =
#' H(X) + H(Y) - H(X,Y)
#' }
#'
#' where \eqn{H(X)} and \eqn{H(Y)} are marginal entropies
#' and \eqn{H(X,Y)} is the joint entropy.
#'
#' @param x A contingency table, matrix, or a vector that can
#'   be coerced into a contingency table.
#' @param y An optional second variable used together with
#'   \code{x} to create a contingency table via
#'   \code{table(x, y, ...)}.
#' @param base Logarithm base.
#'   Defaults to \code{2} (bits).
#' @param normalize Logical.
#'   If \code{TRUE}, returns normalized mutual information
#'   (NMI).
#' @param ... Additional arguments passed to \code{table()}.
#'
#' @return
#' A numeric scalar containing the mutual information.
#'
#' @details
#' Mutual information is always nonnegative:
#'
#' \deqn{
#' I(X;Y) \ge 0
#' }
#'
#' Larger values indicate stronger dependence.
#'
#' If \code{normalize = TRUE}, the returned value is:
#'
#' \deqn{
#' \frac{I(X;Y)}
#' {\sqrt{H(X)H(Y)}}
#' }
#'
#' which approximately scales the measure to \eqn{[0,1]}.
#'
#' @examples
#' tab <- matrix(
#'   c(10, 20,
#'     30, 40),
#'   nrow = 2
#' )
#'
#' mutInf(tab)
#'
#' mutInf(tab, normalize = TRUE)
#'
#' x <- sample(letters[1:3], 100, TRUE)
#' y <- sample(LETTERS[1:2], 100, TRUE)
#'
#' mutInf(x, y)
#'
#' @references
#' Cover TM, Thomas JA (2006).
#' Elements of Information Theory (2nd ed.).
#' Wiley.
#'



#' @family assoc.nominal  
#' @concept association-measure  
#' @concept nominal  
#' @concept information-theory
#'
#'
#' @export
mutInf <- function(x,
                   y = NULL,
                   base = 2,
                   normalize = FALSE,
                   ...) {
  
  if (!is.null(y))
    x <- table(x, y, ...)
  
  x <- as.matrix(x)
  
  mi <- entropy(rowSums(x), base = base) +
    entropy(colSums(x), base = base) -
    entropy(x, base = base)
  
  if (normalize) {
    
    hx <- entropy(rowSums(x), base = base)
    hy <- entropy(colSums(x), base = base)
    
    if (hx > 0 && hy > 0)
      mi <- mi / sqrt(hx * hy)
    else
      mi <- 0
    
  }
  
  return(mi)
}

