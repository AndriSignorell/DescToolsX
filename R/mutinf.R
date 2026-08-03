
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
#' @param x a contingency table, matrix, or vector that can be coerced into a
#'   contingency table
#' @param y an optional second variable used together with
#'   \code{x} to create a contingency table via
#'   \code{table(x, y, ...)}
#' @param base logarithm base.
#'   Defaults to \code{2} (bits).
#' @param normalize logical.
#'   If \code{TRUE}, returns normalized mutual information
#'   (NMI).
#' @param ... additional arguments passed to \code{table()}
#'
#' @return a numeric scalar containing the mutual information
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
#'
#' @seealso \code{\link{entropy}}, \code{\link{uncertCoef}}
#'
#' @family assoc.nominal
#' @concept association-measure
#' @concept nominal
#' @concept information-theory
#' @export
mutInf <- function(x,
                   y = NULL,
                   base = 2,
                   normalize = FALSE,
                   ...) {
  
  if (!is.null(y))
    x <- table(x, y, ...)
  
  x <- as.matrix(x)
  
  if (!is.numeric(x))
    stop("'x' must be a numeric table or matrix of counts")
  if (any(x < 0, na.rm = TRUE))
    stop("'x' must contain non-negative counts")
  if (anyNA(x))
    return(NA_real_)
  if (sum(x) == 0)
    return(NA_real_)
  
  # computed once: the two marginal entropies were evaluated twice when
  # normalize = TRUE, once for mi and once for the denominator
  hx <- entropy(rowSums(x), base = base)
  hy <- entropy(colSums(x), base = base)
  
  mi <- hx + hy - entropy(x, base = base)
  
  # Mutual information is non-negative; a difference of three entropies
  # can land marginally below zero in floating point, and a printed
  # -2.2e-16 invites the reader to look for a sign error that is not
  # there.
  mi <- max(mi, 0)
  
  if (normalize) {
    
    if (hx > 0 && hy > 0)
      mi <- mi / sqrt(hx * hy)
    else
      # one margin is degenerate, so the variables cannot share
      # information and the normalized value is 0 by definition
      mi <- 0
    
  }
  
  return(mi)
}
