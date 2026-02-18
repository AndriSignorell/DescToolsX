
#' Shannon Entropy
#' 
#' Computes Shannon entropy of two variables. The
#' entropy quantifies the expected value of the information contained in a
#' vector. The mutual information is a quantity that measures the mutual
#' dependence of the two random variables.  
#' 
#' The Shannon entropy equation provides a way to estimate the average minimum
#' number of bits needed to encode a string of symbols, based on the frequency
#' of the symbols.\cr It is given by the formula \eqn{H = - \sum(\pi log(\pi))}
#' where \eqn{\pi} is the probability of character number i showing up in a
#' stream of characters of the given "script".\cr The entropy is ranging from 0
#' to Inf. 
#' 
#' @aliases Entropy MutInf
#' @param x a vector or a matrix of numerical or categorical type. If only x is
#' supplied it will be interpreted as contingency table. 
#' @param y a vector with the same type and dimension as x. If y is not
#' \code{NULL} then the entropy of \code{table(x, y, ...)} will be calculated.
#' @param base base of the logarithm to be used, defaults to 2. 
#' @param \dots further arguments are passed to the function
#' \code{\link{table}}, allowing i.e. to set \code{useNA}.
#' @return a numeric value. %% ~Describe the value returned 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso package \pkg{entropy} which implements various estimators of
#' entropy 
#' @references Shannon, Claude E. (July/October 1948). A Mathematical Theory of
#' Communication, \emph{Bell System Technical Journal} 27 (3): 379-423.
#' 
#' Ihara, Shunsuke (1993) \emph{Information theory for continuous systems},
#' World Scientific. p. 2. ISBN 978-981-02-0985-8.
#' 
#' @family topic.association-measures
#' @concept association
#' @concept information-theory
#' 
#' @examples
#' examp <- c(1,3)
#' # todo: some more ********
#' 


entropy <- function(x, y = NULL, base = 2, ...) {
  
  # x is either a table or a vector if y is defined
  
  if(!is.null(y)) { x <- table(x, y, ...) }
  x <- as.matrix(x)
  
  ptab <- x / sum(x)
  H <- - sum( ifelse(ptab > 0, ptab * log(ptab, base=base), 0) )
  return(H)
  
}


